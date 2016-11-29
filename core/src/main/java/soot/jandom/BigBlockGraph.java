/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Derived from work by the Sable Research Group and others 1997-2003.
 * See the 'credits' file distributed with Soot for the complete list of
 * contributors.  (Soot is distributed at http://www.sable.mcgill.ca/soot)
 */

package soot.jandom;

import java.util.*;

import soot.*;
import soot.util.Chain;
import soot.toolkits.graph.*;


/**
 *  <p>
 *  Represents the control flow graph of a {@link Body} at the basic
 *  block level. Each node of the graph is a {@link Block} while the
 *  edges represent the flow of control from one basic block to
 *  the next.</p>
 *
 *  <p> This is an abstract base class for different variants of
 *  {@link BigBlockGraph}, where the variants differ in how they
 *  analyze the control flow between individual units (represented
 *  by passing different variants of {@link UnitGraph} to the
 *  <code>BlockGraph</code> constructor) and in how they identify
 *  block leaders (represented by overriding <code>BlockGraph</code>'s
 *  definition of {@link computeLeaders()}.</p>
 *
 *  <p>The difference with {@link BlockGraph} is that it allows (and
 *  builds by default) bigger blocks, in that a jump instruction is
 *  not required to end a  {@link Block}.
 */

public abstract class BigBlockGraph extends BlockGraph {

    /**
     *  Create a <code>BigBlockGraph</code> representing at the basic block
     *  level the control flow specified, at the <code>Unit</code> level,
     *  by a given {@link UnitGraph}.
     *
     *   @param unitGraph  A representation of the control flow at
     *                     the level of individual {@link Unit}s.
     */
	 protected BigBlockGraph(UnitGraph unitGraph)
	 {
		 super(unitGraph);
     }

	/**
	 * <p>
	 * Utility method for computing the basic block leaders for a {@link Body},
	 * given its {@link UnitGraph} (i.e., the instructions which begin new basic
	 * blocks).
	 * </p>
	 *
	 * <p>
	 * This implementation designates as basic block leaders :
	 *
	 * <ul>
	 *
	 * <li>Any <code>Unit</code> which has zero predecessors, is the target of
	 * a jump (we should check the case when the target of a conditional jump is
	 * the fall-through node) or a tail of the unit graph (so that returns open
	 * a new block)
	 *
	 * <li>The first <code>Unit</code> in any <code>Trap</code> handler.
	 * (Strictly speaking, if <code>unitGraph</code> were a
	 * <code>ExceptionalUnitGraph</code> that included only a single
	 * unexceptional predecessor for some handler&mdash;because no trapped unit
	 * could possibly throw the exception that the handler catches, while the
	 * code preceding the handler fell through to the handler's code&mdash;then
	 * you could merge the handler into the predecessor's basic block; but such
	 * situations occur only in carefully contrived bytecode.)
	 *
	 * </ul>
	 * </p>
	 *
	 * @param unitGraph
	 *            is the <code>Unit</code>-level CFG which is to be split into
	 *            basic blocks.
	 *
	 * @return the {@link Set} of {@link Unit}s in <code>unitGraph</code> which
	 *         are block leaders.
	 */
	@Override
	protected Set<Unit> computeLeaders(UnitGraph unitGraph) {
		Body body = unitGraph.getBody();
		if (body != mBody) {
			throw new RuntimeException(
					"BlockGraph.computeLeaders() called with a UnitGraph that doesn't match its mBody.");
		}
		Set<Unit> leaders = new HashSet<Unit>();

		// Trap handlers start new basic blocks, no matter how many
		// predecessors they have.
		Chain<Trap> traps = body.getTraps();
		for (Iterator<Trap> trapIt = traps.iterator(); trapIt.hasNext();) {
			Trap trap = trapIt.next();
			leaders.add(trap.getHandlerUnit());
		}

		Chain<Unit> units = body.getUnits();
		for (Iterator<Unit> unitIt = units.iterator(); unitIt.hasNext();) {
			Unit u = unitIt.next();
			List<Unit> predecessors = unitGraph.getPredsOf(u);
			int predCount = predecessors.size();

			if (predCount != 1 || predecessors.get(0) != units.getPredOf(u) || unitGraph.getTails().contains(u)) {
				leaders.add(u);
			}
		}
		return leaders;
	}

	/**
	 * <p>
	 * A utility method that does most of the work of constructing basic blocks,
	 * once the set of block leaders has been determined, and which designates
	 * the heads and tails of the graph.
	 * </p>
	 *
	 * <p>
	 * <code>BlockGraph</code> provides an implementation of
	 * <code>buildBlocks()</code> which splits the {@link Unit}s in
	 * <code>unitGraph</code> so that each <code>Unit</code> in the passed set
	 * of block leaders is the first unit in a block. It defines as heads the
	 * blocks which begin with <code>Unit</code>s which are heads in
	 * <code>unitGraph</code>, and defines as tails the blocks which end with
	 * <code>Unit</code>s which are tails in <code>unitGraph</code>. Subclasses
	 * might override this behavior.
	 * </p>
	 *
	 * <p>
	 * Differently from the {@link BlockGraph} variant, we allow a jump not to
	 * end a BasicBlock, since we scan the entire block for jumps, not only its
	 * last {@link Unit}.
	 *
	 *
	 * @param leaders
	 *            Contains <code>Unit</code>s which are to be block leaders.
	 *
	 * @param unitGraph
	 *            Provides information about the predecessors and successors of
	 *            each <code>Unit</code> in the <code>Body</code>, for
	 *            determining the predecessors and successors of each created
	 *            {@link Block}.
	 *
	 * @return a {@link Map} from {@link Unit}s which begin or end a block to
	 *         the block which contains them.
	 */
	@Override
	protected Map<Unit, Block> buildBlocks(Set<Unit> leaders,
			UnitGraph unitGraph) {
		List<Block> blockList = new ArrayList<Block>(leaders.size());
		Map<Unit, Block> unitToBlock = new HashMap<Unit, Block>(); // Maps head
																	// and tail
																	// units to
		// their blocks, for building
		// predecessor and successor lists.
		Unit blockHead = null;
		int blockLength = 0;
		Iterator<Unit> unitIt = mUnits.iterator();
		if (unitIt.hasNext()) {
			blockHead = unitIt.next();
			if (!leaders.contains(blockHead)) {
				throw new RuntimeException(
						"BlockGraph: first unit not a leader!");
			}
			blockLength++;
		}
		Unit blockTail = blockHead;
		int indexInMethod = 0;

		while (unitIt.hasNext()) {
			Unit u = unitIt.next();
			if (leaders.contains(u)) {
				addBlock(blockHead, blockTail, indexInMethod, blockLength,
						blockList, unitToBlock);
				indexInMethod++;
				blockHead = u;
				blockLength = 0;
			}
			blockTail = u;
			blockLength++;
		}
		if (blockLength > 0) {
			// Add final block.
			addBlock(blockHead, blockTail, indexInMethod, blockLength,
					blockList, unitToBlock);
		}

		// The underlying UnitGraph defines heads and tails.
		for (Iterator<Unit> it = unitGraph.getHeads().iterator(); it.hasNext();) {
			Unit headUnit = it.next();
			Block headBlock = unitToBlock.get(headUnit);
			if (headBlock.getHead() == headUnit) {
				mHeads.add(headBlock);
			} else {
				throw new RuntimeException(
						"BlockGraph(): head Unit is not the first unit in the corresponding Block!");
			}
		}
		for (Iterator<Unit> it = unitGraph.getTails().iterator(); it.hasNext();) {
			Unit tailUnit = it.next();
			Block tailBlock = unitToBlock.get(tailUnit);
			if (tailBlock.getTail() == tailUnit) {
				mTails.add(tailBlock);
			} else {
				throw new RuntimeException(
						"BlockGraph(): tail Unit is not the last unit in the corresponding Block!");
			}
		}

		for (Iterator<Block> blockIt = blockList.iterator(); blockIt.hasNext();) {
			Block block = blockIt.next();

			List<Unit> predUnits = unitGraph.getPredsOf(block.getHead());
			List<Block> predBlocks = new ArrayList<Block>(predUnits.size());
			for (Iterator<Unit> predIt = predUnits.iterator(); predIt.hasNext();) {
				Unit predUnit = predIt.next();
				Block predBlock = unitToBlock.get(predUnit);
				if (predBlock == null) {
					throw new RuntimeException(
							"BlockGraph(): block head mapped to null block!");
				}
				predBlocks.add(predBlock);
			}

			if (predBlocks.size() == 0) {
				block.setPreds(Collections.<Block> emptyList());

				// If the UnreachableCodeEliminator is not eliminating
				// unreachable handlers, then they will have no
				// predecessors, yet not be heads.
				/*
				 * if (! mHeads.contains(block)) { throw new
				 * RuntimeException("Block with no predecessors is not a head!"
				 * );
				 *
				 * // Note that a block can be a head even if it has //
				 * predecessors: a handler that might catch an exception //
				 * thrown by the first Unit in the method. }
				 */
			} else {
				block.setPreds(Collections.unmodifiableList(predBlocks));
				if (block.getHead() == mUnits.getFirst()) {
					mHeads.add(block); // Make the first block a head
					// even if the Body is one huge loop.
				}
			}

			List<Block> succBlocks = new ArrayList<Block>();
			for (Iterator<Unit> unitBlockIt = block.iterator(); unitBlockIt.hasNext(); ) {
				Unit unit = unitBlockIt.next();
				if (unit.branches()) {
					List<Unit> succUnits = unitGraph.getSuccsOf(unit);
					Iterator<Unit> succIt = succUnits.iterator();
					if (unit.fallsThrough()) succIt.next();
					for (; succIt.hasNext();) {
						Unit succUnit = succIt.next();
						Block succBlock = unitToBlock.get(succUnit);
						if (succBlock == null) {
							throw new RuntimeException(
									"BlockGraph(): block jup mapped to null block!");
						}
						succBlocks.add(succBlock);
					}
				}
			}
			if (block.getTail().fallsThrough()) {
				Unit succUnit = unitGraph.getSuccsOf(block.getTail()).get(0);
				Block succBlock = unitToBlock.get(succUnit);
				if (succBlock == null) {
					throw new RuntimeException(
							"BlockGraph(): block tail mapped to null block!");
				}
				succBlocks.add(0, succBlock);
			}


			if (succBlocks.size() == 0) {
				block.setSuccs(Collections.<Block> emptyList());
				if (!mTails.contains(block)) {
					throw new RuntimeException(
							"Block with no successors is not a tail!: "
									+ block.toString());
					// Note that a block can be a tail even if it has
					// successors: a return that throws a caught exception.
				}
			} else {
				block.setSuccs(Collections.unmodifiableList(succBlocks));
			}

		}
		mBlocks = Collections.unmodifiableList(blockList);
		mHeads = Collections.unmodifiableList(mHeads);
		if (mTails.size() == 0) {
			mTails = Collections.emptyList();
		} else {
			mTails = Collections.unmodifiableList(mTails);
		}
		return unitToBlock;
	}

	/**
	 * A utility method which creates a new block and adds information about it
	 * to data structures used to build the graph.
	 *
	 * @param head
	 *            The first unit in the block.
	 * @param tail
	 *            The last unit in the block.
	 * @param index
	 *            The index of this block this {@link Body}.
	 * @param length
	 *            The number of units in this block.
	 * @param blockList
	 *            The list of blocks for this method. <code>addBlock()</code>
	 *            will add the newly created block to this list.
	 * @param unitToBlock
	 *            A map from units to blocks. <code>addBlock()</code> will add
	 *            mappings from all the units betweend <code>head</code> and
	 *            <code>tail</code> to the new block.
	 */
	private void addBlock(Unit head, Unit tail, int index, int length,
			List<Block> blockList, Map<Unit, Block> unitToBlock) {
		Block block = new Block(head, tail, mBody, index, length, this);
		blockList.add(block);
		for (Unit u = head; u != tail; u = mUnits.getSuccOf(u) )
			unitToBlock.put(u, block);
		unitToBlock.put(tail, block);
	}
}
