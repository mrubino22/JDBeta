//! @tags complexity
//! @citations AliasDFG10

//Generated by c2fsm -cut -nodiv -int 
model main {
var N,j,k,m,tmp,tmp_1,tmp_2,tmp_3,tmp_4;
//parameters N,m;
states stop,start,lbl_7_1,lbl_12_1,lbl_12_3,lbl_11_1,lbl_10_1,lbl_4_3;
transition t_28 :={
  from  := start;
  to    := stop;
  guard := (N <= 2);
  action:=;
};
transition t_36 :={
  from  := start;
  to    := lbl_7_1;
  guard := (3 <= N);
  action:= j' = 1, k' = 1, tmp' = ?;
};
transition t_31 :={
  from  := lbl_7_1;
  to    := lbl_10_1;
  guard := ( ( (j <= 2tmp+2) && (2tmp+1 <= j) ) && (1 <= j) );
  action:= tmp_3' = ?;
};
transition t_32 :={
  from  := lbl_7_1;
  to    := lbl_4_3;
  guard := ( (j <= 2tmp+2) && (2tmp+1 <= j) );
  action:= k' = k+1, tmp_4' = k;
};
transition t_19 :={
  from  := lbl_12_1;
  to    := lbl_12_3;
  guard := ( (j <= 2tmp_1+2) && (2tmp_1+1 <= j) );
  action:= j' = tmp_1;
};
transition t_29 :={
  from  := lbl_12_3;
  to    := lbl_7_1;
  guard := true;
  action:= tmp' = ?;
};
transition t_21 :={
  from  := lbl_11_1;
  to    := lbl_12_1;
  guard := ( (j <= 2tmp_2+2) && (2tmp_2+1 <= j) );
  action:= tmp_1' = ?;
};
transition t_23 :={
  from  := lbl_10_1;
  to    := lbl_11_1;
  guard := ( (j <= 2tmp_3+2) && (2tmp_3+1 <= j) );
  action:= tmp_2' = ?;
};
transition t_33 :={
  from  := lbl_4_3;
  to    := stop;
  guard := (N <= k);
  action:=;
};
transition t_34 :={
  from  := lbl_4_3;
  to    := lbl_7_1;
  guard := (k+1 <= N);
  action:= j' = k, tmp' = ?;
};
}
strategy dumb {
    Region init := { state = start };
}

