//! @tags complexity
//! @citations AliasDFG10

//Generated by c2fsm -cut -nodiv -int 
model main {
var i,j,n,tmp_1;
//parameters n;
states stop,start,cut;
transition t_43 :={
  from  := start;
  to    := stop;
  guard := (n <= 0);
  action:= i' = n, j' = 0;
};
transition t_68 :={
  from  := start;
  to    := cut;
  guard := (2 <= n);
  action:= i' = n-1, j' = 0;
};
transition t_69 :={
  from  := start;
  to    := stop;
  guard := (1 = n);
  action:= i' = n-1, j' = 0;
};
transition t_70 :={
  from  := start;
  to    := cut;
  guard := (2 <= n);
  action:= i' = n-1, j' = 1, tmp_1' = 0;
};
transition t_73 :={
  from  := start;
  to    := stop;
  guard := (1 = n);
  action:= i' = n-1, j' = 0, tmp_1' = 0;
};
transition t_58 :={
  from  := cut;
  to    := cut;
  guard := ( (2 <= i) && (2 <= j) );
  action:= i' = i-1, j' = j-1;
};
transition t_59 :={
  from  := cut;
  to    := stop;
  guard := ( (i <= 1) && (2 <= j) );
  action:= i' = i-1, j' = j-1;
};
transition t_60 :={
  from  := cut;
  to    := cut;
  guard := ( (2 <= i) && (j <= 1) );
  action:= i' = i-1, j' = 0;
};
transition t_61 :={
  from  := cut;
  to    := stop;
  guard := ( (i <= 1) && (j <= 1) );
  action:= i' = i-1, j' = 0;
};
transition t_62 :={
  from  := cut;
  to    := cut;
  guard := ( (2 <= i) && (j+2 <= n) );
  action:= i' = i-1, j' = j+1, tmp_1' = j;
};
transition t_63 :={
  from  := cut;
  to    := stop;
  guard := ( (i <= 1) && (j+2 <= n) );
  action:= i' = i-1, j' = j+1, tmp_1' = j;
};
transition t_64 :={
  from  := cut;
  to    := cut;
  guard := ( (2 <= i) && (n <= j+1) );
  action:= i' = i-1, j' = 0, tmp_1' = j;
};
transition t_65 :={
  from  := cut;
  to    := stop;
  guard := ( (i <= 1) && (n <= j+1) );
  action:= i' = i-1, j' = 0, tmp_1' = j;
};
}
strategy dumb {
    Region init := { state = start };
}
