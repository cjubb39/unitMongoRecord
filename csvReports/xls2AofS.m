function out = xls2AofS(in);
%shortcut for xls to array of structs

out = SofA2AofS(csv2struct(in)); 
