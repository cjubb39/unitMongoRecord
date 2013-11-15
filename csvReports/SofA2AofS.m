function out = SofA2AofS(in)
% converts input struct of arrays to array of structs

f = fieldnames(in); 
numFields = size(f);
length = size(in.SN);

for i=1:length
	temp = struct();

	for st = 1:numFields
		field = char(genvarname(f(st)));
		temp = setfield(temp, (field), in.(field)(i));
	end
	
	out(i) = temp;
end
