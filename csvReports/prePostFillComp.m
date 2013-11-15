function out = prePostFillComp(pre, post)
% compare pre and post oil filling data.
% Methodology to use for compare in 'comp' function
fields = fieldnames(pre(1));
numFields = size(fields);
numUnits = size(pre);
numUnits = numUnits(2);

for i=1:numUnits
	temp = struct();
	tempSN = pre(i).SN; %preserve SN.  Numeric -> will be set to 0 in comp

	for st = 1:numFields
		field = char(genvarname(fields(st)));
		data = comp(pre(i).(field), post(i).(field));
		temp = setfield(temp, (field), data);
	end

	% set group name
	temp.Group = strcat(post(i).Group, '-DIFF');
	temp.SN = tempSN;

	out(i) = temp;
end

	function ret = comp(preData, postData)
		if (isnumeric(preData))
			ret = postData-preData;
		else
			ret = preData;
		end
	end
end

