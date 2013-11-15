function out = aggregateData(in, toSkip)
%Takes array of structs and averages each numeric field
%returns output

if nargin < 2
	toSkip = [];
end

fields = fieldnames(in);
numFields = size(fields);
numUnits = length(in);
numUnitsUsed = numUnits - length(toSkip);

% create avg struct
out = struct();

for i = 1:numFields
	field = char(genvarname(fields(i)));

	%get average if appropriate
	if(isnumeric(in(1).(field)))
		data = 0;
		for j = 1:numUnits
			if (~ismember(in(j).SN, toSkip))
				data = data + (in(j).(field));
			end
		end
		data = data / numUnitsUsed; % finish averaging
	else
		data = in.(field);
	end

	% set appropriate field appropriately
	out = setfield(out, (field), data);
end
end

