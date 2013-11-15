function [avgDiff fillDiff]= calcAvgDiff(preXLS, postXLS, toSkip)
% Takes in XLS file with pre oil fill and post oil fill data
% Spits out struct containing average of differences in values of stats
%
% SN, fullSN, (and maybe group) are meaningless.
% Fluid field, however, is significanti
% 
% Takes array argument.  If SN in that array, we skip in avg calculation

% get struct of array of values
fluidpre = csv2struct(preXLS);
fluidpost = csv2struct(postXLS);

% convert to array of struct
fluidpreCorrected = SofA2AofS(fluidpre);
fluidpostCorrected = SofA2AofS(fluidpost);

% find differences between pre- and post- fills
fillDiff = prePostFillComp(fluidpreCorrected, fluidpostCorrected);

% compute average of pre/post diff
avgDiff = aggregateData(fillDiff, toSkip);
