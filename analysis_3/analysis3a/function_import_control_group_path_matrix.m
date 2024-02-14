function controlgrouppathmatrix = function_import_control_group_path_matrix(filename, dataLines)
%IMPORTFILE Import data from a text file
%  CONTROLGROUPPATHMATRIX = IMPORTFILE(FILENAME) reads data from text
%  file FILENAME for the default selection.  Returns the data as a table.
%
%  CONTROLGROUPPATHMATRIX = IMPORTFILE(FILE, DATALINES) reads data for
%  the specified row interval(s) of text file FILENAME. Specify
%  DATALINES as a positive scalar integer or a N-by-2 array of positive
%  scalar integers for dis-contiguous row intervals.
%
%  Example:
%  controlgrouppathmatrix = importfile("/home/matt/gits/bate-scan/analysis_3/analysis3a/control_group_path_matrix.csv", [2, Inf]);
%
%  See also READTABLE.
%
% Auto-generated by MATLAB on 14-Feb-2024 14:34:29

%% Input handling

% If dataLines is not specified, define defaults
if nargin < 2
    dataLines = [2, Inf];
end

%% Set up the Import Options and import the data
opts = delimitedTextImportOptions("NumVariables", 55);

% Specify range and delimiter
opts.DataLines = dataLines;
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["subj", "ability", "face", "fame", "angle", "trial", "rt", "acc", "sq", "iaseq", "s001", "s002", "s003", "s004", "s005", "s006", "s007", "s008", "s009", "s010", "s011", "s012", "s013", "s014", "s015", "s016", "s017", "s018", "s019", "s020", "s021", "s022", "s023", "s024", "s025", "s026", "s027", "s028", "s029", "s030", "s031", "s032", "s033", "s034", "s035", "s036", "s037", "s038", "s039", "s040", "s041", "s042", "s043", "s044", "s045"];
opts.VariableTypes = ["categorical", "categorical", "categorical", "categorical", "categorical", "double", "double", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "categorical"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["subj", "ability", "face", "fame", "angle", "acc", "sq", "iaseq", "s001", "s002", "s003", "s004", "s005", "s006", "s007", "s008", "s009", "s010", "s011", "s012", "s013", "s014", "s015", "s016", "s017", "s018", "s019", "s020", "s021", "s022", "s023", "s024", "s025", "s026", "s027", "s028", "s029", "s030", "s031", "s032", "s033", "s034", "s035", "s036", "s037", "s038", "s039", "s040", "s041", "s042", "s043", "s044", "s045"], "EmptyFieldRule", "auto", ...
    'TreatAsMissing', {'x', 'NA'}, 'FillValue', categorical(NaN));

% Import the data
controlgrouppathmatrix = readtable(filename, opts);
