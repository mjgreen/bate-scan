cd '/home/matt/gits/bate-scan/analysis_3/analysis3a/';
addpath '/home/matt/gits/bate-scan/ScanMatch/';

control_group_path_matrix = function_import_control_group_path_matrix("control_group_path_matrix");
devprop_group_path_matrix = function_import_devprop_group_path_matrix("devprop_group_path_matrix");
super_group_path_matrix = function_import_super_group_path_matrix("super_group_path_matrix");

%% Set up scanmatch substitution matrix
info.Xres = 1920;
info.Yres = 1080;
info.Xbin = 3.0;
info.Ybin = 6.0;
info.RoiModulus = info.Xbin;
info.Threshold = 1.0;
info.GapValue = -0.5;
info.TempBin = 50;
% Compute substitution matrix
info.SubMatrix = ScanMatch_CreateSubMatrix(info.Xbin, info.Ybin, info.Threshold);
info.mask = ones(info.Xres, info.Yres);
ScanMatchInfo = ScanMatch_Struct(info);
pause(1);
saveas(gcf, "mask.png");
close all;

%% Control Group Scanmatch Values
dat = control_group_path_matrix;
f = waitbar(0,'1','Name','Control Group Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
iaseq_col = 10;
first_comparison = 11;
disp('go')
dat2 = dat;
for row = 1:nrows
    if mod(row, 30) == 1
        % Update waitbar and message
        waitbar(row/nrows,f,[newline, sprintf('%.2f', (100*(row/nrows))), '%'])
    end
    % Check for clicked Cancel button
    if getappdata(f,'canceling')
        delete(f)
        break
    end
    seq1 = char(table2array(dat(row, iaseq_col)));
    for col = first_comparison:ncols
        seq2 = char(table2array(dat(row, col)));
        try 
            [score, align] = ScanMatch(seq1, seq2, ScanMatchInfo);
        catch
            score = NaN;
        end
        dat2(row, col) = {categorical(score)};
    end
end
dat2(:,"iaseq")=[];
writetable(dat2, "control_group_scanmatch_values_matrix.csv")
disp('done');
delete(f);

%% Devprop Group Scanmatch Values
dat = devprop_group_path_matrix;
f = waitbar(0,'1','Name','Devprop Group Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
iaseq_col = 10;
first_comparison = 11;
disp('go')
dat2 = dat;
for row = 1:nrows
    if mod(row, 30) == 1
        % Update waitbar and message
        waitbar(row/nrows,f,[newline, sprintf('%.2f', (100*(row/nrows))), '%'])
    end
    % Check for clicked Cancel button
    if getappdata(f,'canceling')
        delete(f)
        break
    end
    seq1 = char(table2array(dat(row, iaseq_col)));
    for col = first_comparison:ncols
        seq2 = char(table2array(dat(row, col)));
        try 
            [score, align] = ScanMatch(seq1, seq2, ScanMatchInfo);
        catch
            score = NaN;
        end
        dat2(row, col) = {categorical(score)};
    end
end
dat2(:,"iaseq")=[];
writetable(dat2, "devprop_group_scanmatch_values_matrix.csv")
disp('done');
delete(f);

%% Super Group Scanmatch Values
dat = super_group_path_matrix;
f = waitbar(0,'1','Name','Super Group Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
iaseq_col = 10;
first_comparison = 11;
disp('go')
dat2 = dat;
for row = 1:nrows
    if mod(row, 30) == 1
        % Update waitbar and message
        waitbar(row/nrows,f,[newline, sprintf('%.2f', (100*(row/nrows))), '%'])
    end
    % Check for clicked Cancel button
    if getappdata(f,'canceling')
        delete(f)
        break
    end
    seq1 = char(table2array(dat(row, iaseq_col)));
    for col = first_comparison:ncols
        seq2 = char(table2array(dat(row, col)));
        try 
            [score, align] = ScanMatch(seq1, seq2, ScanMatchInfo);
        catch
            score = NaN;
        end
        dat2(row, col) = {categorical(score)};
    end
end
dat2(:,"iaseq")=[];
writetable(dat2, "super_group_scanmatch_values_matrix.csv")
disp('done');
delete(f);
