%% hello world
cd  '/home/matt/gits/sarah/analysis_4';

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

%% Set global params
iaseq_col = 10;
first_comparison = 11;


%% Do ScanMatch on Control
% Import the data

analysis_4_control = control_group_import_paths('control_group_sequence_matrix.csv');
dat = analysis_4_control;

f = waitbar(0,'1','Name','Control Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
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
writetable(dat2,  'control_group_scanmatch_values_matrix.csv')
disp('done');
delete(f);

%% Do ScanMatch on devprop
% Import the data

analysis_4_devprop = devprop_group_import_paths('devprop_group_sequence_matrix.csv');
dat = analysis_4_devprop;

f = waitbar(0,'1','Name','Devprop Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
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
writetable(dat2,  'devprop_group_scanmatch_values_matrix.csv')
disp('done');
delete(f);



%% Do ScanMatch on super
% Import the data

analysis_4_super = super_group_import_paths('super_group_sequence_matrix.csv');
dat = analysis_4_super;

f = waitbar(0,'1','Name','Super Rows processed...', 'CreateCancelBtn','setappdata(gcbf,''canceling'',1)');
setappdata(f,'canceling',0);
nrows = size(dat,1);
ncols = size(dat,2);
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
writetable(dat2,  'super_group_scanmatch_values_matrix.csv')
disp('done');
delete(f);