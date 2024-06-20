% This program save data to do MANOVA analysis
clear all
close all

% Select the subject file
str = computer;
if strcmp(str,'MACI64') == 1
    cd('/Volumes/mnl/Data/Adaptation/structural_interference/Post_Step_2_test_FC');
else
    cd('Z:\Data\Adaptation\structural_interference\Post_Step_2_test_FC\');
end

dir_list_fs = dir('*_lh_fc_FS.mat');    %Store subject *mat data file names in variable (struct array).
dir_list_cmd = dir('*_lh_fc_CMD.mat');

dir_list_fs = {dir_list_fs.name}; % filenames
dir_list_fs = sort(dir_list_fs);  % sorts files

dir_list_cmd = {dir_list_cmd.name}; % filenames
dir_list_cmd = sort(dir_list_cmd);  % sorts files

A2 = length(dir_list_fs);      % how many files to process?
hand='_lh';

for B = 1:1:A2
    load(char(dir_list_fs(B)));
    load(char(dir_list_cmd(B)));
    
    if strcmp(dir_list_fs{B}(8:10),'CON') == 1
        group = ones(48,1);
    elseif strcmp(dir_list_fs{B}(8:10),'STR') == 1
        group = ones(48,1)*2;
    else
        error('Group not assigned, check filename for correct naming convention')
    end
    
    FS=[subjectID upBool_export wrong_trial_export group forceFS]; %You store the current matrix
    CMD = [subjectID upBool_export wrong_trial_export group forceCMD];
    
    if strcmp(str,'MACI64') == 1
        cd('/Volumes/mnl/Data/Adaptation/structural_interference/Post_Step_3_test_FC');
        dlmwrite('lh_raw_fc_fs.csv', FS, '-append', 'delimiter', ',', 'precision','%.6f');
        dlmwrite('lh_raw_fc_cmd.csv', CMD, '-append', 'delimiter', ',', 'precision','%.6f');
        cd('/Volumes/mnl/Data/Adaptation/structural_interference/Post_Step_2_test_FC');
    else
        cd('Z:\Data\Adaptation\structural_interference\Post_Step_3_test_FC');
        dlmwrite('lh_raw_fc_fs.csv', FS, '-append', 'delimiter', ',', 'precision','%.6f');
        dlmwrite('lh_raw_fc_cmd.csv', CMD, '-append', 'delimiter', ',', 'precision','%.6f');
        cd('Z:\Data\Adaptation\structural_interference\Post_Step_2_test_FC');
    end
    % the notation '%.6f' writes each variable out to six decimal places, should get rid of engineering notation
end