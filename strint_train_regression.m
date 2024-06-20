clear all; close all;
%% File import
% This will read in all files from the specified folder. NOTE: if an ".xdf"
% files exists in the folder, it will be included in the exported
% dataframe!
if strcmp(computer, 'MACI64')
    cd('cactus')
    files = dir('*train*');
else
    %cd('Z:\Data\Adaptation\structural_interference\manuscript\Post_Step_2_train')
    cd('C:\Users\Alex\Desktop\Post_Step_2_train') % Home PC
    files = dir('*train*');    
end

%% Regressions
% To quantify learning during the exposure to the random rotations in the
% training phase, we are adapting a technique from Bond & Taylor, 2017. We
% regress the outcome variable (IDE, RMSE, etc...) on the rotation amount
% during the first 40 and last 40 trials of exposure (exact number subject
% to change).
earlyTrials = 17:56;
lateTrials = 217:256;

for i = 1:length(files)
    % Load in subject data
    if strcmp(computer, 'MACI64')
        cd('cactus')
        load(files(i).name);
    else
        %cd('Z:\Data\Adaptation\structural_interference\manuscript\Post_Step_2_train')
        cd('C:\Users\Alex\Desktop\Post_Step_2_train') % Home PC
        load(files(i).name);
    end
  
    % re-calculate theta (not saved over from step 2)
    vbTrials = 1:16;
    exTrials = 17:256;
    peTrials = 257:272;
    numTrials = length(ide);
    rot = zeros(numTrials,1);
    for j = 1:numTrials
        rot(j) = sortData(j).rotation_amount(1); % Change to theta after this loop runs
    end
    % fix issue with index shift
    theta = rot;
    theta(peTrials) = 0;
    theta(exTrials) = theta(exTrials+1);
    theta(256) = theta(255);
    theta(end) = 0;

    % Perform linear model fits
    lm.ide = fitlm(theta(exTrials), ide(exTrials));
    lm.ide_early = fitlm(theta(earlyTrials), ide((earlyTrials)));
    lm.ide_late = fitlm(theta(lateTrials), ide((lateTrials)));
    
    % NOTE: For these measures, I removed the 'fb' prefix since we don't
    % have the feedback version of these. Nor do we use them at the
    % moment...
    lm.rmse = fitlm(theta(exTrials), rmse(exTrials));
    lm.rmse_early = fitlm(theta(earlyTrials), rmse((earlyTrials)));
    lm.rmse_late = fitlm(theta(lateTrials), rmse((lateTrials)));
    
    lm.MT = fitlm(theta(exTrials), MT(exTrials));
    lm.MT_early = fitlm(theta(earlyTrials), MT((earlyTrials)));
    lm.MT_late = fitlm(theta(lateTrials), MT((lateTrials)));
    
    lm.mov_int = fitlm(theta(exTrials), mov_int(exTrials));
    lm.mov_int_early = fitlm(theta(earlyTrials), mov_int((earlyTrials)));
    lm.mov_int_late = fitlm(theta(lateTrials), mov_int((lateTrials)));
    
    lm.norm_jerk = fitlm(theta(exTrials), norm_jerk(exTrials));
    lm.norm_jerk_early = fitlm(theta(earlyTrials), norm_jerk((earlyTrials)));
    lm.norm_jerk_late = fitlm(theta(lateTrials), norm_jerk((lateTrials)));
    
    % Create new dataframe with subject id number, Beta coefficients, and its
    % associated pValue.
    regData = [str2num(subID(12:13)) lm.ide.Coefficients.Estimate(2), lm.ide.Coefficients.pValue(2),...
        lm.rmse.Coefficients.Estimate(2), lm.rmse.Coefficients.pValue(2),...
        lm.MT.Coefficients.Estimate(2), lm.MT.Coefficients.pValue(2),...
        lm.mov_int.Coefficients.Estimate(2), lm.mov_int.Coefficients.pValue(2),...
        lm.norm_jerk.Coefficients.Estimate(2), lm.norm_jerk.Coefficients.pValue(2)];
    
    % Create a new dataframe with subject id number, time (early = 1; late =
    % 2), Beta coefficient, and its associate pValue.
    regDataEL = [str2num(subID(12:13)) 1 lm.ide_early.Coefficients.Estimate(2), lm.ide_early.Coefficients.pValue(2),...
        lm.rmse_early.Coefficients.Estimate(2), lm.rmse_early.Coefficients.pValue(2),...
        lm.MT_early.Coefficients.Estimate(2), lm.MT_early.Coefficients.pValue(2),...
        lm.mov_int_early.Coefficients.Estimate(2), lm.mov_int_early.Coefficients.pValue(2),...
        lm.norm_jerk_early.Coefficients.Estimate(2), lm.norm_jerk_early.Coefficients.pValue(2);...
        str2num(subID(12:13)) 2 lm.ide_late.Coefficients.Estimate(2) lm.ide_late.Coefficients.pValue(2),...
        lm.rmse_late.Coefficients.Estimate(2) lm.rmse_late.Coefficients.pValue(2),...
        lm.MT_late.Coefficients.Estimate(2) lm.MT_late.Coefficients.pValue(2),...
        lm.mov_int_late.Coefficients.Estimate(2) lm.mov_int_late.Coefficients.pValue(2),...
        lm.norm_jerk_late.Coefficients.Estimate(2) lm.norm_jerk_late.Coefficients.pValue(2)];
    %% Save data to file
    %asdkfjagaegaega % hard break so you don't override existing data.
    if strcmp(computer,'MACI64') == 1
        cd('cactus') % Mac
    else
        %cd('Z:\Data\Adaptation\structural_interference\manuscript') % PCs
        cd('C:\Users\Alex\Desktop\Post_Step_2_train') % Home PC
    end
    writematrix(regData,'regData.xls','WriteMode','append')
    writematrix(regDataEL,'regDataEL.xls','WriteMode','append')
end