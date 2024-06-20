% Function to calculate RMSE from the index of peak velocity to the
% endpoint of movement. This is in response to the March 2022 reviewer
% comments (initial submission)

function [fbrmse, fbrmse_c, fbrmse_down_st, fbrmse_up_st] = feedbackrmse_strint(plotBool, cursorPosX, cursorPosY, indPeak, offset, numTrials, wrong_trial, channel_trial, upTrials, downTrials, kbTrials, exTrials, peTrials, subID)

%% RMSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RMSE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Taken straight from kinsym2 step 2 files
rmse=zeros(numTrials,1); % allocate space for rmse
mov_int = zeros(numTrials,1);
for i=1:numTrials
    if (wrong_trial(i)==0 && channel_trial(i)==0)
        xx=cursorPosX{i,1}(indPeak(i):offset(i))*1000; % convert to mm
        yy=cursorPosY{i,1}(indPeak(i):offset(i))*1000;
        % spatial resampling of movement path
        N= 2000; N1= length(xx); % Computes equally-spaced vector assuming 1000 samples
        xc= 1/(N-1)*(0:N-1)*(xx(N1)-xx(1))+xx(1);
        yc= 1/(N-1)*(0:N-1)*(yy(N1)-yy(1))+yy(1);
        % integrates the movement length
        mov_int(i)=sum(sqrt(diff(xx).^2+ diff(yy).^2));
        di=(0:N-1)*mov_int(i)/(N-1);
        d=[0; (cumsum(sqrt((diff(xx).^2)+ (diff(yy).^2))))];
        % interpolates the movement path to make it equally spaced
        x2i= interp1q(d,xx,di');
        y2i= interp1q(d,yy,di');
        x2i(N)=xc(N);
        y2i(N)=yc(N);
        optimal =[xc', yc'];
        resampled_path =[x2i, y2i];
        rmse(i) = sqrt(sum(sum((resampled_path - optimal).^2))/N);
    else rmse(i)=NaN;
    end
end

%% rmse outlier analysis
data = outlier_t(rmse(upTrials(1:10))); % Outlier for visual baseline
rmse_c(upTrials(1:10)) = data;
data = outlier_t(rmse(downTrials(1:10)));
rmse_c(downTrials(1:10)) = data;
clear data;

data = outlier_t(rmse(upTrials(11:20))); % Outlier for kin baseline
rmse_c(upTrials(11:20)) = data;
data = outlier_t(rmse(downTrials(11:20)));
rmse_c(downTrials(11:20)) = data;
clear data;

rmse_c(upTrials(21)) = NaN; rmse_c(downTrials(21)) = NaN; % Toss out catch trials

% data = outlier_t(rmse(upTrials(14:23))); % Outlier for first 20 exposure
% rmse_c(upTrials(14:23)) = data;
% data = outlier_t(rmse(downTrials(14:23)));
% rmse_c(downTrials(14:23)) = data;
% clear data;
rmse_c(upTrials(22:31)) = rmse(upTrials(22:31));
rmse_c(downTrials(22:31)) = rmse(downTrials(22:31));

data = outlier_t(rmse(upTrials(32:91))); % Outlier for last 100 exposure
rmse_c(upTrials(32:91)) = data;
data = outlier_t(rmse(downTrials(32:91)));
rmse_c(downTrials(32:91)) = data;
clear data;

data = outlier_t(rmse(upTrials(92:96))); % Outlier for first 10 post-exp
rmse_c(upTrials(92:96)) = data;
data = outlier_t(rmse(downTrials(92:96)));
rmse_c(downTrials(92:96)) = data;
clear data;

data = outlier_t(rmse(upTrials(97:111))); % Outlier for last 10 post-exp
rmse_c(upTrials(97:111)) = data;
data = outlier_t(rmse(downTrials(97:111)));
rmse_c(downTrials(97:111)) = data;
clear data;

% transpose and calculate standardized variable
rmse_c = rmse_c';
bvup_mean = nanmean(rmse_c(upTrials(1:10)));
bvup_std = nanstd(rmse_c(upTrials(1:10)));
rmse_up_st = (rmse_c(upTrials) - bvup_mean)/bvup_std;

bvdown_mean = nanmean(rmse_c(downTrials(1:10)));
bvdown_std = nanstd(rmse_c(downTrials(1:10)));
rmse_down_st = (rmse_c(downTrials) - bvdown_mean)/bvdown_std;

clear bvup_mean; clear bvup_std; clear bvdown_mean; clear bvdown_std;

%% Plotting Code for rmse
if plotBool == 1
    figure
    set(gcf,'Color','w','Position',[560 528 600 420])
    hold on;
    
    subplot('Position',[0.06 0.2 0.1 0.6]); hold on;
    plot(upTrials(1:10),rmse(upTrials(1:10)),'bo');
    hold on
    plot(upTrials(1:10),rmse_c(upTrials(1:10)),'bx');
    hold on
    plot(downTrials(1:10),rmse(downTrials(1:10)),'ro');
    hold on
    plot(downTrials(1:10),rmse_c(downTrials(1:10)),'rx');
    axis([0 20 0 60]); set(gca,'LineWidth',2,'XTick',[1 10 20],'YTick',0:10:60,'YTickLabel',0:10:60,'FontName','Arial','FontSize',10); ylabel('rmse [mm]'); title('vis-pre','fontsize',11);
    
    hold on
    subplot('Position',[0.19 0.2 0.1 0.6]); hold on;
    plot(upTrials(11:20)-kbTrials(1)+1,rmse(upTrials(11:20)),'bo');
    hold on
    plot(upTrials(11:20)-kbTrials(1)+1,rmse_c(upTrials(11:20)),'bx');
    hold on
    plot(downTrials(11:20)-kbTrials(1)+1,rmse(downTrials(11:20)),'ro');
    hold on
    plot(downTrials(11:20)-kbTrials(1)+1,rmse_c(downTrials(11:20)),'rx');
    axis([0 20 0 60]); set(gca,'LineWidth',2,'XTick',[1 10 20],'YTick',[],'YTickLabel',[],'FontName','Arial','FontSize',10); title('kin-pre','fontsize',11);
    
    hold on
    subplot('Position',[0.32 0.2 0.4 0.6]); hold on;
    plot(upTrials(22:91)-exTrials(1)+1,rmse(upTrials(22:91)),'bo');
    hold on
    plot(upTrials(22:91)-exTrials(1)+1,rmse_c(upTrials(22:91)),'bx');
    hold on
    plot(downTrials(22:91)-exTrials(1)+1,rmse(downTrials(22:91)),'ro');
    hold on
    plot(downTrials(22:91)-exTrials(1)+1,rmse_c(downTrials(22:91)),'rx');
    axis([0 140 0 60]); set(gca,'LineWidth',2,'XTick',[1 20 70 120 140],'YTick',[],'YTickLabel',[],'FontName','Arial','FontSize',10); title(['exposure'], 'fontsize',11); xlabel('Trials','fontsize',11);
    
    hold on
    subplot('Position',[0.75 0.2 0.24 0.6]); hold on;
    plot(upTrials(92:111)-peTrials(1)+1,rmse(upTrials(92:111)),'bo');
    hold on
    plot(upTrials(92:111)-peTrials(1)+1,rmse_c(upTrials(92:111)),'bx');
    hold on
    plot(downTrials(92:111)-peTrials(1)+1,rmse(downTrials(92:111)),'ro');
    hold on
    plot(downTrials(92:111)-peTrials(1)+1,rmse_c(downTrials(92:111)),'rx');
    axis([0 40 0 60]); set(gca,'LineWidth',2,'XTick',[1 20 40],'YTick',[],'YTickLabel',[],'FontName','Arial','FontSize',10); title(['post-exposure'], 'fontsize',11); xlabel('Trials','fontsize',11);
    title('feedback rmse')
end
%% Export vars
fbrmse = rmse;
fbrmse_c = rmse_c;
fbrmse_down_st = rmse_down_st;
fbrmse_up_st = rmse_up_st;