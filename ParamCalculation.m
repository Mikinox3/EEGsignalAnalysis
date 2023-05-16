clear all
close all
clc

%% Do the single trial analysis 
load MXTsimp AllTactSimple time
Sbj = 11; %change for condition
E=1; % change if required
PercentTrials = {};

for g = 1:2
    for c = 1:2
        for e = 1: E 
            for s =  1:Sbj
                Ntrials = size(AllTactSimple{g,s}(c,:,e,:),2);
                trial_sum = sum(squeeze(AllTactSimple{g,s}(c,:,e,:))>0,1)/Ntrials;
                PercentTrials{g,c,e,s} = trial_sum;
                
            end
        end
    end
end    
            

save PercentTrials PercentTrials

%% Single trials - peak analysis
clear all
 
load MXTsimp AllTactSimple time
Sbj = 11; %change for condition
E=1; % change if required

p1_x1 = find(time < 76 & time > 74);
p1_x2 = find(time > 114 & time < 116);
p1_real_x2 =find(time > 114 & time < 116);

kp = 1;
for g = 1:2
    for s =  1:Sbj
        for c = 1:2
            Ntrials = size(AllTactSimple{g,s}(c,:,1,:),2);
            for e = 1:E
                PE = double(mean(squeeze(AllTactSimple{g,s}(c,:,e,:)),1));
                [pks,locs] = findpeaks(PE(p1_x1:p1_real_x2));
                T = time(p1_x1:p1_real_x2);
                locs = T(locs);
                if isempty (locs)
                    p1_100(e) = nan;
                else
                    p1_100(e) = locs(1);
                end
                for t = 1:size(AllTactSimple{g,s}(c,:,e,:),2)
                    [pks,locs] = findpeaks(squeeze(double(AllTactSimple{g,s}(c,t,e,p1_x1:p1_x2))));
                    if isempty(pks)
                        [val, i] = max(squeeze(AllTactSimple{g,s}(c,t,e,p1_x1:p1_x2)));
                        single_P1_ampl(t,e) = val;
                        single_P1_lat(t,e) = time(p1_x1-1+i);
                        Essais_MesureParMax(t,e) = 1;
                    else
                        if numel(pks) == 1
                            single_P1_ampl(t,e) = pks;
                            single_P1_lat(t,e) =  time(p1_x1-1+locs);
                            Essais_MesureParMax(t,e) = 0;
                        else
                            [val, ij] = min(abs((p1_100(e)-p1_x1)-locs));
                            single_P1_ampl(t,e) = pks(ij);
                            single_P1_lat(t,e) =  time(p1_x1-1+locs(ij));
                            Essais_MesureParMax(t,e) = 2;
                        end
                     end
                     
                    
                end
                  
            end 
            TrialsWithMax (kp,c,:) = sum(Essais_MesureParMax == 1)/Ntrials*100;
            TrialsWithPEP100 (kp,c,:) = sum(Essais_MesureParMax == 2)/Ntrials*100;
            MAD_ampl (kp,c,:) = mad(single_P1_ampl,1);
            MAD_lat(kp,c,:)= mad (single_P1_lat,1);
            Avg_P1_all_Ampl(kp,c,:) = nanmean(single_P1_ampl);
            Avg_P1_all_Lat(kp,c,:) = nanmean(single_P1_lat);
            save(['Groupe' num2str(g) '_Subject' num2str(s),'_condition' num2str(c),'_P1'], 'single_P1_lat', 'single_P1_ampl')
        end  
        kp = kp + 1;
                     
        
    end
end

save P1measurements MAD* Avg_P1* TrialsWith* 

