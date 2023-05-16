

Path2EEGLAB = fullfile('xxxx\EEG_data\eeglab14_1_2b');
%%
cd(Path2EEGLAB)
eeglab
close all
cd('xxxx\MATLABcode\APICE\')

Sbj= {'xxxxx' ;'xxxxx'};
bigData = nan(8,2,60,64,563);

for y = 1:length(Sbj)
     Path2Data = char(strcat('xxxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\')); 
     files2pp = dir(fullfile(Path2Data,'*visuelsimpleshort*.mat'));
     load(fullfile(Path2Data , [files2pp.name]) );
     nt = size(MxSbjCisi,2);
     bigData(y,:,1:nt,:,:) = MxSbjCisi;
     
    figure;
    subplot(1,2,1)
    plot(squeeze(nanmean(nanmean(bigData(y,1,:,64,:),2),3))')
    subplot(1,2,2)
    plot(squeeze(nanmean(nanmean(bigData(y,2,:,64,:),2),3))')
    
end


figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,[27 64],:),1),2),3)))
figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(bigData([1,2,3,4],:,:,27,:),2),3)))
 %figure; plot(squeeze(nanmean(nanmean(bigData([1,2,3,4,5,6,7,8],:,:,64,:),2),3))')
%figure; plot(squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,[38 47 48 8 9 6 14 43 44 41 51],:),1),2),3))')
%figure; plot(squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,27,:),1),2),3))')

% figure; 
% topoplot(squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,:,75),1),2),3)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
% 
% figure; 
% topoplot(squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,:,85),1),2),3)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
% 
% figure; 
% topoplot(squeeze(nanmean(nanmean(nanmean(bigData(:,:,:,:,140),1),2),3)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
% 
% %Load from bigMx
Path2Mx = 'xxxx\EEG_data\zeBigMx\T1sessionA\';
files2plot = dir(fullfile(Path2Mx,'*VisuelSimpleShort*.mat'));
load(fullfile(Path2Mx , [files2plot.name]) );
bigData = Mxshort;

figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(bigData(:,2,[64],:),1),2)))
figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(bigData(:,1,[64],:),1),2)))
%figure; plot(squeeze(nanmean(nanmean(bigData(:,:,[7 8 15 51 52 ],:),1),2))')

% 
% figure; 
% topoplot(squeeze(nanmean(nanmean(bigData(:,:,:,100),1),2)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
% 
% figure; 
% topoplot(squeeze(nanmean(nanmean(bigData(:,:,:,150),1),2)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
% 
% figure; 
% topoplot(squeeze(nanmean(nanmean(bigData(:,:,:,200),1),2)), 'Resau_ChanLocs_64.sfp', 'electrodes', 'labels')
