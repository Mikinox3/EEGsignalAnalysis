clear all
close all
clc

%-----Parameters--------
%E = [23 26 27 29 60 63 64];

%-----end parameters-----
 %Reads the subject list
Sbj= {'xxx'; 'xxx';};

 Mxshort = NaN(length(Sbj),2,64,563); 
 Mxlong = NaN(length(Sbj),2,64,563);
 
   for y = 1:length(Sbj)
     %change for sessionB 
     Path2Data = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\'));
     
     
     files2pp = dir(fullfile(Path2Data,'*visuelsimpleshort*.mat'));
     load(fullfile(Path2Data , [files2pp.name]) );
     disp(Sbj(y))
     Mxshort(y,:,:,:) = squeeze(nanmean(MxSbjCisi(:,:,:,:),2));
     
    figure;
    subplot(1,2,1)
    plot(squeeze(nanmean(  MxVisuelSimpleshort(y,1,64,:),2))')
    subplot(1,2,2)
    plot(squeeze(nanmean(  MxVisuelSimpleshort(y,2,64,:),2))')
     
    
     files2pp = dir(fullfile(Path2Data,'*tactsimplelong*.mat'));
     load(fullfile(Path2Data , [files2pp.name]) );
     Mxlong(y,:,:,:) = squeeze(nanmean(MxSbjCisi(:,:,:,:),2));
     
    figure;
    subplot(1,2,1)
    plot(squeeze(nanmean(MxTactSimpleshort(y,1,14,:),2))')
    subplot(1,2,2)
    plot(squeeze(nanmean(MxTactSimpleshort(y,2,14,:),2))')
     
   end
   
  %figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(Mxshort(:,:,[14 17],:),1),2)))
  %figure; plot(-100:1000/512:999, squeeze(nanmean(nanmean(Mxlong(:,:,[14 17],:),1),2)))
  %figure; plot(-100:1000/512:999, squeeze(nanmean(Mxlong([1,2,3,4,5,6,7,8],:,14,:),2)))


  %save(['Y:\PILAe\EEG_data\', 'AllSbjTactSimpleShort_ERPdata.mat'], 'Mxshort');
 %save(['Y:\PILAe\EEG_data\', 'AllSbjTactSimpleLong_ERPdata.mat'], 'Mxlong');
  save(['xxxx\EEG_data\BigMxAB\SessionA\','AllSbjAudioSimpleShort_ERPdata.mat'], 'Mxshort');

 

 %figure;
 %plot(squeeze(nanmean(MxAudioSnslong(:,1,64,:),1)),'r')
 %plot(squeeze(mean(mean(MxSbjCisi(:,:,E,:),1),2)),'r')
 % in loadfromPRP_ERP : topoplot(squeeze(mean(EEG.data(:,200,:),3)), EEG.chanlocs, 'electrodes', 'numbers')
 %in loadfromPRP_ERP :figure; pop_timtopo(EEG, [-1 700.0469], [], 'ERP data and scalp maps','plotchans',[25,26,27,29,61,62,63]);
 
 
 
