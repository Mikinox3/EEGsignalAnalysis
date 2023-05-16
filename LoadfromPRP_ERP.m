clear all
close all
clc


Path2EEGLAB = fullfile('XXXX\EEG_data\eeglab14_1_2b');
Path2APICE = fullfile('xxxxx\EEG_data\APICE');
Path0 = 'xxxx\EEG_data\APICE\examples\example_original';
Path2Parameters = fullfile(Path0,'parameters');
filechanloc = fullfile(Path2EEGLAB,'plugins','dipfit2.3','standard_BESA','standard-10-5-cap385.elp'); 

cd(Path2EEGLAB)
eeglab
close all
cd(Path0)
addpath(genpath(Path2APICE))
addpath(Path2Parameters)

%------------- PARAMETERS------------------

% extension reader
Ppp.Files2Read = '*.set';
doPlots = 0;


% ---- Choose the type of file you want to load ----
loadfile = 'PRP';
%loadfile = 'ERP';

%------------- END PARAMETERS------------------

   %Reads the subject list
 Sbj= importdata('xxxx\EEG_data\subjectsList.txt');
 
 %Sbj = 1;
 %y = length(Sbj);
  
 %creates big matrixes with data for all subjects per stim block
 MxVisuelSimpleshort = NaN(length(Sbj),2,64,563); 
 MxVisuelSimplelong = NaN(length(Sbj),2,64,563);
 MxVisuelSnsshort = NaN(length(Sbj),2,64,563); 
 MxVisuelSnslong = NaN(length(Sbj),2,64,563);  
 MxAudioSimpleshort = NaN(length(Sbj),2,64,563); 
 MxAudioSimplelong = NaN(length(Sbj),2,64,563);
 MxAudioSnsshort = NaN(length(Sbj),2,64,563); 
 MxAudioSnslong = NaN(length(Sbj),2,64,563); 
 MxTactSimpleshort = NaN(length(Sbj),2,64,563); 
 MxTactSimplelong = NaN(length(Sbj),2,64,563); 
 

 
 if strcmp('PRP',loadfile)
     
     for y = 1:length(Sbj)
    % change for T1sessionB if needed or create a loop if lazy ;)    
    PathPRPdata = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\PRP\')); 
    %PathPRPdata = 'C:\Users\21600367t\Downloads\';
    PathERPdata = char(strcat('xxxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\ERP\')); 
    Path2Data = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\')); 
    % Read the PRP files.set for a given subject
    files2pp = dir(fullfile(PathPRPdata,Ppp.Files2Read));
 
     for condition=1:length(files2pp)
         FilesnameIn = files2pp(condition).name;
         FilesnameIn = FilesnameIn(1:end);
         
         IstimSize={'short','long'};
         n=cellstr(IstimSize);
         % set the event triggers according to the stim block analysed
         [ev, evl]=SetEpochEvents(FilesnameIn);
%          ev = {'condition 15','condition 25'};
%          evl = {'condition 16','condition 26'};
         eventIsi = [ev,evl];
         
         for i=1:length(IstimSize)
             
             
             EEG = pop_loadset(fullfile(PathPRPdata, [FilesnameIn,'_PRP.set']) );
             EEG = ERPdata(EEG,doPlots,IstimSize(i),ev,evl);
             
             if isempty(EEG.data)
                 disp('ERROR : No epoch retained');
             end
             
             if ~isempty(EEG.data)
                 % Save the epoched pre-process data with ERP
                 save([PathERPdata,FilesnameIn,n{i},'_ERP.set'], '-struct', 'EEG');
             end
                         %200ms is for ERPV
%         topoplot(squeeze(mean(EEG.data(:,200,:),3)), EEG.chanlocs, 'electrodes', 'numbers')
%         figure;  
%         plot(squeeze(mean(EEG.data(64,:,:),3)),'r')
%         figure; pop_timtopo(EEG, [-1 500], [], 'ERP data and scalp maps','plotchans',[25,26,27,29,61,62,63]);

             %Matrix splitting event type (social/non social OR low/high freq) ERP for a given stim block
             MxSbjCisi = NaN([2,round(EEG.trials/2),EEG.nbchan,EEG.pnts]);
             
             
             
             %Mx with eventtype correspondance for each epoch
             mcell=struct2cell(EEG.epoch);
             mep=squeeze(cell2mat(mcell(1,1,:)));
             %mevtype=squeeze(cell2mat(mcell(2,1,:)));
             mevtype=permute(cell2mat(mcell(2,1,:)), [3,2,1]);
             Mxev = [mep,mevtype];
             
             % m = trial number
             g=1;
             h=1;
             for m = 1:length(mep)
                 if Mxev(m,2)== str2double(eventIsi(2*i))
                     MxSbjCisi(2,g,:,:) = EEG.data(:,:,m);
                     g=g+1;
                     
                 else
                     MxSbjCisi(1,h,:,:) = EEG.data(:,:,m);
                     h=h+1;
                 end
             end
             
             save([Path2Data, FilesnameIn,n{i},'_ERPdata.mat'], 'MxSbjCisi');
           if strcmp(n{i},'short')
                if  strcmp(ev(1),'15')
                MxVisuelSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxVisuelSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                elseif strcmp(ev(1),'13')
                MxVisuelSnsshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxVisuelSnsshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                elseif strcmp(ev(1),'1')
                MxTactSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxTactSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                elseif strcmp(ev(1),'31')
                MxAudioSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxAudioSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                else
                MxAudioSnsshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxAudioSnsshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                end
            else
                 if strcmp(evl(1),'16')
                MxVisuelSimplelong(y,1,:,:) =squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxVisuelSimplelong(y,2,:,:) =squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
                elseif strcmp(evl(1),'14')
                MxVisuelSnslong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxVisuelSnslong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
                elseif strcmp(evl(1),'32')
                MxAudioSimplelong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxAudioSimplelong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                elseif strcmp(ev(1),'17')
                MxTactSimplelong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxTactSimplelong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                else
                MxAudioSnslong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2)); 
                MxAudioSnslong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2)); 
                end
           
            end
             
             
         end
         
     end
    
    end
     
     
 else
     
      
   for y = 1:length(Sbj)
     %change for T1sessionB if needed    
    PathERPdata = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\ERP\')); 
    Path2Data = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\')); 
    % Read the ERP files.set for a given subject
    files2pp = dir(fullfile(PathERPdata,Ppp.Files2Read));
    
    for condition=1:length(files2pp)
        FilesnameIn = files2pp(condition).name;
        FilesnameIn = FilesnameIn(1:end-8);
                                                                                                                                                                                                                                                                                                                                                                                                               %script originally written by MSR_2023
        EEG = pop_loadset(fullfile(PathERPdata, [FilesnameIn,'_ERP.set']) );
        %figure;
            %200ms is for ERPV
%         topoplot(squeeze(mean(EEG.data(:,200,:),3)), EEG.chanlocs, 'electrodes', 'numbers')
%         figure;  
%         plot(squeeze(mean(EEG.data(64,:,:),3)),'r')
%         figure; pop_timtopo(EEG, [-1 500], [], 'ERP data and scalp maps','plotchans',[25,26,27,29,61,62,63]);

        IstimSize={'short','long'};
        n=cellstr(IstimSize);
        % set the event triggers according to the stim block analysed
        [ev, evl]=SetEpochEvents(FilesnameIn);
        eventIsi = [ev,evl];
        
        if FilesnameIn(end)== 't'
            i=1;
        else
            i=2;
        end
            
        %Matrix splitting event type (social/non social OR low/high freq) ERP for a given stim block
        MxSbjCisi = NaN([2,round(EEG.trials/2),EEG.nbchan,EEG.pnts]);
        
        %Mx with eventtype correspondance for each epoch
        mcell=struct2cell(EEG.epoch);
        mep=squeeze(cell2mat(mcell(1,1,:)));
        
%         mevtype=permute(cell2mat(mcell(2,1,:)), [3,2,1]);
%             Mxev = [mep,mevtype];
        mevtype=squeeze(cell2mat(mcell(2,1,:)));
        Mxev = [mep,mevtype];
        
        g=1;
        h=1;
        for m = 1:length(mep)
            if Mxev(m,2)== str2double(eventIsi(2*i))
                MxSbjCisi(2,g,:,:) = EEG.data(:,:,m);
                g=g+1;
                
            else
                MxSbjCisi(1,h,:,:) = EEG.data(:,:,m);
                h=h+1;
            end
        end
        
        save([Path2Data, FilesnameIn,'_ERPdata.mat'], 'MxSbjCisi');
        
        if strcmp(n{i},'short')
            if  strcmp(ev(1),'15')
                MxVisuelSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxVisuelSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(ev(1),'13')
                MxVisuelSnsshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxVisuelSnsshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(ev(1),'1')
                MxTactSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxTactSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(ev(1),'31')
                MxAudioSimpleshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxAudioSimpleshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            else
                MxAudioSnsshort(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxAudioSnsshort(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            end
        else
            if strcmp(evl(1),'16')
                MxVisuelSimplelong(y,1,:,:) =squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxVisuelSimplelong(y,2,:,:) =squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(evl(1),'14')
                MxVisuelSnslong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxVisuelSnslong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(evl(1),'32')
                MxAudioSimplelong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxAudioSimplelong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            elseif strcmp(ev(1),'17')
                MxTactSimplelong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxTactSimplelong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            else
                MxAudioSnslong(y,1,:,:) = squeeze(nanmean(MxSbjCisi(1,:,:,:),2));
                MxAudioSnslong(y,2,:,:) = squeeze(nanmean(MxSbjCisi(2,:,:,:),2));
            end
           
         end
        
    end 
    end
 end

%  save(['Y:\PILAe\EEG_data\', 'AllSbjVisuelSimpleShort_ERPdata.mat'], 'MxVisuelSimpleshort');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjVisuelSimpleLong_ERPdata.mat'], 'MxVisuelSimplelong');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjAudioSimpleShort_ERPdata.mat'], 'MxAudioSimpleshort');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjAudiolSimpleLong_ERPdata.mat'], 'MxAudioSimplelong');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjTactSimpleShort_ERPdata.mat'], 'MxTactSimpleshort');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjTactSimpleLong_ERPdata.mat'], 'MxTactSimplelong');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjVisuelSnsShort_ERPdata.mat'], 'MxVisuelSnsshort');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjVisuelSnsLong_ERPdata.mat'], 'MxVisuelSnslong');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjAudioSnsShort_ERPdata.mat'], 'MxAudioSnsshort');
%  save(['Y:\PILAe\EEG_data\', 'AllSbjAudiolSnsLong_ERPdata.mat'], 'MxAudioSnslong');
 