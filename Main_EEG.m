clear all
close all
clc

%write the subjects names in the subject list in 'subjectsList.txt'
% arrange the data in files so it fits to the default datafile struct and path 
%Path2DataBdf = 'RawData\BDFeeg'
%PathERPdata = 'xxxx';
%PathPRPdata = 'xxxx';

%----SCRIPT READY TO BE RUN----

Path2EEGLAB = fullfile('xxx\eeglab14_1_2b');
Path2APICE = fullfile('xxx\APICE');
Path0 = 'xxxx\APICE\examples\example_original';
Path2Parameters = fullfile(Path0,'parameters');
filechanloc = fullfile(Path2EEGLAB,'plugins','dipfit2.3','standard_BESA','standard-10-5-cap385.elp'); 

addpath 'G:\xxx\APICE'
cd(Path2EEGLAB)
eeglab
close all
cd(Path0)
addpath(genpath(Path2APICE))
addpath(Path2Parameters)

%------------- PARAMETERS------------------

% extension reader
Ppp.Files2Read = '*.set';
Ppp.BdfFiles2Read = '*.bdf';

doPlots = 0;
%----------- END PARAMETERS --------------

 %Reads the subject list
 Sbj= importdata('subjectsList.txt');
 %Sbj=1;

for y = 1:length(Sbj)
     
    Path2DataBdf = char(strcat('xxxx\EEG_data\',Sbj(y),'\RawData\BDFeeg'));     
    PathPRPdata = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\PRP\')); 
    PathERPdata = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\ERP\')); 
    Path2Data = char(strcat('xxxx\EEG_data\',Sbj(y),'\EEGpreprocessed\T1sessionA\')); 
    % Read the files.bdf (one file per stim block) for a given subject
    files2pp = dir(fullfile(Path2DataBdf,Ppp.BdfFiles2Read));
    
    %preprocess for each file for a given subject
    for condition=1:length(files2pp)
        %condition =1;
        FilesnameIn = files2pp(condition).name;
        
        %load and convert Biosig data in matlab structure
        EEG = pop_biosig(fullfile(Path2DataBdf, FilesnameIn),'channels',(1:64),'ref',47);                                                                                                                                                                                                                                                                                                                                                                                                                                       
        
        % Import the channels location file
        EEG=pop_chanedit(EEG, 'lookup','xxxxx\eeglab14_1_2b\plugins\dipfit2.3\standard_BESA\standard-10-5-cap385.elp');
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    %Script originally Written by MSR_2023
        EEG = eeg_checkset( EEG );
        disp ([FilesnameIn,' successfully loaded'])
        
        % Preprocess the data
        EEG = PRPdata(EEG,doPlots);
        
        % Save the continous pre-processed data
        FilesnameIn = FilesnameIn(1:end-4);
        save([PathPRPdata, FilesnameIn '_PRP.set'], '-struct', 'EEG');
        
        % set the event triggers according to the stim block analysed
        %[ev, evl]=SetEpochEvents(FilesnameIn);
        
        IstimSize={'short','long'};
        n=cellstr(IstimSize);
        % set the event triggers according to the stim block analysed
        [ev, evl]=SetEpochEvents(FilesnameIn);
        %ev = {'condition 31','condition 41'};
        %evl = {'condition 32','condition 42'};
        eventIsi = [ev,evl];
        
        % ERP creation for short and long isi
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
            
            %Matrix splitting event type (social/non social OR low/high freq) ERP for a given stim block 
             MxSbjCisi = NaN([2,round(EEG.trials/2),EEG.nbchan,EEG.pnts]);

            
            %Mx with eventtype correspondance for each epoch
            mcell=struct2cell(EEG.epoch);
            mep=squeeze(cell2mat(mcell(1,1,:)));
            mevtype=squeeze(cell2mat(mcell(2,1,:)));
             %mevtype=permute(cell2mat(mcell(2,1,:)), [3,2,1]);
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
            
%             figure; plot(squeeze(mean(nanmean(mean(MxSbjCisi(:,:,[25,26,27,29,61,62,63,64],:),1),2),3)),'r')
%             
%             figure;topoplot(squeeze(mean(EEG.data(:,200,:),3)), EEG.chanlocs, 'electrodes', 'numbers')
% 
%             figure; pop_timtopo(EEG, [-1 500], [], 'ERP data and scalp maps','plotchans',[25,26,27,29,61,62,63,64]);

            
        end
        
        
   end

end


 %-----PlOT THE DATA

%-------Plot specific condition in Mx-------

% hold on; plot(mean(ALLEEG(2).data(64,769:1332,:),3), 'r')
% figure; plot(mean(ALLEEG(1).data(63,:,:),3),'r')
% hold on; plot(mean(ALLEEG(2).data(64,:,:),3), 'g')
% hold on; plot(mean(ALLEEG(3).data(64,:,:),3), 'k')

%figure; plot(squeeze(nanmean(MxSbjCisi(1,:,64,:),2)),'r')

%figure; pop_timtopo(EEG, [-1 700.0469], [], 'ERP data and scalp maps','plotchans',[25,26,27,29,61,62,63]);
% topoplot(squeeze(mean(EEG.data(:,200,:),3)), EEG.chanlocs, 'electrodes', 'numbers')

% FOR ERPS, ITC CALCULATION
% in loadfromPRP_ERP : [P,R,mbase,times,freqs,Pboot,Rboot,Rphase,PA] =  pop_newtimef( EEG, 1, 29, [-100  998], [3         0.5] , 'topovec', 29, 'elocs', EEG.chanlocs, 'chaninfo', EEG.chaninfo, 'caption', 'Oz', 'baseline',[0], 'freqs', [0 40], 'plotphase', 'off', 'ntimesout', 50, 'padratio', 1)

 



