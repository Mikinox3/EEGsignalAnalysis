function EEGerp = ERPdata(EEG,doPlots,size,ev,evl)

%% Parameters for ERP analysis

% Parameters for epoching

% Time window to epoch in seconds
Perp.Epoch.tw = [-0.1, 1];  

% Events relative to which the data epoched 
% add un truc pour choisir les conditions selon title (here for visual simple)
if strcmp(size, 'short')
Perp.Epoch.ev = ev;
else
Perp.Epoch.ev = evl;
end;
% Parameters for artifacts interpolation
Perp.Int = ex1_APICE_Interpolation;

% Parameters to define Bad Times (BT) and Bad Channels (BC) 
% Limits for the proportion of BT to define a BC during the whole recording (the last value is the final/effective one, here 30 %)
Perp.BCall.nbt           = [0.70 0.50 0.50 0.30];
% Limits for the proportion of BT to define a BC during each epoch (the last value is the final/effective one, here 100 ms)
Perp.BCep.nbt            = [0.70 0.50 0.30 0.10/diff(Perp.Epoch.tw)];
% Limits for the proportion of BC to define a BT (the last value is the final/effective one, here 30 %)
Perp.BTep.nbc            = [0.70 0.50 0.30 0.30]; 
% Shorter intervals between bad segments will be marked as bad
Perp.BTep.minGoodTime    = 1.00;            
% Shorter periods will not be considered as bad
Perp.BTep.minBadTime     = 0.100;            
% Also mark as bad surronding samples within this value 
Perp.BTep.maskTime       = 0;         

% Parameters to define Bad Epochs (BE) based on the amount of bad data       
% Maximun proportion of bad data per epoch  
Perp.DefBEa.limBCTa      = 1.00;   
% Maximun proportion of bad times per epoch  
Perp.DefBEa.limBTa       = 0.00;   
% Maximun proportion of bad channels per epoch  
Perp.DefBEa.limBCa       = 0.30;   
%Perp.DefBEa.limBCa       = 0.50;  
% Maximun proportion of interpolated data per epoch  
Perp.DefBEa.limCCTa      = 0.50;   

% Time window (in ms) to compute the baseline correction
Perp.BL.tw = [-100 0];  
% Parameters to generate the report
Perp.report.patternname = 'data';  
Perp.report.patternn = [];

    
	% Epoch

	EEG = eega_epoch(EEG, Perp.Epoch.ev, Perp.Epoch.tw);
    
    %figure; plot(mean(mean(EEG.data(:,:,:),3),1),'r')
    
	% Define Bad Times and Bad Channels on the epoched data
    EEG = eega_tDefBTBC(EEG, Perp.BTep.nbc, Perp.BCep.nbt, Perp.BCall.nbt, 'keeppre', 0, 'minBadTime', Perp.BTep.minBadTime, 'minGoodTime', Perp.BTep.minGoodTime, 'maskTime', Perp.BTep.maskTime);

	% Interpolate the bad channels for each epoch
    EEG = eega_tInterpSpatialEEG(EEG, Perp.Int.Spl.p,'pneigh', Perp.Int.Spl.pneigh);
    EEG = eega_tDefBTBC(EEG, Perp.BTep.nbc, Perp.BCep.nbt, Perp.BCall.nbt, 'keeppre', 0, 'minBadTime', Perp.BTep.minBadTime, 'minGoodTime', Perp.BTep.minGoodTime, 'maskTime', Perp.BTep.maskTime);
    
    % Define bad epochs
    EEG = eega_tDefBEbaddata(EEG, Perp.DefBEa, 'keeppre', 0, 'plot', 0);
    
    % Plot the rejection matrix
    if doPlots
        eega_plot_artifacts(EEG)
        set(gcf, 'Name', 'artifacts rejection 2')
    end
    
   %hold on; plot(mean(mean(EEG.data(:,:,:),3),1),'g')
    
    % Remove the bad epochs
    EEG = eega_rmvbadepochs(EEG);
    
   if isempty(EEG.data)
    disp('ERROR : No epoch retained');
  end;

  if ~isempty(EEG.data)
  	% Average reference 
    EEG = pop_reref(EEG,[]);
    
	% Baseline correction
    EEG = eega_rmbaseline(EEG, Perp.BL.tw);
    
  end;
  EEGerp =EEG;
end 