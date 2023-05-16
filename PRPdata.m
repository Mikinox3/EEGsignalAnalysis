function EEGprp = PRPdata(EEG,doPlots)



%----------------PARAMETERS-----------------------------
% Parameters for plotting
% Plot rejection matrixes at different stages
% (1) plot | (0) do not plot
Pplot.rejection = doPlots; 

% Parameters for filtering

% High pass filter
Ppp.filt_highpass   = 0.1;

% Low pass filter
Ppp.filt_lowpass    = 100;

% High pass filter
%Perp.filt_highpass = 0.2;

% Low pass filter
Perp.filt_lowpass = 40;

% Parameters for artifacts detection

% get algorithm to detect bad electrodes
Ppp.ArtBadEl = ex1_APICE_ArtPP_BadEl(3);
% to detect jumps in the signal
Ppp.ArtJump = ex1_APICE_ArtPP_Jump(3);
% to detect motion artifacts
Ppp.ArtMot1 = ex1_APICE_ArtPP_Mot1(3);
% to detect motion artifacts
Ppp.ArtMot2 = ex1_APICE_ArtPP_Mot2(3);

% Parameters for transient artifacts interpolation

Ppp.IntTransientArtPCA = 1;
Ppp.IntTransientArtSpline = 1;
Ppp.Int = ex1_APICE_Interpolation;

% Parameters to define Bad Times (BT) and Bad Channels (BC) 

% Limits for the proportion of BT to define a BC (the last value is the final/effective one)
Ppp.BCall.nbt           = [0.70 0.50 0.30];
% Limits for the proportion of BC to define a BT (the last value is the final/effective one)
Ppp.BTall.nbc           = [0.70 0.50 0.30];
% Shorter intervals between bad segments will be marked as bad
Ppp.BTall.minGoodTime   = 1.000;
% Shorter periods will not be considered as bad
Ppp.BTall.minBadTime    = 0.100;   
% Also mark as bad surronding samples within this value 
Ppp.BTall.maskTime      = 0.500;     

% Parameters to generate the report
Ppp.report.patternname = 'data';  
Ppp.report.patternn = [];

%-----END PARAMETERS -----------------

% Filter
    	EEG = eega_demean(EEG);
    	EEG = pop_eegfiltnew(EEG, [], Ppp.filt_lowpass,  [], 0, [], [], 0);
    	EEG = pop_eegfiltnew(EEG, Ppp.filt_highpass, [], [], 0, [], [], 0);
        
	% Detect artifacts

	% detect bad channels
    	EEG = eega_tArtifacts(EEG, Ppp.ArtBadEl, 'KeepRejPre', 1);

	% detect motion artifacts
    	EEG = eega_tArtifacts(EEG, Ppp.ArtMot1, 'KeepRejPre', 1);
	
	% detect jumps in the signal
	    EEG = eega_tArtifacts(EEG, Ppp.ArtJump, 'KeepRejPre', 1);

	% define Bad Times and Bad Channels
    	EEG = eega_tDefBTBC(EEG, Ppp.BTall.nbc, Ppp.BCall.nbt, Ppp.BCall.nbt, 'keeppre', 0, 'minBadTime', Ppp.BTall.minBadTime, 'minGoodTime', Ppp.BTall.minGoodTime, 'maskTime', Ppp.BTall.maskTime);

	% Plot the rejection matrix
    if Pplot.rejection
        eega_plot_artifacts(EEG)
        set(gcf, 'Name', 'artifacts rejection 1')
        
        eega_plot_rejection(EEG, 1, 1, 1, 120)
        set(gcf, 'Name', 'artifacts rejection 1')
    end

 	 % Correct transient artifacts 
    
   	 % correct brief jumps in the signal using target PCA
   	 if Ppp.IntTransientArtPCA
       		EEG = eega_tTargetPCAxElEEG(EEG, Ppp.Int.PCA.nSV, Ppp.Int.PCA.vSV, 'maxTime', Ppp.Int.PCA.maxTime,'maskTime', Ppp.Int.PCA.maskTime,'splicemethod', Ppp.Int.PCA.splicemethod);
        	EEG = eega_demean(EEG);
       	 	EEG = pop_eegfiltnew(EEG, Ppp.filt_highpass, [], [], 0, [], [], 0);
        	EEG = eega_tDefBTBC(EEG, Ppp.BTall.nbc, Ppp.BCall.nbt, Ppp.BCall.nbt, 'keeppre', 0, 'minBadTime', Ppp.BTall.minBadTime, 'minGoodTime', Ppp.BTall.minGoodTime, 'maskTime', Ppp.BTall.maskTime);
   	end


    % Spatially interpolate channels not working during the whole recording 
    EEG = eega_tInterpSpatialEEG(EEG, Ppp.Int.Spl.p, 'pneigh', Ppp.Int.Spl.pneigh);
    
   % Plot the rejection matrix
    if Pplot.rejection
        eega_plot_artifacts(EEG)
        set(gcf, 'Name', 'artifacts correction')
        
        
        eega_plot_rejection(EEG, 1, 1, 1, 120)
        set(gcf, 'Name', 'artifacts correction')
    end
    

    % Detect artifacts again
    EEG = eega_tArtifacts(EEG, Ppp.ArtMot2, 'KeepRejPre', 0);
    EEG = eega_tDefBTBC(EEG, Ppp.BTall.nbc, Ppp.BCall.nbt, Ppp.BCall.nbt, 'keeppre', 0, 'minBadTime', Ppp.BTall.minBadTime, 'minGoodTime', Ppp.BTall.minGoodTime, 'maskTime', Ppp.BTall.maskTime);
    
    % Plot the rejection matrix
    if Pplot.rejection
        eega_plot_artifacts(EEG)
        set(gcf, 'Name', 'artifacts rejection 2')
        
        eega_plot_rejection(EEG, 1, 1, 1, 120)
        set(gcf, 'Name', 'artifacts rejection 2')
    end

    % Filter
    EEG = pop_eegfiltnew(EEG, [], Perp.filt_lowpass,  [], 0, [], [], 0);
    

    
    EEGprp=EEG;
end