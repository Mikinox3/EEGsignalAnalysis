///////////////////////////////////////////////////// 20min pour 7.bdf

When you get your .bdf files from Biosemi software

--------- Main_EEG.mat-----------------------------------------------------------------------------------------------------------------------------------------

////Open & Run Main_EEG.mat (change paths and directories if required for your data organization + create a subject_list.txt)

It will create :
---> a _PRP.set and a _ERP.set file (PRP = preprocessed data before epoaching and ERP are epoched data with ERP ready to be plot)
---> matrixes MxSbjCisi  --> for 1 sbj with N conditions (ex : visuelsimple,visuelsns...) u get N*2 matrixes (for each condition u get 1 for short isi 1 for long isi)

Nb: 
use line 102/103 instead of  101 if errors in trigger detection (default is '15','14' but sometimes it ca be 'condition 15','condition 14' in the bdf file)
use line 130 instead of 129 if error for Mxev / mevtype creation

Nb: at the end of the script you can calculate ERSP and ITC of a specific ERP .set file

///The Main_EEG.mat file requires to access PRPdata.m + ERPdata.m + SetEpochEvent.m functions

-------If all your data.bdf files are correct  you'll get all the ERP and recap matrixes with this script-----------------------------------------


If you have issues or the loop crashes and you need to reload some files 
If during your ERP visualization you want to check again the file content


--------- LoadfromPRP_ERP.mat-------------------------------------------------------------------------------------------------------------------------------------

You can use the LoadfromPRP_ERP.mat file (as u can guess you save time because you don't have to do again the time consuming filtering and artifact analysis to explore
the file content or finish the preprocess).

It will create :
---> a _ERP.set file if a PRP.set file is loaded
---> matrixes MxSbjCisi  --> for 1 sbj with N conditions (ex : visuelsimple,visuelsns...) u get N*2 matrixes (for each condition u get 1 for short isi 1 for long isi)
---> optionnaly Big Matrixes containing for one stim type + complexity + interstiminterval : all the subjects data (64elect over all times) for the 2 conditions (hf/lf or social/NS) 

NB : line 195:200 you can try several plots

-----------------------------------------------------------you'll get all the ERP and recap matrixes with this script-----------------------------------------

If you have all your MXSbjCisi matrixes for all sbj all conditions and isi and you want to get the Big Matrixes containint all sbj data, use :


--------- CreateMx.mat---------------------------------------------------------------------------------------------------------------------------------------

You can use CreateMx.mat to generate the big matrixes and ERP plots from the MxSbjCisi matrixes


-----------------------------------------------------------you'll get all the ERP and Big Matrixes -----------------------------------------

you can try to plot your data with plots.m
you can calculate your peak amplitudes,latencies and MAD with ParamCalculation.m

Finally, you can use stat.R to chack normality and homogeneity of the data before using a parametric test.
PCA is for data reduction if required.
