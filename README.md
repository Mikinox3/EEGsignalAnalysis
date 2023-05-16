# EEGsignalAnalysis

Matlab scripts for EEG signal filtration, artifact detection and ERP calculation (APICE inspired see below). Also contains a script for ERP peak amplitude, peak latency, MAD calculation.
Optionnaly calculate ERSP, ITPC.
Wavelets transform is used for frequency domain parameters calculation.
PCA and k-means are used for dimention reduction of the final dataset (see the R script)

Read the ReadMe.txt for more info.

APICE pipeline functions are used in the script, for more info see : Fló A, Gennari G, Benjamin L, Dehaene-Lambertz G. Automated Pipeline for Infants Continuous EEG (APICE): A flexible pipeline for developmental cognitive studies. Dev Cogn Neurosci. 2022 Apr;54:101077. doi: 10.1016/j.dcn.2022.101077. Epub 2022 Jan 25. PMID: 35093730; PMCID: PMC8804179.

Please note : this script has been created for a very specific experimental paradigm with precise triggers created during EEG recording.
