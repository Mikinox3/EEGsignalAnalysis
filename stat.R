rm(list=ls()) 
#**************************************
Path="xxxxx/SingleTrialAnalysis/" 
setwd(Path)

library(tkrplot)
library(ggplot2)
library("dplyr")
library("ggpubr")
library(R.matlab)
library(rptR)

alpha = 0.05
Normal = list()
Postrial = TRUE

theFiles <- list.files(Path,pattern="Subj*")
gpA <- theFiles[1:(length(theFiles)/2)]
gpB <- na.omit(theFiles[(length(theFiles)/2)+1:length(theFiles)])
k=1

#**************************************

for (k in (1:length(theFiles))) {

  

  dataA <- readMat(gpA[k])
  print(gpA[k])
  dataB <- readMat(gpB[k])
  print(gpB[k])
  
  
  ntAA = length(dataA$single.P1.ampl)/2
  ntBB = length(dataB$single.P1.ampl)/2

  
  #***********************************************
  #*Positive Trials Only --> rm the trials showing neg amp on O1 or O2 
  if(Postrial){
    f = 1
    for (f in 1:ntAA){
      if (dataA$single.P1.ampl[f,1]<0|dataA$single.P1.ampl[f,2]<0){
        dataA$single.P1.ampl[f,1:2]<- NaN
        dataA$single.P1.lat[f,1:2]<- NaN
      }
    }
    f = 1
    for (f in 1:ntBB){
      if (dataB$single.P1.ampl[f,1]<0|dataB$single.P1.ampl[f,2]<0){
        dataB$single.P1.ampl[f,1:2]<- NaN
        dataB$single.P1.lat[f,1:2]<- NaN
      }
    }
   
    
  }
  
  ntA = length(na.omit(dataA$single.P1.ampl[1:ntAA,1]))
  ntB = length(na.omit(dataB$single.P1.ampl[1:ntBB,1]))
  
  POSdataA = list()
  POSdataB =list()
  
  
  POSdataA$single.P1.O1.ampl = na.omit(dataA$single.P1.ampl[1:ntAA,1])
  POSdataA$single.P1.O2.ampl= na.omit(dataA$single.P1.ampl[1:ntAA,2])
  POSdataA$single.P1.O1.lat = na.omit(dataA$single.P1.lat[1:ntAA,1])
  POSdataA$single.P1.O2.lat = na.omit(dataA$single.P1.lat[1:ntAA,2])
  
  POSdataB$single.P1.O1.ampl = na.omit(dataB$single.P1.ampl[1:ntBB,1])
  POSdataB$single.P1.O2.ampl = na.omit(dataB$single.P1.ampl[1:ntBB,2])
  POSdataB$single.P1.O1.lat = na.omit(dataB$single.P1.lat[1:ntBB,1])
  POSdataB$single.P1.O2.lat = na.omit(dataB$single.P1.lat[1:ntBB,2])
  
  
  #***********************************************
  #*check distribution normality for session A & B for Amp & Lat On O1 & O2 separately
  #*
  n=1
  color = c('skyblue','aquamarine3','goldenrod3','lightgoldenrod')
  label=c("AmplitudeO1","LatenceO1","AmplitudeO2","LatenceO2")
  Idn = c("AmplO1","LatO1","AmplO2","LatO2")
  
  for (n in 1:2) {
    #define plotting region
    par(mfrow=c(1,2)) 
    #create histogram for both datasets
    hist(dataA$single.P1.ampl[1:ntA,n], col=color[n], main='SessionA',xlab = gpA[k])
    hist(dataB$single.P1.ampl[1:ntB,n], col=color[n], main='SessionB',xlab = label[n*2-1])
    
    #test data normality
    y = shapiro.test(dataA$single.P1.ampl[1:ntA,n])
    w = shapiro.test(dataB$single.P1.ampl[1:ntB,n])
    
    if(y$p.value>alpha && w$p.value>alpha)
    {
      #test data var homogeneity
      z= var.test(dataA$single.P1.ampl[1:ntA,n],dataB$single.P1.ampl[1:ntB,n])
      if (y$p.value>alpha){
        Id = c(gpA[k],Idn[n*2-1])
        Normal = c(Normal,Id)
      }
      
    } 
    
    #**********************************************
    
    
    #define plotting region
    par(mfrow=c(1,2)) 
    #create histogram for both datasets
    hist(dataA$single.P1.lat[1:ntA,n], col=color[n+2], main='SessionA',xlab = gpA[k])
    hist(dataB$single.P1.lat[1:ntB,n], col=color[n+2], main='SessionB',xlab = label[n*2])
    
    #test data normality
    y = shapiro.test(dataA$single.P1.lat[1:ntA,n])
    w = shapiro.test(dataB$single.P1.lat[1:ntB,n])
    
    if(y$p.value>alpha && w$p.value>alpha)
    {
      #test data var homogeneity
      z= var.test(dataA$single.P1.lat[1:ntA,n],dataB$single.P1.lat[1:ntB,n])
      if (y$p.value>alpha){
        Id = c(gpA[k],Idn[n*2])
        Normal = c(Normal,Id)
      }
      
    } 

  }
}

#t.test(sample(dataA$single.P1.lat[1:ntA,1],ntB),dataB$single.P1.lat[1:ntB,1],paired = TRUE,var.equal = TRUE)


# ICC calculation
Session <- c( purrr::rerun(ntA, 1), purrr::rerun(ntB, 2))
AmpO1 = list(c(POSdataA$single.P1.O1.ampl,POSdataB$single.P1.O1.ampl))

SingleTrial= data.frame(unlist(Session),unlist(AmpO1))
names(SingleTrial) = c("Session","AmpO1")

I= rpt(AmpO1 ~ (1 | Session), grname = "Session", data = SingleTrial, datatype = "Gaussian", 
    nboot = 1000, npermut = 0)
