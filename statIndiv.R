rm(list=ls())


library(svglite)
library(tkrplot)
library(ggplot2)
library("dplyr")
library("ggpubr")
library(R.matlab)
library(rptR)
library(fmsb)
library("FactoMineR")
library("factoextra")

#**************************************
#*#***********FUNCTION DEF*************
#**************************************


normy<- function(x){
  
    y = (x-min(x))/(max(x)-min(x))
    return(y)
}

sp <- function (sessionA, sessionB,name,Sbj,iiv,col,mod){
  L=length(sessionA)
  sessionA = data.frame(sessionA)
  sessionB <- data.frame(sessionB)
  rownames(sessionA) <- name
  rownames(sessionB) <- name
  max = (rep(1,times= L))
  min = (rep(0,times =L))
  colo = col 
  
  d= cbind(max,min,sessionA,sessionB)
  
  title = (paste(mod,"_sujet : ",Sbj,sep = ""))
  
  op <- par(mar = c(1, 1, 1, 1))
  radarchart(data.frame(t(d)),pcol = colo, pfcol =  scales::alpha(colo, 0.5),vlcex = 0.7,title = title,plwd =2,cglty = 5,cglwd=1, cglcol="black")
  par(op)
  
  if (iiv == "N"){
    
    w= paste("sbj",Sbj,"_plot.pdf",sep = "")
    
    
  }else{
    w=paste("sbj",Sbj,"_iiv_plot.pdf",sep = "")
    
  }
  pdf(w)
  op <- par(mar = c(1, 0.3, 1, 0.3))
  radarchart(data.frame(t(d)),pcol = colo, pfcol =  scales::alpha(colo, 0.5),vlcex = 0.7,title = title,plwd =2,cglty = 5,cglwd=1, cglcol="black")
  par(op)
  
  dev.off()
  
}

spId <- function (data,name,Sbj){
  L=length(data)
  ID = data.frame(data)
  ID = t(ID)
  rownames(ID) <- name
  max = (rep(1,times= L))
  min = (rep(0,times =L))
  
  d= cbind(max,min,ID)
  
  title = (paste("ID_",Sbj,sep = ""))
  
  op <- par(mar = c(1, 1, 1, 1))
  radarchart(data.frame(t(d)),pcol = '#C00052', pfcol =  scales::alpha('#C00052', 0.5),vlcex = 0.7,title = title,plwd =2,cglty = 5,cglwd=1, cglcol="black")
  
  par(op)
  
  w= paste("sbj",Sbj,"_ID_plot.pdf",sep = "")
  
  pdf(w)
  op <- par(mar = c(1, 0.3, 1, 0.3))
  radarchart(data.frame(t(d)),pcol = '#C00052', pfcol =  scales::alpha('#C00052', 0.5),vlcex = 0.7,title = title,plwd =2,cglty = 5,cglwd=1, cglcol="black")
  dev.off()
  
}


#**************************************
#*#************PATH & PARAM DEF********
#*#**************************************


Path="xxxxx/EEG_data/" 
setwd(Path)



Mod = c("Vsimp","Vsns","Asimp","Asns","Tsimp") 
ChooseMod = 5
typ= c("mean_e","mean_e_by_cond","mean_e_bycond_bycomp")
t= 3
opt = 1

#**************************************
#******************GET THE MOD DATA****
#**************************************

if (ChooseMod == 1 | ChooseMod == 2 ) {
  
  Path2MeanALA = paste(Path,"ERPs/T1SessionA/PEV/",Mod[ChooseMod], sep = "")
  Path2MeanALB = paste(Path,"ERPs/T1SessionB/PEV/",Mod[ChooseMod],  sep = "")
  Path2IIV = paste(Path,"IIV/",Mod[ChooseMod],  sep="")   
  
} else if (ChooseMod == 3 | ChooseMod == 4 ){
  
  Path2MeanALA = paste(Path,"ERPs/T1SessionA/PEA/",Mod[ChooseMod], sep = "")
  Path2MeanALB = paste(Path,"ERPs/T1SessionB/PEA/", Mod[ChooseMod], sep = "")
  Path2IIV = paste(Path,"IIV/", Mod[ChooseMod], sep="")   
  
} else {
  Path2MeanALA = paste(Path,"ERPs/T1SessionA/PES/", sep = "")
  Path2MeanALB = paste(Path,"ERPs/T1SessionB/PES/", sep = "")
  Path2IIV = paste(Path,"IIV/",Mod[ChooseMod],  sep="")   
}
  if (ChooseMod == 5){
    
    setwd(Path2MeanALA)
    theFiles <- list.files(Path2MeanALA, pattern = "P50_*")
    ALAP50= readMat(theFiles[2])
    setwd(Path2MeanALB)
    theFiles <- list.files(Path2MeanALB, pattern = "P50_*")
    ALBP50= readMat(theFiles[2])
    
    
    setwd(Path2MeanALA)
    theFiles <- list.files(Path2MeanALA, pattern = "N70_*")
    ALAN1= readMat(theFiles[2])
    setwd(Path2MeanALB)
    theFiles <- list.files(Path2MeanALB, pattern = "N70_*")
    ALBN1= readMat(theFiles[2])
    
    setwd(Path2MeanALA)
    theFiles <- list.files(Path2MeanALA, pattern = "P10_*")
    ALAP100= readMat(theFiles[2])
    setwd(Path2MeanALB)
    theFiles <- list.files(Path2MeanALB, pattern = "P10_*")
    ALBP100= readMat(theFiles[2])
    
    setwd(Path2MeanALA)
    theFiles <- list.files(Path2MeanALA, pattern = "P20_*")
    ALAP200= readMat(theFiles[2])
    setwd(Path2MeanALB)
    theFiles <- list.files(Path2MeanALB, pattern = "P20_*")
    ALBP200= readMat(theFiles[2])
    
    setwd(Path2IIV)
    theFiles <- list.files(Path2IIV)
    PTP50 = readMat(theFiles[8])
    MesP50 = readMat(theFiles[7])
    PTN70 = readMat(theFiles[2])
    MesN70 = readMat(theFiles[1])
    PTP100 = readMat(theFiles[4])
    MesP100 = readMat(theFiles[3])
    PTP200 = readMat(theFiles[6])
    MesP200 = readMat(theFiles[5])
    
    
    
  }else{

  setwd(Path2MeanALA)
  theFiles <- list.files(Path2MeanALA, pattern = "P1_*")
  ALAP1= readMat(theFiles[1])
  setwd(Path2MeanALB)
  theFiles <- list.files(Path2MeanALB, pattern = "P1_*")
  ALBP1= readMat(theFiles[1])
  

  setwd(Path2MeanALA)
  theFiles <- list.files(Path2MeanALA, pattern = "N1_*")
  ALAN1= readMat(theFiles[1])
  setwd(Path2MeanALB)
  theFiles <- list.files(Path2MeanALB, pattern = "N1_*")
  ALBN1= readMat(theFiles[1])
 
  setwd(Path2IIV)
  theFiles <- list.files(Path2IIV)
  PTP1 = readMat(theFiles[4])
  MesP1 = readMat(theFiles[3])
  PTN1 = readMat(theFiles[2])
  MesN1 = readMat(theFiles[1])
  }
 

#**************************************
#********TYPE OF COMPARISON************
#**************************************


if (typ[t]=="mean_e")
{
  
  if (Mod[ChooseMod]=="Vsimp"){
    
    
    rho_Vsimp = list()
    rho = list()
    nbsbj = 10
    Sbj = 1
    
    for (Sbj in 1:nbsbj){
      
      Amp_A_N1 = normy((ALAN1$N170.ampl[1:nbsbj,1:2,1] + ALAN1$N170.ampl[1:nbsbj,1:2,2])/2)
      Amp_A_N1 = 1-Amp_A_N1
      Lat_A_N1 = normy((ALAN1$N170.lat[1:nbsbj,1:2,1] + ALAN1$N170.lat[1:nbsbj,1:2,2])/2)
      Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,1:2,1] + ALAP1$P100.amplA[1:nbsbj,1:2,2])/2)
      Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,1:2,1] + ALAP1$P100.latA[1:nbsbj,1:2,2])/2)
      MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,1:2,1]+MesN1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,1:2,1]+MesN1$MAD.ampl[1:nbsbj,1:2,2])/2)
      MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,1:2,1]+MesP1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,1:2,1]+MesP1$MAD.ampl[1:nbsbj,1:2,2])/2)
      PTN1_A = normy((PTN1$values[1,1:nbsbj,1:2,1]+PTN1$values[1,1:nbsbj,1:2,2])/2)
      PTP1_A = normy((PTP1$values[1,1:nbsbj,1:2,1]+PTP1$values[1,1:nbsbj,1:2,2])/2)
      
      Amp_B_N1 = normy((ALBN1$N170.ampl[1:nbsbj,1:2,1] + ALBN1$N170.ampl[1:nbsbj,1:2,2])/2)
      Amp_B_N1 = 1-Amp_B_N1
      Lat_B_N1 =normy((ALBN1$N170.lat[1:nbsbj,1:2,1] + ALBN1$N170.lat[1:nbsbj,1:2,2])/2)
      Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,1:2,1] + ALBP1$P100.amplB[1:nbsbj,1:2,2])/2)
      Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,1:2,1] + ALBP1$P100.latB[1:nbsbj,1:2,2])/2)
      MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      PTN1_B = normy((PTN1$values[2,1:nbsbj,1:2,1]+PTN1$values[2,1:nbsbj,1:2,2])/2)
      PTP1_B = normy((PTP1$values[2,1:nbsbj,1:2,1]+PTP1$values[2,1:nbsbj,1:2,2])/2)
      
      
      
      #sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      #sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      
      
      sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2])
      sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2])
      
      
      sessionAiiv =c(MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      sessionBiiv = c(MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      
      #color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange")
      
      #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
      
      color =c("blue","blue","green","green","red","red","orange","orange")
      coloriiv =c("blue","blue","blue","green","green","green","red","red","red","orange","orange","orange")
      
      name = c("AMP_P1_C1","Lat_P1_C1","AMP_P1_C2","Lat_P1_C2","AMP_N1_C1","Lat_N1_C1","AMP_N1_C2","Lat_N1_C2")
      nameiiv = c("MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","MADL_P1_C2","MADA_P1_C2","PTP1_C2","MADL_N1_C1","MADA_N1_C1","PTN1_C1","MADL_N1_C2","MADA_N1_C2","PTN1_C2")
      
      
      df = data.frame(sessionA,sessionB,color)
      plot( ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      df2 = data.frame(sessionAiiv,sessionBiiv,coloriiv)
      plot( ggplot(df2,aes(x=sessionAiiv, y=sessionBiiv)) + geom_point(aes(size= 0.1,colour = coloriiv),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      
      # shapiro.test(sessionA)
      # shapiro.test(sessionB)
      # var.test(sessionA,sessionB)
      
      rho_Vsimp = c(rho_Vsimp, cor(c(sessionA,sessionAiiv),c(sessionB,sessionBiiv)))
      
      rho = c(rho,cor.test(c(sessionA,sessionAiiv), c(sessionB,sessionBiiv) , method = c("spearman")))
      
      col = c("#898989","#E7B800") 
      sp(sessionA,sessionB,name,Sbj,"N",col,Mod[ChooseMod])
      sp(sessionAiiv,sessionBiiv,nameiiv,Sbj,"Y",col,Mod[ChooseMod])
      
      
    }
  }
 
  
  if (Mod[ChooseMod]=="Vsns"){
    
    rho_Vsns = list()
    rho = list()
    nbsbj = 11
    Sbj = 1
    
    for (Sbj in 1:nbsbj){
      
      
      Amp_A_N1 = normy((ALAN1$DeltaP1N170PO78A[1:nbsbj,1:2,1] + ALAN1$DeltaP1N170PO78A[1:nbsbj,1:2,2])/2)
      Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,1:2,1] + ALAP1$P100.amplA[1:nbsbj,1:2,2])/2)
      Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,1:2,1] + ALAP1$P100.latA[1:nbsbj,1:2,2])/2)
      Lat_A_N1 = normy((MesN1$Avg.P1.all.Lat[1:nbsbj,1:2,1]+MesN1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,1:2,1]+MesN1$Avg.P1.all.Lat[1:nbsbj,1:2,2])/2)
      MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,1:2,1]+MesN1$MAD.ampl[1:nbsbj,1:2,2])/2)
      MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,1:2,1]+MesP1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,1:2,1]+MesP1$MAD.ampl[1:nbsbj,1:2,2])/2)
      PTN1_A = normy((PTN1$values[1,1:nbsbj,1:2,1]+PTN1$values[1,1:nbsbj,1:2,2])/2)
      PTP1_A = normy((PTP1$values[1,1:nbsbj,1:2,1]+PTP1$values[1,1:nbsbj,1:2,2])/2)
      
      
      
      Amp_B_N1 = normy((ALBN1$DeltaP1N170PO78B[1:nbsbj,1:2,1] + ALBN1$DeltaP1N170PO78B[1:nbsbj,1:2,2])/2)
      Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,1:2,1] + ALBP1$P100.amplB[1:nbsbj,1:2,2])/2)
      Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,1:2,1] + ALBP1$P100.latB[1:nbsbj,1:2,2])/2)
      Lat_B_N1 = normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADlat_B_N1 =normy((MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      PTN1_B = normy((PTN1$values[2,1:nbsbj,1:2,1]+PTN1$values[2,1:nbsbj,1:2,2])/2)
      PTP1_B = normy((PTP1$values[2,1:nbsbj,1:2,1]+PTP1$values[2,1:nbsbj,1:2,2])/2)
      
      # sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],Amp_A_N1[Sbj,1],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],Amp_A_N1[Sbj,2],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      # sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],Amp_B_N1[Sbj,1],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],Amp_B_N1[Sbj,2],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      # 
      # color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","orange","orange","orange","orange")
      # 
      # df = data.frame(sessionA,sessionB,color)
      # ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
      #   ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1"))
      # 
      # 
      # cor(sessionA,sessionB)
      # cor.test(sessionA, sessionB, method = c("spearman"))
      # 
      
      sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2])
      sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2])
      
      
      sessionAiiv =c(MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      sessionBiiv = c(MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      
      #color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange")
      
      #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
      
      color =c("blue","blue","green","green","red","red","orange","orange")
      coloriiv =c("blue","blue","blue","green","green","green","red","red","red","orange","orange","orange")
      
      name = c("AMP_P1_C1","Lat_P1_C1","AMP_P1_C2","Lat_P1_C2","Delt_PN_C1","Lat_N1_C1","Delt_PN_C2","Lat_N1_C2")
      nameiiv = c("MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","MADL_P1_C2","MADA_P1_C2","PTP1_C2","MADL_N1_C1","MADA_N1_C1","PTN1_C1","MADL_N1_C2","MADA_N1_C2","PTN1_C2")
      
      
      df = data.frame(sessionA,sessionB,color)
      plot( ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      df2 = data.frame(sessionAiiv,sessionBiiv,coloriiv)
      plot( ggplot(df2,aes(x=sessionAiiv, y=sessionBiiv)) + geom_point(aes(size= 0.1,colour = coloriiv),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      
      # shapiro.test(sessionA)
      # shapiro.test(sessionB)
      # var.test(sessionA,sessionB)
      
      rho_Vsns= c(rho_Vsns, cor(c(sessionA,sessionAiiv),c(sessionB,sessionBiiv)))
      
      rho = c(rho,cor.test(c(sessionA,sessionAiiv), c(sessionB,sessionBiiv) , method = c("spearman")))
      
      col = c("#898989","#00A100") 
      sp(sessionA,sessionB,name,Sbj,"N",col,Mod[ChooseMod])
      sp(sessionAiiv,sessionBiiv,nameiiv,Sbj,"Y",col,Mod[ChooseMod])
    }
    
  }
  
  
  if (Mod[ChooseMod]=="Asns"|Mod[ChooseMod]=="Asimp"){
    #  Asimp et sns
    if (Mod[ChooseMod]=="Asimp"){
      rho_Asimp = list()
    }else{
      rho_Asns = list()
    }
    rho = list()
    nbsbj = 9
    Sbj = 1
    
    for (Sbj in 1:nbsbj){
      
      Amp_A_N1 = normy((ALAN1$N1.amplA[1:nbsbj,1:2,1] + ALAN1$N1.amplA[1:nbsbj,1:2,2])/2)
      Amp_A_N1 = 1-Amp_A_N1
      Lat_A_N1 = normy((ALAN1$N1.latA[1:nbsbj,1:2,1] + ALAN1$N1.latA[1:nbsbj,1:2,2])/2)
      Amp_A_P1 = normy((ALAP1$P1.amplA[1:nbsbj,1:2,1] + ALAP1$P1.amplA[1:nbsbj,1:2,2])/2)
      Lat_A_P1 = normy((ALAP1$P1.latA[1:nbsbj,1:2,1] + ALAP1$P1.latA[1:nbsbj,1:2,2])/2)
      MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,1:2,1]+MesN1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,1:2,1]+MesN1$MAD.ampl[1:nbsbj,1:2,2])/2)
      MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,1:2,1]+MesP1$MAD.lat[1:nbsbj,1:2,2])/2)
      MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,1:2,1]+MesP1$MAD.ampl[1:nbsbj,1:2,2])/2)
      PTN1_A = normy((PTN1$values[1,1:nbsbj,1:2,1]+PTN1$values[1,1:nbsbj,1:2,2])/2)
      PTP1_A = normy((PTP1$values[1,1:nbsbj,1:2,1]+PTP1$values[1,1:nbsbj,1:2,2])/2)
      
      Amp_B_N1 = normy((ALBN1$N1.amplB[1:nbsbj,1:2,1] + ALBN1$N1.amplB[1:nbsbj,1:2,2])/2)
      Amp_B_N1 = 1-Amp_B_N1
      Lat_B_N1 =normy((ALBN1$N1.latB[1:nbsbj,1:2,1] + ALBN1$N1.latB[1:nbsbj,1:2,2])/2)
      Amp_B_P1 = normy((ALBP1$P1.amplB[1:nbsbj,1:2,1] + ALBP1$P1.amplB[1:nbsbj,1:2,2])/2)
      Lat_B_P1 = normy((ALBP1$P1.latB[1:nbsbj,1:2,1] + ALBP1$P1.latB[1:nbsbj,1:2,2])/2)
      MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),1:2,2])/2)
      PTN1_B = normy((PTN1$values[2,1:nbsbj,1:2,1]+PTN1$values[2,1:nbsbj,1:2,2])/2)
      PTP1_B = normy((PTP1$values[2,1:nbsbj,1:2,1]+PTP1$values[2,1:nbsbj,1:2,2])/2)
      
      # sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      # sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      # 
      # color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange")
      # 
      # df = data.frame(sessionA,sessionB,color)
      # ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
      #   ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1"))
      # 
      # 
      # cor(sessionA,sessionB)
      # cor.test(sessionA, sessionB, method = c("spearman"))
      # 
      
      sessionA= c(Amp_A_P1[Sbj,1],Lat_A_P1[Sbj,1],Amp_A_P1[Sbj,2],Lat_A_P1[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2])
      sessionB= c(Amp_B_P1[Sbj,1],Lat_B_P1[Sbj,1],Amp_B_P1[Sbj,2],Lat_B_P1[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2])
      
      
      sessionAiiv =c(MADlat_A_P1[Sbj,1],MADamp_A_P1[Sbj,1],PTP1_A[Sbj,1],MADlat_A_P1[Sbj,2],MADamp_A_P1[Sbj,2],PTP1_A[Sbj,2],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2])
      sessionBiiv = c(MADlat_B_P1[Sbj,1],MADamp_B_P1[Sbj,1],PTP1_B[Sbj,1],MADlat_B_P1[Sbj,2],MADamp_B_P1[Sbj,2],PTP1_B[Sbj,2],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2])
      
      #color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange")
      
      #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
      
      color =c("blue","blue","green","green","red","red","orange","orange")
      coloriiv =c("blue","blue","blue","green","green","green","red","red","red","orange","orange","orange")
      
      name = c("AMP_P1_C1","Lat_P1_C1","AMP_P1_C2","Lat_P1_C2","AMP_N1_C1","Lat_N1_C1","AMP_N1_C2","Lat_N1_C2")
      nameiiv = c("MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","MADL_P1_C2","MADA_P1_C2","PTP1_C2","MADL_N1_C1","MADA_N1_C1","PTN1_C1","MADL_N1_C2","MADA_N1_C2","PTN1_C2")
      
      
      df = data.frame(sessionA,sessionB,color)
      plot( ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      df2 = data.frame(sessionAiiv,sessionBiiv,coloriiv)
      plot( ggplot(df2,aes(x=sessionAiiv, y=sessionBiiv)) + geom_point(aes(size= 0.1,colour = coloriiv),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      
      # shapiro.test(sessionA)
      # shapiro.test(sessionB)
      # var.test(sessionA,sessionB)
      if (Mod[ChooseMod]=="Asimp"){
        
        rho_Asimp = c(rho_Asimp, cor(c(sessionA,sessionAiiv),c(sessionB,sessionBiiv)))
        
        rho = c(rho,cor.test(c(sessionA,sessionAiiv), c(sessionB,sessionBiiv) , method = c("spearman")))
        
        col = c("#898989","#00AFBB") 
        
      }else{
        rho_Asns = c(rho_Asns, cor(c(sessionA,sessionAiiv),c(sessionB,sessionBiiv)))
        
        rho = c(rho,cor.test(c(sessionA,sessionAiiv), c(sessionB,sessionBiiv) , method = c("spearman")))
        
        col = c("#898989","#3300CC") 
        
      }
      
      sp(sessionA,sessionB,name,Sbj,"N",col,Mod[ChooseMod])
      sp(sessionAiiv,sessionBiiv,nameiiv,Sbj,"Y",col,Mod[ChooseMod])
    }
  }
 
  
  if (Mod[ChooseMod]=="Tsimp"){
    rho_Tsimp = list()
    nbsbj = 11
    Sbj = 1
    rho = list()

    
    for (Sbj in 1:nbsbj){
      Amp_A_P50 = normy(ALAP50$P50.amplA[1:nbsbj,1:2])
      Lat_A_P50 = normy(ALAP50$P50.latA[1:nbsbj,1:2])
      Amp_A_N1 = normy(ALAN1$N70.amplA[1:nbsbj,1:2])
      Amp_A_N1 = 1-Amp_A_N1
      Lat_A_N1 = normy(ALAN1$N70.latA[1:nbsbj,1:2])
      Amp_A_P100 = normy(ALAP100$P100.amplA[1:nbsbj,1:2])
      Lat_A_P100 = normy(ALAP100$P100.latA[1:nbsbj,1:2])
      Amp_A_P200 = normy(ALAP200$P200.amplA[1:nbsbj,1:2])
      Lat_A_P200 = normy(ALAP200$P200.latA[1:nbsbj,1:2])
      
      
      MADlat_A_P50 = normy(MesP50$MAD.lat[1:nbsbj,1:2])
      MADamp_A_P50 =normy(MesP50$MAD.ampl[1:nbsbj,1:2])
      MADlat_A_N1 = normy(MesN70$MAD.lat[1:nbsbj,1:2])
      MADamp_A_N1 = normy(MesN70$MAD.ampl[1:nbsbj,1:2])
      MADlat_A_P100 = normy(MesP100$MAD.lat[1:nbsbj,1:2])
      MADamp_A_P100 =normy(MesP100$MAD.ampl[1:nbsbj,1:2])
      MADlat_A_P200 = normy(MesP200$MAD.lat[1:nbsbj,1:2])
      MADamp_A_P200 =normy(MesP200$MAD.ampl[1:nbsbj,1:2])
      
      
      PTP50_A = normy(PTP50$values[1,1:nbsbj,1:2])
      PTN1_A = normy(PTN70$values[1,1:nbsbj,1:2])
      PTP100_A = normy(PTP100$values[1,1:nbsbj,1:2])
      PTP200_A = normy(PTP200$values[1,1:nbsbj,1:2])
      
      Amp_B_P50 = normy(ALBP50$P50.amplB[1:nbsbj,1:2] )
      Lat_B_P50 = normy(ALBP50$P50.latB[1:nbsbj,1:2])
      Amp_B_N1 = normy(ALBN1$N70.amplB[1:nbsbj,1:2])
      Amp_B_N1 = 1-Amp_B_N1
      Lat_B_N1 = normy(ALBN1$N70.latB[1:nbsbj,1:2] )
      Amp_B_P100 = normy(ALBP100$P100.amplB[1:nbsbj,1:2])
      Lat_B_P100 = normy(ALBP100$P100.latB[1:nbsbj,1:2])
      Amp_B_P200 = normy(ALBP200$P200.amplB[1:nbsbj,1:2])
      Lat_B_P200 = normy(ALBP200$P200.latB[1:nbsbj,1:2])
      
      
      MADlat_B_P50 = normy(MesP50$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADamp_B_P50 =normy(MesP50$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADlat_B_N1 = normy(MesN70$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADamp_B_N1 = normy(MesN70$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADlat_B_P100 = normy(MesP100$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADamp_B_P100 =normy(MesP100$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADlat_B_P200 = normy(MesP200$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      MADamp_B_P200 =normy(MesP200$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),1:2])
      
      
      PTP50_B = normy(PTP50$values[2,1:nbsbj,1:2])
      PTN1_B = normy(PTN70$values[2,1:nbsbj,1:2])
      PTP100_B = normy(PTP100$values[2,1:nbsbj,1:2])
      PTP200_B = normy(PTP200$values[2,1:nbsbj,1:2])
      
      
      
      # sessionA= c(Amp_A_P50[Sbj,1],Lat_A_P50[Sbj,1],MADlat_A_P50[Sbj,1],MADamp_A_P50[Sbj,1],PTP50_A[Sbj,1],Amp_A_P50[Sbj,2],Lat_A_P50[Sbj,2],MADlat_A_P50[Sbj,2],MADamp_A_P50[Sbj,2],PTP50_A[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2],Amp_A_P100[Sbj,1],Lat_A_P100[Sbj,1],MADlat_A_P100[Sbj,1],MADamp_A_P100[Sbj,1],PTP100_A[Sbj,1],Amp_A_P100[Sbj,2],Lat_A_P100[Sbj,2],MADlat_A_P100[Sbj,2],MADamp_A_P100[Sbj,2],PTP100_A[Sbj,2],Amp_A_P200[Sbj,1],Lat_A_P200[Sbj,1],MADlat_A_P200[Sbj,1],MADamp_A_P200[Sbj,1],PTP200_A[Sbj,1],Amp_A_P200[Sbj,2],Lat_A_P200[Sbj,2],MADlat_A_P200[Sbj,2],MADamp_A_P200[Sbj,2],PTP200_A[Sbj,2])
      # sessionB= c(Amp_B_P50[Sbj,1],Lat_B_P50[Sbj,1],MADlat_B_P50[Sbj,1],MADamp_B_P50[Sbj,1],PTP50_B[Sbj,1],Amp_B_P50[Sbj,2],Lat_B_P50[Sbj,2],MADlat_B_P50[Sbj,2],MADamp_B_P50[Sbj,2],PTP50_B[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2],Amp_B_P100[Sbj,1],Lat_B_P100[Sbj,1],MADlat_B_P100[Sbj,1],MADamp_B_P100[Sbj,1],PTP100_B[Sbj,1],Amp_B_P100[Sbj,2],Lat_B_P100[Sbj,2],MADlat_B_P100[Sbj,2],MADamp_B_P100[Sbj,2],PTP100_B[Sbj,2], Amp_B_P200[Sbj,1],Lat_B_P200[Sbj,1],MADlat_B_P200[Sbj,1],MADamp_B_P200[Sbj,1],PTP200_B[Sbj,1],Amp_B_P200[Sbj,2],Lat_B_P200[Sbj,2],MADlat_B_P200[Sbj,2],MADamp_B_P200[Sbj,2],PTP200_B[Sbj,2])
      # 
      # 
      # color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange","purple","purple","purple","purple","purple","pink","pink","pink","pink","pink","black","black","black","black","black","grey","grey","grey","grey","grey")
      # 
      # #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
      # 
      # 
      # df = data.frame(sessionA,sessionB,color)
      # ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
      #   ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P50 + N70 + P100 + P200"))
      # 
      # 
      # 
      # shapiro.test(sessionA)
      # shapiro.test(sessionB)
      # var.test(sessionA,sessionB)
      # 
      # cor(sessionA,sessionB)
      # cor.test(sessionA, sessionB, method = c("spearman"))
      # 
      
      sessionA= c(Amp_A_P50[Sbj,1],Lat_A_P50[Sbj,1],Amp_A_P50[Sbj,2],Lat_A_P50[Sbj,2],Amp_A_N1[Sbj,1],Lat_A_N1[Sbj,1],Amp_A_N1[Sbj,2],Lat_A_N1[Sbj,2],Amp_A_P100[Sbj,1],Lat_A_P100[Sbj,1],Amp_A_P100[Sbj,2],Lat_A_P100[Sbj,2],Amp_A_P200[Sbj,1],Lat_A_P200[Sbj,1],Amp_A_P200[Sbj,2],Lat_A_P200[Sbj,2])
      sessionB= c(Amp_B_P50[Sbj,1],Lat_B_P50[Sbj,1],Amp_B_P50[Sbj,2],Lat_B_P50[Sbj,2],Amp_B_N1[Sbj,1],Lat_B_N1[Sbj,1],Amp_B_N1[Sbj,2],Lat_B_N1[Sbj,2],Amp_B_P100[Sbj,1],Lat_A_P100[Sbj,1],Amp_B_P100[Sbj,2],Lat_B_P100[Sbj,2],Amp_B_P200[Sbj,1],Lat_B_P200[Sbj,1],Amp_B_P200[Sbj,2],Lat_B_P200[Sbj,2])
      
      
      sessionAiiv =c(MADlat_A_P50[Sbj,1],MADamp_A_P50[Sbj,1],PTP50_A[Sbj,1],MADlat_A_P50[Sbj,2],MADamp_A_P50[Sbj,2],PTP50_A[Sbj,2],MADlat_A_N1[Sbj,1],MADamp_A_N1[Sbj,1],PTN1_A[Sbj,1],MADlat_A_N1[Sbj,2],MADamp_A_N1[Sbj,2],PTN1_A[Sbj,2],MADlat_A_P100[Sbj,1],MADamp_A_P100[Sbj,1],PTP100_A[Sbj,1],MADlat_A_P100[Sbj,2],MADamp_A_P100[Sbj,2],PTP100_A[Sbj,2],MADlat_A_P200[Sbj,1],MADamp_A_P200[Sbj,1],PTP200_A[Sbj,1],MADlat_A_P200[Sbj,2],MADamp_A_P200[Sbj,2],PTP200_A[Sbj,2])
      sessionBiiv = c(MADlat_B_P50[Sbj,1],MADamp_B_P50[Sbj,1],PTP50_B[Sbj,1],MADlat_B_P50[Sbj,2],MADamp_B_P50[Sbj,2],PTP50_B[Sbj,2],MADlat_B_N1[Sbj,1],MADamp_B_N1[Sbj,1],PTN1_B[Sbj,1],MADlat_B_N1[Sbj,2],MADamp_B_N1[Sbj,2],PTN1_B[Sbj,2],MADlat_A_P100[Sbj,1],MADamp_A_P100[Sbj,1],PTP100_A[Sbj,1],MADlat_A_P100[Sbj,2],MADamp_A_P100[Sbj,2],PTP100_A[Sbj,2],MADlat_A_P200[Sbj,1],MADamp_A_P200[Sbj,1],PTP200_A[Sbj,1],MADlat_A_P200[Sbj,2],MADamp_A_P200[Sbj,2],PTP200_A[Sbj,2])
      
      #color =c("blue","blue","blue","blue","blue","green","green","green","green","green","red","red","red","red","red","orange","orange","orange","orange","orange")
      
      #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
      
      color =c("blue","blue","green","green","red","red","orange","orange","purple","purple","pink","pink","black","black","gray","gray")
      coloriiv =c("blue","blue","blue","green","green","green","red","red","red","orange","orange","orange","purple","purple","purple","pink","pink","pink","black","black","black","gray","gray","gray")
      
      name = c("AMP_P50_C1","Lat_P50_C1","AMP_P50_C2","Lat_P50_C2","AMP_N1_C1","Lat_N1_C1","AMP_N1_C2","Lat_N1_C2","AMP_P100_C1","Lat_P100_C1","AMP_P100_C2","Lat_P100_C2","AMP_P200_C1","Lat_P200_C1","AMP_P200_C2","Lat_P200_C2")
      nameiiv = c("MADL_P50_C1","MADA_P50_C1","PTP50_C1","MADL_P50_C2","MADA_P50_C2","PTP50_C2","MADL_N1_C1","MADA_N1_C1","PTN1_C1","MADL_N1_C2","MADA_N1_C2","PTN1_C2","MADL_P100_C1","MADA_P100_C1","PTP100_C1","MADL_P100_C2","MADA_P100_C2","PTP100_C2","MADL_P200_C1","MADA_P200_C1","PTP200_C1","MADL_P200_C2","MADA_P200_C2","PTP200_C2")
      
      
      df = data.frame(sessionA,sessionB,color)
      plot( ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      df2 = data.frame(sessionAiiv,sessionBiiv,coloriiv)
      plot( ggplot(df2,aes(x=sessionAiiv, y=sessionBiiv)) + geom_point(aes(size= 0.1,colour = coloriiv),show.legend = FALSE) +scale_color_identity() +
              ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/comp : P1 + N1")))
      
      
      # shapiro.test(sessionA)
      # shapiro.test(sessionB)
      # var.test(sessionA,sessionB)
      
      rho_Tsimp = c(rho_Tsimp, cor(c(sessionA,sessionAiiv),c(sessionB,sessionBiiv)))
      
      rho = c(rho,cor.test(c(sessionA,sessionAiiv), c(sessionB,sessionBiiv) , method = c("spearman")))
      
      col = c("#898989","#FC4E07") 
      sp(sessionA,sessionB,name,Sbj,"N",col,Mod[ChooseMod])
      sp(sessionAiiv,sessionBiiv,nameiiv,Sbj,"Y",col,Mod[ChooseMod])
    }
    
    
  }
}

if (typ[t]=="mean_e_by_cond"){
 
  
  P1featuresA1= list()
  P1featuresB1 = list()
  N1FeaturesA1 = list()
  N1FeaturesB1 = list()
  
  P1featuresA2= list()
  P1featuresB2 = list()
  N1FeaturesA2 = list()
  N1FeaturesB2 = list()
  
  
  P50featuresA1= list()
  P50featuresB1 = list()
  P50featuresA2= list()
  P50featuresB2 = list()
  
  P100featuresA1= list()
  P100featuresB1 = list()
  P100featuresA2= list()
  P100featuresB2 = list()
  
  P200featuresA1= list()
  P200featuresB1 = list()
  P200featuresA2= list()
  P200featuresB2 = list()
  
  if (Mod[ChooseMod]=="Vsimp"){
  
    rho_Vsimp = list()
    nbsbj = 10
    Sbj = 1
    for (Sbj in 1:nbsbj){
      cond =1
      for (cond in 1:2)
      {
        Amp_A_N1 = normy((ALAN1$N170.ampl[1:nbsbj,cond,1] + ALAN1$N170.ampl[1:nbsbj,cond,2])/2)
        Amp_A_N1 = 1-Amp_A_N1
        Lat_A_N1 = normy((ALAN1$N170.lat[1:nbsbj,cond,1] + ALAN1$N170.lat[1:nbsbj,cond,2])/2)
        Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,cond,1] + ALAP1$P100.amplA[1:nbsbj,cond,2])/2)
        Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,cond,1] + ALAP1$P100.latA[1:nbsbj,cond,2])/2)
        MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
        MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
        PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
        PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
        
        Amp_B_N1 = normy((ALBN1$N170.ampl[1:nbsbj,cond,1] + ALBN1$N170.ampl[1:nbsbj,cond,2])/2)
        Amp_B_N1 = 1-Amp_B_N1
        Lat_B_N1 =normy((ALBN1$N170.lat[1:nbsbj,cond,1] + ALBN1$N170.lat[1:nbsbj,cond,2])/2)
        Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,cond,1] + ALBP1$P100.amplB[1:nbsbj,cond,2])/2)
        Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,cond,1] + ALBP1$P100.latB[1:nbsbj,cond,2])/2)
        MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
        PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
        
        sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj],Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj],Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue","red","red","red","red","red") 
          
          P1featuresA1= c(P1featuresA1,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB1= c(P1featuresB1,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA1 = c(N1FeaturesA1,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB1 = c(N1FeaturesB1,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
        }else {
          color=c("green","green","green","green","green","orange","orange","orange","orange","orange") 
          
          P1featuresA2= c(P1featuresA2,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB2= c(P1featuresB2,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA2 = c(N1FeaturesA2,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB2 = c(N1FeaturesB2,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        }
        
        
        
        if (opt == 1){
          df = data.frame(sessionA,sessionB,color)
          plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
                 ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1 + N1")))
          
          rho_Vsimp= c(rho_Vsimp, cor(sessionA,sessionB))
          
          name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1","AMP_N1","Lat_N1","MADL_N1","MADA_N1","PTN1")
          
          col = c("#898989","#E7B800") 
          
          title = paste(Sbj,"_cond_",cond, sep="")
          sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        }  
          
       
        
     
       
      }  
    }
  }
  
  if (Mod[ChooseMod]=="Vsns"){
    
    rho_Vsns = list()
    rho = list()
    nbsbj = 11
    Sbj = 1
    
    for (Sbj in 1:nbsbj){
      
      cond =1
      for (cond in 1:2)
      {
        
        Amp_A_N1 = normy((ALAN1$DeltaP1N170PO78A[1:nbsbj,cond,1] + ALAN1$DeltaP1N170PO78A[1:nbsbj,cond,2])/2)
        Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,cond,1] + ALAP1$P100.amplA[1:nbsbj,cond,2])/2)
        Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,cond,1] + ALAP1$P100.latA[1:nbsbj,cond,2])/2)
        Lat_A_N1 = normy((MesN1$Avg.P1.all.Lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
        MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$Avg.P1.all.Lat[1:nbsbj,cond,2])/2)
        MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
        MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
        PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
        PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
        
        
        
        Amp_B_N1 = normy((ALBN1$DeltaP1N170PO78B[1:nbsbj,cond,1] + ALBN1$DeltaP1N170PO78B[1:nbsbj,cond,2])/2)
        Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,cond,1] + ALBP1$P100.amplB[1:nbsbj,cond,2])/2)
        Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,cond,1] + ALBP1$P100.latB[1:nbsbj,cond,2])/2)
        Lat_B_N1 = normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_N1 =normy((MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
        PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
        
        sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj],Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj],Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue","red","red","red","red","red") 
          
          P1featuresA1= c(P1featuresA1,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB1= c(P1featuresB1,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA1 = c(N1FeaturesA1,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB1 = c(N1FeaturesB1,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
        }else {
          color=c("green","green","green","green","green","orange","orange","orange","orange","orange") 
          
          P1featuresA2= c(P1featuresA2,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB2= c(P1featuresB2,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA2 = c(N1FeaturesA2,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB2 = c(N1FeaturesB2,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        }
        
        if (opt == 1){
        df = data.frame(sessionA,sessionB,color)
        plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1 + N1")))
        
        rho_Vsns = c(rho_Vsns, cor(sessionA,sessionB))
        
        
        name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1","AMP_DPN","Lat_N1","MADL_N1","MADA_N1","PTN1")
        
        col = c("#898989","#00A100") 
        
        title = paste(Sbj,"_cond_",cond, sep="")
        sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        }
      }
    }
  }
  
  if (Mod[ChooseMod]=="Asimp"|Mod[ChooseMod]=="Asns"){
    
    if (Mod[ChooseMod]=="Asimp"){
    rho_Asimp = list()
    }else{
      rho_Asns = list()
    }
    nbsbj = 9
    Sbj = 1
    for (Sbj in 1:nbsbj){
      cond =1
      for (cond in 1:2)
      {
        
        Amp_A_N1 = normy((ALAN1$N1.amplA[1:nbsbj,cond,1] + ALAN1$N1.amplA[1:nbsbj,cond,2])/2)
        Amp_A_N1 = 1-Amp_A_N1
        Lat_A_N1 = normy((ALAN1$N1.latA[1:nbsbj,cond,1] + ALAN1$N1.latA[1:nbsbj,cond,2])/2)
        Amp_A_P1 = normy((ALAP1$P1.amplA[1:nbsbj,cond,1] + ALAP1$P1.amplA[1:nbsbj,cond,2])/2)
        Lat_A_P1 = normy((ALAP1$P1.latA[1:nbsbj,cond,1] + ALAP1$P1.latA[1:nbsbj,cond,2])/2)
        MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
        MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
        PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
        PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
        
        Amp_B_N1 = normy((ALBN1$N1.amplB[1:nbsbj,cond,1] + ALBN1$N1.amplB[1:nbsbj,cond,2])/2)
        Amp_B_N1 = 1-Amp_B_N1
        Lat_B_N1 =normy((ALBN1$N1.latB[1:nbsbj,cond,1] + ALBN1$N1.latB[1:nbsbj,cond,2])/2)
        Amp_B_P1 = normy((ALBP1$P1.amplB[1:nbsbj,cond,1] + ALBP1$P1.amplB[1:nbsbj,cond,2])/2)
        Lat_B_P1 = normy((ALBP1$P1.latB[1:nbsbj,cond,1] + ALBP1$P1.latB[1:nbsbj,cond,2])/2)
        MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
        PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
        
        sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj],Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj],Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue","red","red","red","red","red") 
          
          P1featuresA1= c(P1featuresA1,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB1= c(P1featuresB1,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA1 = c(N1FeaturesA1,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB1 = c(N1FeaturesB1,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
        }else {
          color=c("green","green","green","green","green","orange","orange","orange","orange","orange") 
          
          P1featuresA2= c(P1featuresA2,Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          P1featuresB2= c(P1featuresB2,Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          N1FeaturesA2 = c(N1FeaturesA2,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB2 = c(N1FeaturesB2,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        }
        
        if (opt == 1){
          
          df = data.frame(sessionA,sessionB,color)
          ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
            ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1 + N1"))
          
          
          
          
          if (Mod[ChooseMod]=="Asimp"){
            
            col = c("#898989","#00AFBB")
            rho_Asimp = c(rho_Asimp ,cor(sessionA,sessionB))
            
          }else{
            col = c("#898989","#3300CC") 
            rho_Asns = c(rho_Asns ,cor(sessionA,sessionB))
            
          }
          
          name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1","AMP_N1","Lat_N1","MADL_N1","MADA_N1","PTN1")
          
          title = paste(Sbj,"_cond_",cond, sep="")
          sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        }
        
        
      }
    }
    
  }
  
  if (Mod[ChooseMod]=="Tsimp"){
    
    rho_Tsimp = list()
    nbsbj = 11
    Sbj = 1
    for (Sbj in 1:nbsbj){
      cond =1
      for (cond in 1:2)
      {
        Amp_A_P50 = normy(ALAP50$P50.amplA[1:nbsbj,cond])
        Lat_A_P50 = normy(ALAP50$P50.latA[1:nbsbj,cond])
        Amp_A_N1 = normy(ALAN1$N70.amplA[1:nbsbj,cond])
        Amp_A_N1 = 1-Amp_A_N1
        Lat_A_N1 = normy(ALAN1$N70.latA[1:nbsbj,cond])
        Amp_A_P100 = normy(ALAP100$P100.amplA[1:nbsbj,cond])
        Lat_A_P100 = normy(ALAP100$P100.latA[1:nbsbj,cond])
        Amp_A_P200 = normy(ALAP200$P200.amplA[1:nbsbj,cond])
        Lat_A_P200 = normy(ALAP200$P200.latA[1:nbsbj,cond])
        
        
        MADlat_A_P50 = normy(MesP50$MAD.lat[1:nbsbj,cond])
        MADamp_A_P50 =normy(MesP50$MAD.ampl[1:nbsbj,cond])
        MADlat_A_N1 = normy(MesN70$MAD.lat[1:nbsbj,cond])
        MADamp_A_N1 = normy(MesN70$MAD.ampl[1:nbsbj,cond])
        MADlat_A_P100 = normy(MesP100$MAD.lat[1:nbsbj,cond])
        MADamp_A_P100 =normy(MesP100$MAD.ampl[1:nbsbj,cond])
        MADlat_A_P200 = normy(MesP200$MAD.lat[1:nbsbj,cond])
        MADamp_A_P200 =normy(MesP200$MAD.ampl[1:nbsbj,cond])
        
        
        PTP50_A = normy(PTP50$values[1,1:nbsbj,cond])
        PTN1_A = normy(PTN70$values[1,1:nbsbj,cond])
        PTP100_A = normy(PTP100$values[1,1:nbsbj,cond])
        PTP200_A = normy(PTP200$values[1,1:nbsbj,cond])
        
        Amp_B_P50 = normy(ALBP50$P50.amplB[1:nbsbj,cond] )
        Lat_B_P50 = normy(ALBP50$P50.latB[1:nbsbj,cond])
        Amp_B_N1 = normy(ALBN1$N70.amplB[1:nbsbj,cond])
        Amp_B_N1 = 1-Amp_B_N1
        Lat_B_N1 = normy(ALBN1$N70.latB[1:nbsbj,cond] )
        Amp_B_P100 = normy(ALBP100$P100.amplB[1:nbsbj,cond])
        Lat_B_P100 = normy(ALBP100$P100.latB[1:nbsbj,cond])
        Amp_B_P200 = normy(ALBP200$P200.amplB[1:nbsbj,cond])
        Lat_B_P200 = normy(ALBP200$P200.latB[1:nbsbj,cond])
        
        
        MADlat_B_P50 = normy(MesP50$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P50 =normy(MesP50$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_N1 = normy(MesN70$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_N1 = normy(MesN70$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_P100 = normy(MesP100$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P100 =normy(MesP100$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_P200 = normy(MesP200$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P200 =normy(MesP200$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        
        
        PTP50_B = normy(PTP50$values[2,1:nbsbj,cond])
        PTN1_B = normy(PTN70$values[2,1:nbsbj,cond])
        PTP100_B = normy(PTP100$values[2,1:nbsbj,cond])
        PTP200_B = normy(PTP200$values[2,1:nbsbj,cond])
        
        
        
        sessionA= c(Amp_A_P50[Sbj],Lat_A_P50[Sbj],MADlat_A_P50[Sbj],MADamp_A_P50[Sbj],PTP50_A[Sbj],Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj],Amp_A_P100[Sbj],Lat_A_P100[Sbj],MADlat_A_P100[Sbj],MADamp_A_P100[Sbj],PTP100_A[Sbj],Amp_A_P200[Sbj],Lat_A_P200[Sbj],MADlat_A_P200[Sbj],MADamp_A_P200[Sbj],PTP200_A[Sbj])
        sessionB= c(Amp_B_P50[Sbj],Lat_B_P50[Sbj],MADlat_B_P50[Sbj],MADamp_B_P50[Sbj],PTP50_B[Sbj],Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj],Amp_B_P100[Sbj],Lat_B_P100[Sbj],MADlat_B_P100[Sbj],MADamp_B_P100[Sbj],PTP100_B[Sbj],Amp_B_P200[Sbj],Lat_B_P200[Sbj],MADlat_B_P200[Sbj],MADamp_B_P200[Sbj],PTP200_B[Sbj])
        
        
        
        if (cond ==1){
          
          color =c("blue","blue","blue","blue","blue","red","red","red","red","red","purple","purple","purple","purple","purple","black","black","black","black","black") 
          
          P50featuresA1= c(P50featuresA1,Amp_A_P50[Sbj],Lat_A_P50[Sbj],MADlat_A_P50[Sbj],MADamp_A_P50[Sbj],PTP50_A[Sbj])
          P50featuresB1= c(P50featuresB1,Amp_B_P50[Sbj],Lat_B_P50[Sbj],MADlat_B_P50[Sbj],MADamp_B_P50[Sbj],PTP50_B[Sbj])
          
          P200featuresA1= c(P200featuresA1,Amp_A_P200[Sbj],Lat_A_P200[Sbj],MADlat_A_P200[Sbj],MADamp_A_P200[Sbj],PTP200_A[Sbj])
          P200featuresB1= c(P200featuresB1,Amp_B_P200[Sbj],Lat_B_P200[Sbj],MADlat_B_P200[Sbj],MADamp_B_P200[Sbj],PTP200_B[Sbj])
          
          P100featuresA1= c(P100featuresA1,Amp_A_P100[Sbj],Lat_A_P100[Sbj],MADlat_A_P100[Sbj],MADamp_A_P100[Sbj],PTP100_A[Sbj])
          P100featuresB1= c(P100featuresB1,Amp_B_P100[Sbj],Lat_B_P100[Sbj],MADlat_B_P100[Sbj],MADamp_B_P100[Sbj],PTP100_B[Sbj])
          N1FeaturesA1 = c(N1FeaturesA1,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB1 = c(N1FeaturesB1,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
        }else {
          color=c("green","green","green","green","green","orange","orange","orange","orange","orange","pink","pink","pink","pink","pink","grey","grey","grey","grey","grey") 
          
          P50featuresA2= c(P50featuresA2,Amp_A_P50[Sbj],Lat_A_P50[Sbj],MADlat_A_P50[Sbj],MADamp_A_P50[Sbj],PTP50_A[Sbj])
          P50featuresB2= c(P50featuresB2,Amp_B_P50[Sbj],Lat_B_P50[Sbj],MADlat_B_P50[Sbj],MADamp_B_P50[Sbj],PTP50_B[Sbj])
          
          P200featuresA2= c(P200featuresA2,Amp_A_P200[Sbj],Lat_A_P200[Sbj],MADlat_A_P200[Sbj],MADamp_A_P200[Sbj],PTP200_A[Sbj])
          P200featuresB2= c(P200featuresB2,Amp_B_P200[Sbj],Lat_B_P200[Sbj],MADlat_B_P200[Sbj],MADamp_B_P200[Sbj],PTP200_B[Sbj])
          
          P100featuresA2= c(P100featuresA2,Amp_A_P100[Sbj],Lat_A_P100[Sbj],MADlat_A_P100[Sbj],MADamp_A_P100[Sbj],PTP100_A[Sbj])
          P100featuresB2= c(P100featuresB2,Amp_B_P100[Sbj],Lat_B_P100[Sbj],MADlat_B_P100[Sbj],MADamp_B_P100[Sbj],PTP100_B[Sbj])
         
           N1FeaturesA2 = c(N1FeaturesA2,Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          N1FeaturesB2 = c(N1FeaturesB2,Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
        }
        
        
        
        if (opt == 1){
          
          #name = c("AMP_P1_C1","Lat_P1_C1","MADlat_P1_C1","MADampl_P1_C1","PTP1_C1","AMP_P1_C2","Lat_P1_C2","MADlat_P1_C2","MADampl_P1_C2","PTP1_C2","AMP_N1_C1","Lat_N1_C1","MADlat_N1_C1","MADampl_N1_C1","PTN1_C1","AMP_N1_C2","Lat_N1_C2","MADlat_N1_C2","MADampl_N1_C2","PTN1_C2")
          
          
          df = data.frame(sessionA,sessionB,color)
          plot( ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
                  ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P50 + N70 + P100 + P200")))
          
          
          name = c("AMP_P50_C1","Lat_P50_C1","MADL_P50_C1","MADA_P50_C1","PTP50_C1","AMP_N1_C1","Lat_N1_C1","MADL_N1_C1","MADA_N1_C1","PTN1_C1","AMP_P100_C1","Lat_P100_C1","MADL_P100_C1","MADA_P100_C1","PTP100_C1","AMP_P200_C1","Lat_P200_C1","MADL_P200_C1","MADA_P200_C1","PTP200_C1")
          
          
          rho_Tsimp= c(rho_Tsimp, cor(sessionA,sessionB))
          
          
          col = c("#898989","#FC4E07")
          
          title = paste(Sbj,"_cond_",cond, sep="")
          sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        }
      }
    }
    
  }
}

if (typ[t]=="mean_e_bycond_bycomp"){
  
  if ( Mod[ChooseMod]=="Vsimp"){
    rho_Vsimp_P1 = list()
    rho_Vsimp_N1 = list()
    MA = list()
    MB = list()
    nbsbj = 10
    Sbj = 1
    
    #for (Sbj in 1:nbsbj){
    for (Sbj in 1:1){
      cond =1
      for (cond in 1:2){
          
          Amp_A_N1 = normy((ALAN1$N170.ampl[1:nbsbj,cond,1] + ALAN1$N170.ampl[1:nbsbj,cond,2])/2)
          Amp_A_N1 = 1-Amp_A_N1
          Lat_A_N1 = normy((ALAN1$N170.lat[1:nbsbj,cond,1] + ALAN1$N170.lat[1:nbsbj,cond,2])/2)
          Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,cond,1] + ALAP1$P100.amplA[1:nbsbj,cond,2])/2)
          Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,cond,1] + ALAP1$P100.latA[1:nbsbj,cond,2])/2)
          MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
          MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
          MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
          MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
          PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
          PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
          
          Amp_B_N1 = normy((ALBN1$N170.ampl[1:nbsbj,cond,1] + ALBN1$N170.ampl[1:nbsbj,cond,2])/2)
          Amp_B_N1 = 1-Amp_B_N1
          Lat_B_N1 =normy((ALBN1$N170.lat[1:nbsbj,cond,1] + ALBN1$N170.lat[1:nbsbj,cond,2])/2)
          Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,cond,1] + ALBP1$P100.amplB[1:nbsbj,cond,2])/2)
          Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,cond,1] + ALBP1$P100.latB[1:nbsbj,cond,2])/2)
          MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
          MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
          MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
          MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
          PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
          PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
          
          sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
          sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
          
          if (cond ==1){
            color =c("blue","blue","blue","blue","blue") 
          }else {
            color=c("green","green","green","green","green") 
          }
          
          df = data.frame(sessionA,sessionB,color)
          plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
                 ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1")))
          
          rho_Vsimp_P1= c(rho_Vsimp_P1, cor(sessionA,sessionB))
          MA = c(MA,sessionA)
          MB = c(MB,sessionB)
          
          name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1")
          
          col = c("#898989","#E7B800") 
          
          title = paste(Sbj,"_cond_",cond,"_P1", sep="")
          sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
          
          
          sessionAN1= c(Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
          sessionBN1= c(Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
          
          if (cond ==1){
            color =c("red","red","red","red","red")
          }else {
            color=c("orange","orange","orange","orange","orange")
          }
          
          MA = c(MA,sessionAN1)
          MB = c(MB,sessionBN1)
          
          df = data.frame(sessionAN1,sessionBN1,color)
          plot(ggplot(df,aes(x=sessionAN1, y=sessionBN1)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
                 ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : N1")))
          
          rho_Vsimp_N1= c(rho_Vsimp_N1, cor(sessionAN1,sessionBN1))
          
          name = c("AMP_N1","Lat_N1","MADL_N1","MADA_N1","PTN1")
          
          col = c("#898989","#E7B800") 
          
          title = paste(Sbj,"_cond_",cond,"_N1", sep="")
          sp(sessionAN1,sessionBN1,name,title,"N",col,Mod[ChooseMod])
          
      }
    }
  }
  
  if (Mod[ChooseMod]=="Vsns"){
    
    rho_Vsns_P1 = list()
    rho_Vsns_N1 = list()
    nbsbj = 11
    Sbj = 1
    
    #for (Sbj in 1:nbsbj){
    for (Sbj in 1:1){
      cond =1
      for (cond in 1:2)
      {
        
        Amp_A_N1 = normy((ALAN1$DeltaP1N170PO78A[1:nbsbj,cond,1] + ALAN1$DeltaP1N170PO78A[1:nbsbj,cond,2])/2)
        Amp_A_P1 = normy((ALAP1$P100.amplA[1:nbsbj,cond,1] + ALAP1$P100.amplA[1:nbsbj,cond,2])/2)
        Lat_A_P1 = normy((ALAP1$P100.latA[1:nbsbj,cond,1] + ALAP1$P100.latA[1:nbsbj,cond,2])/2)
        Lat_A_N1 = normy((MesN1$Avg.P1.all.Lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
        MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$Avg.P1.all.Lat[1:nbsbj,cond,2])/2)
        MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
        MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
        PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
        PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
        
        
        
        Amp_B_N1 = normy((ALBN1$DeltaP1N170PO78B[1:nbsbj,cond,1] + ALBN1$DeltaP1N170PO78B[1:nbsbj,cond,2])/2)
        Amp_B_P1 = normy((ALBP1$P100.amplB[1:nbsbj,cond,1] + ALBP1$P100.amplB[1:nbsbj,cond,2])/2)
        Lat_B_P1 = normy((ALBP1$P100.latB[1:nbsbj,cond,1] + ALBP1$P100.latB[1:nbsbj,cond,2])/2)
        Lat_B_N1 = normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_N1 =normy((MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$Avg.P1.all.Lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
        PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
        
        
        
        sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
        sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue") 
        }else {
          color=c("green","green","green","green","green") 
        }
        
        df = data.frame(sessionA,sessionB,color)
        plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1")))
        
        rho_Vsns_P1 = c(rho_Vsns_P1, cor(sessionA,sessionB))
        MA = c(MA,sessionA)
        MB = c(MB,sessionB)
        
        
        name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1")
rowal
        
        col = c("#898989","#00A100") 
        
        title = paste(Sbj,"_cond_",cond,"_P1", sep="")
        #sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        
        
        sessionAN1= c(Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionBN1= c(Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        if (cond ==1){
          color =c("red","red","red","red","red")
        }else {
          color=c("orange","orange","orange","orange","orange")
        }
        
        
        df = data.frame(sessionAN1,sessionBN1,color)
        plot(ggplot(df,aes(x=sessionAN1, y=sessionBN1)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : N1")))
        
        rho_Vsns_N1= c(rho_Vsns_N1, cor(sessionAN1,sessionBN1))
        MA = c(MA,sessionAN1)
        MB = c(MB,sessionBN1)
        
        name = c("AMP_N1","Lat_N1","MADL_N1","MADA_N1","PTN1")
        
        
        col = c("#898989","#00A100") 
        
        title = paste(Sbj,"_cond_",cond,"_N1", sep="")
        #sp(sessionAN1,sessionBN1,name,title,"N",col,Mod[ChooseMod])
        
      }
    }
  }
  
  if (Mod[ChooseMod]=="Asimp"|Mod[ChooseMod]=="Asns"){
    
    if (Mod[ChooseMod]=="Asimp"){
      rho_Asimp_P1 = list()
      rho_Asimp_N1 = list()
    }else{
      rho_Asns_P1 = list()
      rho_Asns_N1 = list()
    }
    nbsbj = 9
    Sbj = 1
    #for (Sbj in 1:nbsbj){
    for (Sbj in 1:1){
      cond =1
      for (cond in 1:2)
      {
        
        Amp_A_N1 = normy((ALAN1$N1.amplA[1:nbsbj,cond,1] + ALAN1$N1.amplA[1:nbsbj,cond,2])/2)
        Amp_A_N1 = 1-Amp_A_N1
        Lat_A_N1 = normy((ALAN1$N1.latA[1:nbsbj,cond,1] + ALAN1$N1.latA[1:nbsbj,cond,2])/2)
        Amp_A_P1 = normy((ALAP1$P1.amplA[1:nbsbj,cond,1] + ALAP1$P1.amplA[1:nbsbj,cond,2])/2)
        Lat_A_P1 = normy((ALAP1$P1.latA[1:nbsbj,cond,1] + ALAP1$P1.latA[1:nbsbj,cond,2])/2)
        MADlat_A_N1 = normy((MesN1$MAD.lat[1:nbsbj,cond,1]+MesN1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_N1 = normy((MesN1$MAD.ampl[1:nbsbj,cond,1]+MesN1$MAD.ampl[1:nbsbj,cond,2])/2)
        MADlat_A_P1 = normy((MesP1$MAD.lat[1:nbsbj,cond,1]+MesP1$MAD.lat[1:nbsbj,cond,2])/2)
        MADamp_A_P1 =normy((MesP1$MAD.ampl[1:nbsbj,cond,1]+MesP1$MAD.ampl[1:nbsbj,cond,2])/2)
        PTN1_A = normy((PTN1$values[1,1:nbsbj,cond,1]+PTN1$values[1,1:nbsbj,cond,2])/2)
        PTP1_A = normy((PTP1$values[1,1:nbsbj,cond,1]+PTP1$values[1,1:nbsbj,cond,2])/2)
        
        Amp_B_N1 = normy((ALBN1$N1.amplB[1:nbsbj,cond,1] + ALBN1$N1.amplB[1:nbsbj,cond,2])/2)
        Amp_B_N1 = 1-Amp_B_N1
        Lat_B_N1 =normy((ALBN1$N1.latB[1:nbsbj,cond,1] + ALBN1$N1.latB[1:nbsbj,cond,2])/2)
        Amp_B_P1 = normy((ALBP1$P1.amplB[1:nbsbj,cond,1] + ALBP1$P1.amplB[1:nbsbj,cond,2])/2)
        Lat_B_P1 = normy((ALBP1$P1.latB[1:nbsbj,cond,1] + ALBP1$P1.latB[1:nbsbj,cond,2])/2)
        MADlat_B_N1 =normy((MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_N1 = normy((MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesN1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADlat_B_P1 = normy((MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.lat[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        MADamp_B_P1 = normy((MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,1]+MesP1$MAD.ampl[(nbsbj+1):length(MesN1$MAD.amp[,1,1]),cond,2])/2)
        PTN1_B = normy((PTN1$values[2,1:nbsbj,cond,1]+PTN1$values[2,1:nbsbj,cond,2])/2)
        PTP1_B = normy((PTP1$values[2,1:nbsbj,cond,1]+PTP1$values[2,1:nbsbj,cond,2])/2)
        
        
        sessionA= c(Amp_A_P1[Sbj],Lat_A_P1[Sbj],MADlat_A_P1[Sbj],MADamp_A_P1[Sbj],PTP1_A[Sbj])
        sessionB= c(Amp_B_P1[Sbj],Lat_B_P1[Sbj],MADlat_B_P1[Sbj],MADamp_B_P1[Sbj],PTP1_B[Sbj])
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue") 
        }else {
          color=c("green","green","green","green","green") 
        }
        
        df = data.frame(sessionA,sessionB,color)
        plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P1")))
        MA = c(MA,sessionA)
        MB = c(MB,sessionB)
        
        
        if (Mod[ChooseMod]=="Asimp"){
          
          col = c("#898989","#00AFBB")
          rho_Asimp_P1 = c(rho_Asimp_P1 ,cor(sessionA,sessionB))
          
        }else{
          col = c("#898989","#3300CC") 
          rho_Asns_P1 = c(rho_Asns_P1 ,cor(sessionA,sessionB))
          
        }
        
      
        name = c("AMP_P1","Lat_P1","MADlat_P1","MADampl_P1","PTP1")
        
        title = paste(Sbj,"_cond_",cond,"_P1", sep="")
        #sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])
        
        
        sessionAN1= c(Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionBN1= c(Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        if (cond ==1){
          color =c("red","red","red","red","red")
        }else {
          color=c("orange","orange","orange","orange","orange")
        }
        MA = c(MA,sessionAN1)
        MB = c(MB,sessionBN1)
        
        df = data.frame(sessionAN1,sessionBN1,color)
        plot(ggplot(df,aes(x=sessionAN1, y=sessionBN1)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : N1")))
        
        
        if (Mod[ChooseMod]=="Asimp"){
          
          col = c("#898989","#00AFBB")
          rho_Asimp_N1 = c(rho_Asimp_N1 ,cor(sessionAN1,sessionBN1))
          
        }else{
          col = c("#898989","#3300CC") 
          rho_Asns_N1 = c(rho_Asns_N1 ,cor(sessionAN1,sessionBN1))
          
        }
        
        name = c("AMP_N1","Lat_N1","MADlat_N1","MADampl_N1","PTN1")
        
        title = paste(Sbj,"_cond_",cond,"_N1", sep="")
       # sp(sessionAN1,sessionBN1,name,title,"N",col,Mod[ChooseMod])
        
      
        
      }
    }
    
  }
  
  if (Mod[ChooseMod]=="Tsimp"){
    
    rho_Tsimp_P50 = list()
    rho_Tsimp_N1 = list()
    rho_Tsimp_P100 = list()
    rho_Tsimp_P200 = list()
    
    nbsbj = 11
    Sbj = 1
    
    #for (Sbj in 1:nbsbj){
    for (Sbj in 1:1){
      cond =1
      for (cond in 1:2)
      {
        Amp_A_P50 = normy(ALAP50$P50.amplA[1:nbsbj,cond])
        Lat_A_P50 = normy(ALAP50$P50.latA[1:nbsbj,cond])
        Amp_A_N1 = normy(ALAN1$N70.amplA[1:nbsbj,cond])
        Amp_A_N1 = 1-Amp_A_N1
        Lat_A_N1 = normy(ALAN1$N70.latA[1:nbsbj,cond])
        Amp_A_P100 = normy(ALAP100$P100.amplA[1:nbsbj,cond])
        Lat_A_P100 = normy(ALAP100$P100.latA[1:nbsbj,cond])
        Amp_A_P200 = normy(ALAP200$P200.amplA[1:nbsbj,cond])
        Lat_A_P200 = normy(ALAP200$P200.latA[1:nbsbj,cond])
        
        
        MADlat_A_P50 = normy(MesP50$MAD.lat[1:nbsbj,cond])
        MADamp_A_P50 =normy(MesP50$MAD.ampl[1:nbsbj,cond])
        MADlat_A_N1 = normy(MesN70$MAD.lat[1:nbsbj,cond])
        MADamp_A_N1 = normy(MesN70$MAD.ampl[1:nbsbj,cond])
        MADlat_A_P100 = normy(MesP100$MAD.lat[1:nbsbj,cond])
        MADamp_A_P100 =normy(MesP100$MAD.ampl[1:nbsbj,cond])
        MADlat_A_P200 = normy(MesP200$MAD.lat[1:nbsbj,cond])
        MADamp_A_P200 =normy(MesP200$MAD.ampl[1:nbsbj,cond])
        
        
        PTP50_A = normy(PTP50$values[1,1:nbsbj,cond])
        PTN1_A = normy(PTN70$values[1,1:nbsbj,cond])
        PTP100_A = normy(PTP100$values[1,1:nbsbj,cond])
        PTP200_A = normy(PTP200$values[1,1:nbsbj,cond])
        
        Amp_B_P50 = normy(ALBP50$P50.amplB[1:nbsbj,cond] )
        Lat_B_P50 = normy(ALBP50$P50.latB[1:nbsbj,cond])
        Amp_B_N1 = normy(ALBN1$N70.amplB[1:nbsbj,cond])
        Amp_B_N1 = 1-Amp_B_N1
        Lat_B_N1 = normy(ALBN1$N70.latB[1:nbsbj,cond] )
        Amp_B_P100 = normy(ALBP100$P100.amplB[1:nbsbj,cond])
        Lat_B_P100 = normy(ALBP100$P100.latB[1:nbsbj,cond])
        Amp_B_P200 = normy(ALBP200$P200.amplB[1:nbsbj,cond])
        Lat_B_P200 = normy(ALBP200$P200.latB[1:nbsbj,cond])
        
        
        MADlat_B_P50 = normy(MesP50$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P50 =normy(MesP50$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_N1 = normy(MesN70$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_N1 = normy(MesN70$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_P100 = normy(MesP100$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P100 =normy(MesP100$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADlat_B_P200 = normy(MesP200$MAD.lat[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        MADamp_B_P200 =normy(MesP200$MAD.ampl[(nbsbj+1):length(MesN70$MAD.amp[,1]),cond])
        
        
        PTP50_B = normy(PTP50$values[2,1:nbsbj,cond])
        PTN1_B = normy(PTN70$values[2,1:nbsbj,cond])
        PTP100_B = normy(PTP100$values[2,1:nbsbj,cond])
        PTP200_B = normy(PTP200$values[2,1:nbsbj,cond])
        
        
        sessionA= c(Amp_A_P50[Sbj],Lat_A_P50[Sbj],MADlat_A_P50[Sbj],MADamp_A_P50[Sbj],PTP50_A[Sbj])
        sessionB= c(Amp_B_P50[Sbj],Lat_B_P50[Sbj],MADlat_B_P50[Sbj],MADamp_B_P50[Sbj],PTP50_B[Sbj])
        
        if (cond ==1){
          color =c("blue","blue","blue","blue","blue") 
        }else {
          color=c("green","green","green","green","green") 
        }
        
        df = data.frame(sessionA,sessionB,color)
        plot(ggplot(df,aes(x=sessionA, y=sessionB)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P50")))
        MA = c(MA,sessionA)
        MB = c(MB,sessionB)
        rho_Tsimp_P50 = c(rho_Tsimp_P50, cor(sessionA,sessionB))
        
        
        name = c("AMP_P50","Lat_P150","MADlat_P50","MADampl_P50","PTP50")
        
        
        col = c("#898989","#FC4E07")
        
        title = paste(Sbj,"_cond_",cond,"_P50", sep="")
       # sp(sessionA,sessionB,name,title,"N",col,Mod[ChooseMod])

        
        
        sessionAN1= c(Amp_A_N1[Sbj],Lat_A_N1[Sbj],MADlat_A_N1[Sbj],MADamp_A_N1[Sbj],PTN1_A[Sbj])
        sessionBN1= c(Amp_B_N1[Sbj],Lat_B_N1[Sbj],MADlat_B_N1[Sbj],MADamp_B_N1[Sbj],PTN1_B[Sbj])
        
        if (cond ==1){
          color =c("red","red","red","red","red")
        }else {
          color=c("orange","orange","orange","orange","orange")
        }
        
        
        df = data.frame(sessionAN1,sessionBN1,color)
        plot(ggplot(df,aes(x=sessionAN1, y=sessionBN1)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : N1")))
        
        rho_Tsimp_N1= c(rho_Tsimp_N1, cor(sessionAN1,sessionBN1))
        MA = c(MA,sessionAN1)
        MB = c(MB,sessionBN1)
        
        name = c("AMP_N1","Lat_N1","MADL_N1","MADA_N1","PTN1")
        
        
        col = c("#898989","#FC4E07") 
        
        title = paste(Sbj,"_cond_",cond,"_N1", sep="")
        #sp(sessionAN1,sessionBN1,name,title,"N",col,Mod[ChooseMod])
        
        
        sessionAP1= c(Amp_A_P100[Sbj],Lat_A_P100[Sbj],MADlat_A_P100[Sbj],MADamp_A_P100[Sbj],PTP100_A[Sbj])
        sessionBP1= c(Amp_B_P100[Sbj],Lat_B_P100[Sbj],MADlat_B_P100[Sbj],MADamp_B_P100[Sbj],PTP100_B[Sbj])
        
        if (cond ==1){
          color =c("purple","purple","purple","purple","purple") 
        }else {
          color=c("pink","pink","pink","pink","pink") 
        }
        
        df = data.frame(sessionAP1,sessionBP1,color)
        plot(ggplot(df,aes(x=sessionAP1, y=sessionBP1)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P100")))
        
        rho_Tsimp_P100 = c(rho_Tsimp_P100, cor(sessionAP1,sessionBP1))
        
        
        name = c("AMP_P100","Lat_P100","MADL_P100","MADA_P100","PTP100")
        
        
        col = c("#898989","#FC4E07")
        
        title = paste(Sbj,"_cond_",cond,"_P100", sep="")
        sp(sessionAP1,sessionBP1,name,title,"N",col,Mod[ChooseMod])
        
        
        sessionAP2= c(Amp_A_P200[Sbj],Lat_A_P200[Sbj],MADlat_A_P200[Sbj],MADamp_A_P200[Sbj],PTP200_A[Sbj])
        sessionBP2= c(Amp_B_P200[Sbj],Lat_B_P200[Sbj],MADlat_B_P200[Sbj],MADamp_B_P200[Sbj],PTP200_B[Sbj])
        
        if (cond ==1){
          color =c("black","black","black","black","black") 
        }else {
          color=c("grey","grey","grey","grey","grey") 
        }
        
        df = data.frame(sessionAP2,sessionBP2,color)
        plot(ggplot(df,aes(x=sessionAP2, y=sessionBP2)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
               ggtitle(paste("mod :",Mod[ChooseMod],"/sbj :" ,as.character(Sbj),"/cond :",as.character(cond),"/comp : P200")))
        
        rho_Tsimp_P200 = c(rho_Tsimp_P200, cor(sessionAP2,sessionBP2))
        
        
        name = c("AMP_P200","Lat_P200","MADL_P200","MADA_P200","PTP200")
        
        
        col = c("#898989","#FC4E07")
        
        title = paste(Sbj,"_cond_",cond,"_P200", sep="")
        sp(sessionAP2,sessionBP2,name,title,"N",col,Mod[ChooseMod])
        
        
      }
    }
    
  }
  
}



#**************************************************************
#********Intra session ****comp by cond******comparison*********
#**************************************************************

color =c("blue","purple","green","red","black") 
rhoN1C1vsC2B = list()
j = 1
k = 5
for (i in 1: Sbj){
  
  x = array(unlist(N1FeaturesB1[j:k]))
  y= array(unlist(N1FeaturesB2[j:k]))
  
  df = data.frame(x,y,color)
  
  plot(ggplot(df,aes(x=x, y=y)) + geom_point(aes(size= 0.1,colour = color),show.legend = FALSE) +scale_color_identity() +
         ggtitle(paste("sbj :" ,as.character(Sbj),"/cond :1 vs 2 /comp : N70")))
  
  rhoN1C1vsC2B = c(rhoN1C1vsC2B, cor(x,y))
  
  name = c("AMP_N70","Lat_N70","MADlat_N70","MADampl_N70","PTN70")

  col = c("#EBA578","#FC4E07") 
  title = paste(i,"_N70_cond_comp_B", sep="")
  sp(x,y,name,title,"N",col,Mod[ChooseMod])
  
  j= j+5
  k = k+5
}


rhoP1C1vsC2B = array(unlist(rhoP1C1vsC2B))

rho = data.frame(rhoP1C1vsC2A,rhoP1C1vsC2B,rhoN1C1vsC2A,rhoN1C1vsC2B)

#rho = data.frame(rhoP50C1vsC2A,rhoP50C1vsC2B,rhoP100C1vsC2A,rhoP100C1vsC2B,rhoP200C1vsC2A,rhoP200C1vsC2B,rhoN1C1vsC2A,rhoN1C1vsC2B)
write.csv(rho, "xxxxx/rho_Tsimp_cond_comp .csv", row.names=TRUE)


#**************************************************************
#********Group parameters consistency **************************
#**************************************************************

param = c("Amp","Lat","MADA","MADL","ITC")
comp = c("C1 / P1"," C2 / P1","C1 / N1","C2 / N1")
Bigdf = data.frame(array(unlist(P1featuresA1)),array(unlist(P1featuresB1)),array(unlist(P1featuresA2)),array(unlist(P1featuresB2)),array(unlist(N1FeaturesA1)),array(unlist(N1FeaturesB1)),array(unlist(N1FeaturesA2)),array(unlist(N1FeaturesB2)))
Allrho= array()
k= 0
l= 1

for (l in 1:4){

  j = 1
  
  for (j in 1:5){
    
    rho = list()
    x =list()
    y = list()
    
    for (i in 1: Sbj){
      
      x= c(x,Bigdf[[1+2*k]][j])
      y= c(y,Bigdf[[2*l]][j])
      
      j= j+5
      
    }
    
    x= array(unlist(x)) 
    y= array(unlist(y)) 
    
    df = data.frame(x,y)
    
    plot(ggplot(df,aes(x=x, y=y)) + geom_point(aes(size= 0.1),show.legend = FALSE) +
           ggtitle(paste("Group comparison /",comp[l],"/",param[j-5*Sbj])))
    
    rho = cor(x,y)
    Allrho = c(Allrho,rho)
  } 
  k = k+1
  l = l+1
}


Allrho = na.omit(Allrho)
#AllrhoVsimp = c(AllrhoVsimp,Allrho)


rho = data.frame(AllrhoVsimp,AllrhoVsns,AllrhoAsimp,AllrhoAsns,AllrhoTsimp[1:20],AllrhoTsimp[21:40])
write.csv(rho, "xxxxx/rho_group_cond_comp.csv", row.names=TRUE)

#Linear Regression

data = read.table("w.txt", header = TRUE)
plot(Vsns~age,data=data)
age.Vsns.lm = lm(Vsns~age, data=data)
summary(age.Vsns.lm)
hist (data$Vsns)
shapiro.test(data$Vsns)
var.test(data$age,data$Vsns)




p <- ggscatter(df, x = "x", y = "y",
               add = "reg.line",  # Add regression line
               add.params = list(color = "blue", fill = "lightblue"), # Customize reg. line
               conf.int = TRUE # Add confidence interval
)
p + stat_cor(method = "pearson")

