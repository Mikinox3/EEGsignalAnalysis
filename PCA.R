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
library(abind)
library("corrplot")

#**************************************
#*#***********FUNCTION DEF*************
#**************************************


normy<- function(x){
  
  y = x
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
#*#************PCA*********************
#*#**************************************
  
  setwd("xxxx/IIV")
  mx =readRDS("oriData.Rds")
  
  
  
  res.pca =PCA(mx[1:5,1:20,1:2,1])
  # 5 Modalities, 20 param, 2 sessions, 1 sbj
  
  res.pca =PCA(mx[1:10,1:20,1])
  get_eigenvalue(res.pca)
  fviz_eig(res.pca)
  fviz_pca_biplot(res.pca)
  res.pca$var
  
  corrplot(res.pca$var$contrib, is.corr=FALSE) 
  
  
  
  set.seed(123)
  res.km <- kmeans(res.pca$var$coord, centers = 3, nstart = 25)
  grp <- as.factor(res.km$cluster)
  # Colorer les variables par groupes
  fviz_pca_var(res.pca, col.var = grp, 
               palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
               legend.title = "Cluster")
  
  
  fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
  
  res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
  res.desc$Dim.1
 
   
   # Contributions des variables à PC1
   fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
   fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
   fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)   

   fviz_pca_var(res.pca, col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
   )   
  
   w=list()
   for (h in 1:12){
     res.pca =PCA(mx[1:5,1:20,1:2,h])
     res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
     corrplot(res.pca$var$contrib, is.corr=FALSE) 
     w = c(w,cbind(res.desc$Dim.1, res.desc$Dim.2))
   }

  
   
   res.pca =PCA(mx[1:5,1:20,1,1])
   res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
   k = res.desc$Dim.1
   
   P1=attr(k$quanti,"dimnames")[1]
   P1=array(unlist(P1))
   
   res.pca =PCA(mx[1:5,1:20,2,1])
   res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
   k = res.desc$Dim.1
   
   P2=attr(k$quanti,"dimnames")[1]
   P2=array(unlist(P2))

   all_lists <- list(P1, P2)
   
  
   # Combine the 10 lists into a single list of lists
   all_lists <- list(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,P11, P12)
   
   # Create an empty matrix to store the comparison results
   result_matrix <- matrix(NA, nrow = length(all_lists), ncol = length(all_lists))
   
   # Create an empty list to store the rank of common items
   common_rank <- vector("list", length(all_lists))
   
   # Loop through each pair of lists and compare their elements
   for (i in 1:length(all_lists)) {
     for (j in 1:length(all_lists)) {
       # Skip comparisons between the same list
       if (i == j) {
         result_matrix[i,j] <- NA
       } else {
         list1 <- all_lists[[i]]
         list2 <- all_lists[[j]]
         common_items <- intersect(list1, list2)
         match_count <- length(common_items)
         result_matrix[i,j] <- match_count
         
         # Store the rank of common items
         if (match_count > 0) {
           rank_list1 <- sapply(common_items, function(x) which(x == list1))
           rank_list2 <- sapply(common_items, function(x) which(x == list2))
           common_rank[[i]][[j]] <- rank_list1
           common_rank[[j]][[i]] <- rank_list2
         }
       }
     }
   }

   
   # Print the result matrix and common item ranks
   result_matrix
   common_rank
  save(result_matrix,common_rank,file='commonIdx.Rdata')

 
  
  
   all_lists=list(P1,P2)
  
  common_items <- all_lists[[1]]
  for (i in 2:length(all_lists)) {
    common_items <- intersect(common_items, all_lists[[i]])
  }
  
  # Print the common items
  print(common_items)   
  
  
  
  
  newmx = abind(mx[1:5,1:20,1,1:12],mx[1:5,1:20,2,1:12], along = 1)
  colnames(newmx)= c("Vsimp_A","Vsns_A","Asimp_A","Asns_A","Tsimp_A","Vsimp_B","Vsns_B","Asimp_B","Asns_B","Tsimp_B")
  rownames(newmx)= name
  res.pca =PCA(mx[1:10,1:20,12])
  
  

  for (h in 2:12){
    res.pca = PCA(mx[1:10,1:20,h])
    ModCoord  = abind(ModCoord ,res.pca$ind$coord,along=3)
  }
  

  for (h in 2:12){
  
  blahA = flatten(data[1:5,1:20,h,1])
  sessA = rbind(sessA,blahA)
  
  
  blahB = flatten(data[1:5,1:20,h,2])
  sessB = rbind(sessB,blahB)
  }
  
  
  
  row.names(sessB)=c("Sbj1","Sbj2","Sbj3","Sbj4","Sbj5","Sbj6","Sbj7","Sbj8","Sbj9","Sbj10","Sbj11","Sbj12")
  Grpmx = rbind(sessA ,sessB)
  va= rep(name,5)
  for (i in 81:100){
    va[i] =paste("T_",va[i],sep="")
  }
  colnames(Grpmx)=va
  
  
  res.pca =PCA(Grpmx)
  GrpCoord = res.pca$ind$coord
  

  
  
  