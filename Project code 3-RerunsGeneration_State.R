#create soil data files for each grid
rm(list=ls())
library(data.table)
mydata <- fread("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/sample6.csv",header=TRUE)
mycsv<-read.csv("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/5km_grid_Rice.csv",header = TRUE)
rerunsdir="C:/Users/S.VYAS/OneDrive - CIMMYT/Madhur CCAFS/reruns4/"
list1<-list('CNTR','WCWP1','WCWP2','WCWP3','TKL1','TKL2','TKL3M','WCFC1','WCFC2','WCFC3','WCST1','WCST2','WCST3','KSAT1','KSAT2','KSAT3','BD1','BD2','BD3','SOCIN1','SOCIN2','SOCIN3','CLAY1','CLAY2','CLAY3','SILT1','SILT2','SILT3','PHSOL1','PHSOL2','PHSOL3','WCAD1','WCAD2','WCAD3','SLOPE','EC1','CN2')
mycsv<-mycsv[mycsv$State == 'Andhra Pradesh',]
codes<-mycsv$New_Code
cc<-mydata[mydata$CNTR %in% codes]
for (i in 1:length(codes)){
  outfile=paste0(rerunsdir,"reruns_",as.character(cc$CNTR[i]),".DAT")
  b <- as.list(cc[i,])
  for (j in 1:length(b)){
    if(list1[j]=="CNTR"){
      write(paste0(list1[j],"=",paste0("'", b[j], "'")),outfile,append=T)
    }else{
      write(paste0(list1[j],"=",noquote(sprintf("%.2f",b[j]))),outfile,append = T)
    }
  }
}




  