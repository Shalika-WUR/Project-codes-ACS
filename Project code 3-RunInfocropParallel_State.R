rm(list=ls())
library(MASS)
library(dplyr)
library(filesstrings)
library(foreach)
library(doParallel)

start_time = Sys.time()
#path to csv file
mycsv<-read.csv("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/5km_grid_Rice.csv",header = TRUE)
mycsv<-mycsv[mycsv$State == 'Andhra Pradesh',]
list_1<-mycsv$New_Code

#directory paths
rerundir<-"C:/Users/S.VYAS/OneDrive - CIMMYT/Madhur CCAFS/reruns4/"
sourcedir<-"C:/Users/S.VYAS/OneDrive - CIMMYT/Madhur CCAFS/model/"
outdir<-"C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/output/"

dir.create(outdir, showWarnings = TRUE)

N <- length(list_1)
cores=7  # no. of cores
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)

result <- foreach(i=0:N) %dopar% 
{
    library(filesstrings)
    pno = Sys.getpid()
    print(pno)
    wrkdir=paste0(sourcedir,"TEMP_", pno,"/")
    if (!dir.exists(wrkdir)){
      dir.create(wrkdir, showWarnings = FALSE)
      file.copy(paste0(sourcedir,"model.exe"), wrkdir)
      file.copy(paste0(sourcedir,"model.DAT"), wrkdir)
      file.copy(paste0(sourcedir,"timer.DAT"), wrkdir)
    }else{
      file.copy(paste0(rerundir,"reruns_",list_1[i],".DAT"),wrkdir)
      file.rename(paste0(wrkdir,"reruns_",list_1[i],".DAT"),paste0(wrkdir,"reruns.DAT"))
      setwd(wrkdir)
      system("model.exe", intern=TRUE, invisible = FALSE, wait = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE)
      Sys.sleep(0.5)
      file.rename(paste0(wrkdir," SUM.OUT"),paste0(wrkdir,"SUM_",list_1[i],".OUT"))
      file.move(paste0(wrkdir,"SUM_",list_1[i],".OUT"),outdir)
    }
}
end_time = Sys.time()

print(paste0("Run time ::: ", end_time - start_time ))
#unlink from memory, shut down R
#resolve for temp file error
