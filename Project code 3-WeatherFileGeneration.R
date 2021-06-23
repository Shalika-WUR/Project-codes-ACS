library(raster)
library(rgdal)
library(MASS)
library(dplyr)
library(foreach)
library(doParallel)

#reading the file with 5km grid codes
mycsv<-read.csv("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/5km_grid_Rice.csv",header = TRUE)

#select the state for which you want to generate the weather files for
mycsv<-mycsv[mycsv$State == 'Andhra Pradesh',]
lat<-mycsv$Latitude
lon<-mycsv$Longitude
code<-mycsv$New_Code
N <- length(lon)

#initiating the parallel processing 
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

result <- foreach(i=1:N) %dopar% 
  {
    library(raster)
    library(rgdal)
    # extract precipitation, tmax and tmin from their respective nc files.
    r <- brick("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/2019.nc", varname = "precip")
    valprecip <- extract(r, matrix(c(lon[i],lat[i]), ncol = 2))
    valprecip1<-format(round(data.frame(valprecip[1:365]),digits=1),nsmall=1)
    
    r1<- brick("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/tmin2019.nc", varname = "tmin")
    valtmin <- extract(r1, matrix(c(lon[i],lat[i]), ncol = 2))
    valtmin1<-format(round(data.frame(valtmin[1:365]),digits = 1),nsmall=1)
    
    r3 <- brick("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/tmax2019.nc", varname = "tmax")
    valtmax <- extract(r3, matrix(c(lon[i],lat[i]), ncol = 2))
    valtmax1<-format(round(data.frame(valtmax[1:365]),digits = 1),nsmall=1)
    
    #insert the year and the number of days in that year
    valyear<-data.frame(rep(2019,365))
    #inserting date number in the required format
    a<-seq(1:365)
    valtime<-noquote(data.frame(sprintf("%03d",a)))
    valghi<-data.frame(rep(-99.9,365))
    valstid<-data.frame(rep(1,365))
    valvp<-data.frame(rep(-99.9,365))
    valws<-data.frame(rep(-99.9,365))
    
    #combining the data to insert
    temp=cbind(valstid,valyear,valtime,valghi,valtmin1,valtmax1,valvp,valws,valprecip1)
    temp1=data.frame(cbind(lon[i],lat[i],"0.00","0.00","0.00"))
     #writing the data  
    write.table(temp1,paste0("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/",code[i],"1.019"),sep=" ",row.names=FALSE,col.names=FALSE,quote=FALSE)
    write.table(temp,paste0("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Crop sim/",code[i],"1.019"),sep=" ",row.names=FALSE,col.names=FALSE,append = TRUE,quote=FALSE)
    
    
  }

stopCluster(cl)