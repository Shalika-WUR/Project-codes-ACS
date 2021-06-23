library(rgdal)
library(raster)
library(tools)
#output files directory
sumfolder="C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/output/"
l=list.files(sumfolder)
#merged output file path
outfile=paste0("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/SUM_FINAL.OUT")
list1<-list()
for(i in 1: length(l))
{ print("executing.......")
  fileName=paste0(sumfolder,l[i])
  con=file(fileName,open="r")
  ctable2<-readLines(con)
  if(!is.na(ctable2[3])){
    write(ctable2[3],outfile,append=T)
    list1[i]<-unlist(strsplit(file_path_sans_ext(l[i]), split='_', fixed=TRUE))[2]
  }
  
  close(con) 
}
print("writing to Shapefile")
mylist2 = list1[-which(sapply(list1, is.null))]
mylist2=t(data.frame(mylist2))
r<-read.table("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/SUM_FINAL.OUT")
r1<-data.frame(r)
b1<-cbind(r1,mylist2)
names(b1)[names(b1) == "mylist2"] <- "New_Code"


# 5km grid shapefile path
p <- shapefile("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/5km_grid_Rice.shp")
m <- merge(p, b1, by='New_Code')

# output merged shapefile path
shapefile(m, "C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/Infocrop_rice_2018.shp")

print("done")
