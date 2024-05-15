source("Data_manipulation.R")
source("Control_checks.R")
source("Indices.R")
source("Calibrations.R")
source("bayRmbt_model.R")
source("bayRmbt_forward.R")
source("bayRmbt_predict.R")
source("bayspR_tex.R")
source("bayspR_tex_analog.R")
source("RthChordDistances.R")
library(geohashTools)
library(ggplot2)
library(plotly)
library(reshape2)
library(viridis)
library(raster)

DATA<-read.csv(file("Compiled_data_brgdgt.csv"),head=TRUE,sep=',')

###Test for duplicates
#What this is doing is taking the geohash from each sample, comparing it to 
#other samples with the same location, then comparing if they are attributed to 
#different references.The output "duplicates" is a list where each object in the 
#list has the name of the Tag for the samples identified with duplicates, and 
#the object contains a vector with the tags of the samples that have the same 
#geohash but different reference. The tag of all samples identified as 
#duplicates are stored in the "exclude" vector, and they are skipped.

duplicates<-list()
exclude<-NULL
for (i in 1:dim(DATA)[1]) {
  sample<-DATA$Tag[i]
  if(sample %in% exclude){next}
  sameLoc<-DATA$Geohash[i]==DATA$Geohash
  DuplicateIndx<-which(DATA$Citation.No.[i] != DATA[sameLoc,27])
  DupCandidates<-DATA[sameLoc,29]
  DupCandidates<-DupCandidates[DuplicateIndx]
  j<-length(duplicates)+1
  if(length(DupCandidates)>0){
    duplicates[[j]]<-DupCandidates;
    names(duplicates)[j]<-sample
  }
  exclude<-c(exclude,DupCandidates)
}

###Now more standard stuff

##Show locations
sites<-data.frame("Sample"=DATA$Type,"Lat"=DATA$Latitude,"Lon"=DATA$Longitude)

fig<-sites
fig <- fig %>% plot_ly(lat = ~Lat,lon = ~Lon,mode="markers",alpha = 100,size=8,color=~Sample,type = 'scattermapbox')
fig <- fig %>% layout(mapbox = list(style = 'open-street-map',zoom =11)) 
fig


##distribution of GDGTs by sample type
mDATA<-melt(DATA[,c(2,3,4,8,10:25)],id.vars = c("HexID","SampleName","Type","MAAT","pH"))

ggplot(mDATA[mDATA$Type!="Bone"&mDATA$Type!="Groundwater",],aes(variable,value))+
  geom_boxplot(aes(fill=variable))+
  geom_jitter(color="black",size=0.4, alpha=0.9) +
  facet_wrap(~Type)+
  scale_fill_viridis(discrete = TRUE)

##environmental variables
ggplot(mDATA[mDATA$Type!="Bone"&mDATA$Type!="Groundwater",],aes(value,MAAT))+
  geom_point()+
  facet_wrap(Type~variable,drop = TRUE)

ggplot(DATA,aes(MBT5(DATA,complete = FALSE,data="fa",na.ignore = TRUE),MAAT))+
  geom_point(aes(color=Type))+
  scale_color_viridis(discrete = TRUE)

###Get environmental parameters from CHELSA
Bio1<-raster("CHELSA_bio1_1981-2010_V.2.1.tif")
Bio4<-raster("CHELSA_bio4_1981-2010_V.2.1.tif")
Bio10<-raster("CHELSA_bio10_1981-2010_V.2.1.tif")

LL<-data.frame("Lat"=DATA$Latitude,"Lon"=DATA$Longitude)
coordinates(LL)<-~Lat+Lon

Bio1_values=extract(Bio1, LL)
