# OBJECT  : Yield potential and gaps methods comparison
# PROJECT : GYGA
# AUTHOR  : Antoine Couedel
# START DATE  : 08 02 2021
# END DATE : 18 02 2025

#0) read GYGA data ####
#select the file type (WEATHER data)
rScripts.wd="C:/Users/COUEDEL/Dropbox/GYGA_Postdoc/USA/0_data/"
inputs.wd=paste0(rScripts.wd,"/1_input_data")
outputs.wd=paste0(rScripts.wd,"/2_outputs")
raw.wd=paste0(rScripts.wd,"/0_raw data")
  
##### Working directory, Packages, Functions####  ---------------------------------
# packages
.libPaths("D:/Mes Donnees/R/win-library/4.1/")
library(splitstackshape)
library(geosphere)
library(sirad)
library(caTools)
library(rnoaa)
library(RCurl)
library(readr)
library(readxl)
if(!require(data.table)){install.packages("data.table");  library(data.table)}
if(!require(plyr)){install.packages("plyr");  library(plyr)}
if(!require(devtools)){install.packages("devtools");  library(devtools)}
if(!require(lattice)){install.packages("lattice");  library(lattice)}
if(!require(ZeBook)){install.packages("ZeBook");  library(ZeBook)}
if(!require(rgdal)){install.packages("rgdal");  library(rgdal)}
if(!require(zoo)){install.packages("zoo");  library(zoo)}
if(!require(stringr)){install.packages("stringr");  library(stringr)}
if(!require(ggplot2)){install.packages("ggplot2");  library(ggplot2)}
if(!require(dplyr)){install.packages("dplyr");  library(dplyr)}
if(!require(tidyr)){install.packages("tidyr");  library(tidyr)}
if(!require(rio)){install.packages("rio");  library(rio)}
if(!require(splusTimeDate)){install.packages("splusTimeDate");  library(splusTimeDate)}
if(!require(lubricate)){install.packages("lubricate");  library(lubricate)}
if(!require(scales)){install.packages("scales");  library(scales)}
if(!require(ggpmisc)){install.packages("ggpmisc");  library(ggpmisc)}
if(!require(quantreg)){install.packages("quantreg");  library(quantreg)}
if(!require(janitor)){install.packages("janitor");  library(janitor)}
if(!require(ggpattern)){install.packages("ggpattern");  library(ggpattern)}
if(!require(ggh4x)){install.packages("ggh4x");  library(ggh4x)}

install.packages("ggplus")
install.packages("ggforce")
install.packages("gridExtra")

library(ggplot2)
library(ggforce)
library(gridExtra)
library(goeveg)

# functions
source("function_plot_linear_regression.R")

# define lattice theme to make nice plots
mytheme <- trellis.par.get()
mytheme$strip.border$col = 'lightblue'
mytheme$strip.background$col = 'lightblue'
mytheme$axis.line$col = 'lightblue'
mytheme$axis.text$col = 'grey40'
mytheme$axis.text$cex = 1
mytheme$par.ylab.text$cex = 1.3
mytheme$par.xlab.text$cex = 1.3
mytheme$plot.line$col = "black"



#read data
data=read.csv(paste0(inputs.wd,"/Station Year Maize Soybean Wheat.csv"),sep=";")

#0)lat long####
#load lat/long
latlong=read.csv(paste0(inputs.wd,"/LatLong.csv"),sep=",")
data=merge(data,latlong,by.x=c("STATIONNAME"))

data$x = data$HARVESTYEAR 

#get year (max, min) per site GYGA#####

dataYear=ddply(data, ~STATIONNAME*CROP, summarise,
                min_year = min(HARVESTYEAR,na.rm=T),
                max_year = max(HARVESTYEAR,na.rm=T))


#1)Hatfield method####
      #prep Ya per buffer for 40y####

#import Ya irrigated and rainfed
#import data from paper (get unc and non irrigated and use unc when non-irrigated are absent ?)
setwd(normalizePath("1_input_data/", winslash = "/"))

#read 40 Ya data
Ya40y=read.csv("Ya_1979_2018_all_crops.csv",sep=";")
Ya40y$State=str_to_lower(Ya40y$State)
Ya40y$State=str_to_title(Ya40y$State)
Ya40y$County=str_to_lower(Ya40y$County)
Ya40y$County=str_to_title(Ya40y$County)

#get the same name than "COU" column
Ya40y$COU=paste0(Ya40y$State,Ya40y$County)
Ya40y$crop=with(Ya40y,ifelse(Commodity=="CORN","maize",ifelse(Commodity=="WHEAT","wheat",
                                                              ifelse(Commodity=="SOYBEANS","soybean",NA))))
Ya40y$I_R=with(Ya40y,ifelse(Data.Item=="WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE"|
                              Data.Item=="WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE"|
                              Data.Item=="CORN, GRAIN - YIELD, MEASURED IN BU / ACRE"|
                              Data.Item=="SOYBEANS - YIELD, MEASURED IN BU / ACRE","R","I"))
Ya40y$W_S=with(Ya40y,ifelse(Data.Item=="WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE","S",
                            ifelse(Data.Item=="WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE","W",NA)))
                        
#recalculate yields
Ya40y$Ya=with(Ya40y,ifelse(Commodity=="CORN",Value*0.0628,Value*0.0673))
#Ya40y$Ya=with(Ya40y,ifelse(Commodity=="CORN",Value*0.0628*0.845,Value*0.0673*0.87))


#import columns usefull from excel Ya
buffers_soy=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_soy$crop="soybean"
buffers_maize=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_maize$crop="maize"
buffers_wheat=read.csv("Ya_buffers_wheat.csv",sep=";")
buffers_wheat$crop="wheat"
buffers_wheat$I_R="R"
buffers=rbind.fill(buffers_soy,buffers_maize,buffers_wheat)

#merge all
data=merge(buffers,Ya40y,by=c("crop","COU","I_R"))

data=subset(data,Year>1978)
data=subset(data,Year<2019)

      #get Ya per buffer####
      #for Wheat####
dataSW=subset(data,Data.Item=="WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE")

dataWW=subset(data,Data.Item=="WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE")

dataSW=dataSW[c("Year","COU","crop","I_R","W_S","Ya")]

dataWheat=merge(dataWW,dataSW,by=c("Year","COU","crop","I_R"),all=T)

dataWheat$Ya_Wheat=with(dataWheat,ifelse(Type.Wheat=="W",Ya.x,
                                  ifelse(Type.Wheat=="S",Ya.y,
                                  ifelse(Type.Wheat=="SW",Ya.x*Percent.Winter.Spring+Ya.y*(1-Percent.Winter.Spring)
                                         ,NA))))

dataWheat$Ya_WW=dataWheat$Ya.x

dataWheat$Yield_weighted=ifelse(is.na(dataWheat$Ya_Wheat)|is.na(dataWheat$Percent_Max.Area),NA,
                                dataWheat$Ya_Wheat*dataWheat$Percent_Max.Area)

dataWheat$Yield_weightedWW=ifelse(is.na(dataWheat$Ya_WW)|is.na(dataWheat$Percent_Max.Area),NA,
                                dataWheat$Ya_WW*dataWheat$Percent_Max.Area)


dataWheat=ddply(dataWheat, ~crop*BUFFER*I_R*Year, summarise,
                Percent_Max.Area.Corr = sum(Percent_Max.Area,na.rm=T),
                Ya = sum(Yield_weighted,na.rm=T),
                Ya_WW=sum(Yield_weightedWW,na.rm=T))

dataWheat$Ya=dataWheat$Ya/dataWheat$Percent_Max.Area.Corr
dataWheat$Ya_WW=dataWheat$Ya_WW/dataWheat$Percent_Max.Area.Corr
dataWheat$Percent_Max.Area.Corr=NULL

      #for Soybean and maize####
dataSM=subset(data,Data.Item!="WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE"&
                Data.Item!="WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE")

dataSM$Yield_weighted=ifelse(is.na(dataSM$Ya)|is.na(dataSM$Percent_Max.Area),NA,
                                dataSM$Ya*dataSM$Percent_Max.Area)

dataSM=ddply(dataSM, ~crop*BUFFER*I_R*Year, summarise,
                Percent_Max.Area.Corr = sum(Percent_Max.Area,na.rm=T),
                Ya = sum(Yield_weighted,na.rm=T))

dataSM$Ya=dataSM$Ya/dataSM$Percent_Max.Area.Corr
dataSM$Percent_Max.Area.Corr=NULL
dataSM$Ya_WW=NA

      #rbind all crops####
data=rbind(dataSM,dataWheat)

data$crop_IR=paste0(data$crop,"_",data$I_R)


      #Ya data for Mueller####
# data_Ya_Mueller=subset(data,Year>1996)
# data_Ya_Mueller=subset(data,Year<2004)
# 
# data_Ya_Mueller=ddply(data_Ya_Mueller, ~crop*BUFFER*crop_IR, summarise,
#                               Ya_Mueller = mean(Ya,na.rm=T))
# 
# data_Ya_Mueller$crop=with(data_Ya_Mueller,
#                             str_replace_all(data_Ya_Mueller$crop_IR,
#                                             c(maize_R="Rainfed maize",maize_I="Irrigated maize",
#                                               soybean_R="Rainfed soybean",soybean_I="Irrigated soybean",
#                                               wheat_R="Rainfed wheat")))
# 
# names(data_Ya_Mueller)[names(data_Ya_Mueller) == 'crop'] <- 'CROP'
# names(data_Ya_Mueller)[names(data_Ya_Mueller) == 'BUFFER'] <- 'STATIONNAME'
# 
# data_Ya_Mueller=data_Ya_Mueller[c("CROP","STATIONNAME","Ya_Mueller")]
# 

      #get the Yw all crops####
for (i in unique(data$crop_IR))
{
datacrop=data[which(data$crop_IR == i), ]
for(j in unique(datacrop$BUFFER))
{
new=datacrop[which(datacrop$BUFFER == j), ]
m = rq(Ya ~  Year, tau=0.95,new);
lm=predict(m,newdata = new,type = "none", interval = c("none", "confidence"), 
           level = .95, na.action = na.pass)
#lm2<-fitted(m)
new=cbind(new, Yw_LM = lm)
new=new[c("crop_IR","BUFFER","Year","Yw_LM")]
if(exists("whole") == F) { whole <- new } else { whole <- as.data.frame(rbind(whole[,] , new[,]) ) }
rm(new)
}
if(exists("data_whole") == F) { data_whole <- whole } else { data_whole <- as.data.frame(rbind(data_whole[,] , whole[,]) ) }
rm(whole)
}
data=merge(data,data_whole,by=c("crop_IR","BUFFER","Year"))
rm(data_whole)

#get the Yw#
#dataframe to get the mean Ya and mean Yw
data$Yg_LM=round(data$Yw_LM-data$Ya,2)

data$BUFFER=with(data,ifelse(crop=="wheat",
                          str_replace_all(data$BUFFER,
                               c(Custar="Custar OH",Dazey="Dazey ND",
                                 Hay="Hays",Lisbon="Lisbon ND" 
                                ,Pierre="Pierre SD",Wooster="Wooster OH")),BUFFER))


#for perc it should be over the avarage so need  to do TDC
#data$Yg_LM_perc=(data$Yw_LM-data$YA)/(data$Yw_LM)
  
  
#jpeg(normalizePath("../2_outputs/maize Yp annual gain graph.jpeg"), units="in", width=15, height=10, res=300)
# ggplot(data = datamaize, aes(x = x, y = YA)) +
#   geom_point()+#geom_text(data=eq,aes(x = 2000, y = 5,label=V1), parse = TRUE, inherit.aes=FALSE)+
#   facet_wrap(STATIONNAME~.)+
#   geom_quantile(quantiles = 0.95)+xlim(2004,2014)


      #merge Yw Hatfield with GYGA data####
latlong=read.csv("LatLong.csv",sep=",")
dataGYGA=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
dataGYGA=merge(dataGYGA,latlong,by.x=c("STATIONNAME"))

data$crop_IR=with(data,str_replace_all(data$crop_IR,
                                         c(maize_R="Rainfed maize",maize_I="Irrigated maize",
                                           soybean_R="Rainfed soybean",soybean_I="Irrigated soybean",
                                           wheat_R="Rainfed wheat")))

dataHat=merge(dataGYGA,data,by.x=c("STATIONNAME","CROP","HARVESTYEAR"),by.y=c("BUFFER","crop_IR","Year"),all=T)

      #prep graph for Hatfield comparison####

dataHat$keep=with(dataHat,ifelse((CROP=="Rainfed wheat"|CROP=="Irrigated wheat")&HARVESTYEAR<2004,"no",
               ifelse(HARVESTYEAR<2009,"no","yes")))
dataHat=subset(dataHat,keep=="yes")

dataHat_year=dataHat

dataHat=ddply(dataHat, ~CROP*STATIONNAME, summarise,
              LONGITUDE=mean(LONGITUDE,na.rm=T),
              LATITUDE=mean(LATITUDE,na.rm=T),
              Yw_GYGA = mean(YW,na.rm=T),
              Ya_GYGA = mean(YA,na.rm=T),
              Yw_Hat = mean(Yw_LM,na.rm=T),
              Yg_Hat = mean(Yg_LM,na.rm=T))

dataHat$Yg_GYGA=dataHat$Yw_GYGA-dataHat$Ya_GYGA

dataHat_year=ddply(dataHat_year, ~CROP*STATIONNAME*HARVESTYEAR, summarise,
              LONGITUDE=mean(LONGITUDE,na.rm=T),
              LATITUDE=mean(LATITUDE,na.rm=T),
              Yw_GYGA = mean(YW,na.rm=T),
              Ya_GYGA = mean(YA,na.rm=T),
              Yw_Hat = mean(Yw_LM,na.rm=T),
              Yg_Hat = mean(Yg_LM,na.rm=T))

dataHat_year$Yg_GYGA=dataHat_year$Yw_GYGA-dataHat_year$Ya_GYGA


#keep only buffers with values when GYGA !=NA
dataHat=subset(dataHat,!is.na(Yg_Hat))


dataHat<- melt(dataHat, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataHat[] <- lapply(dataHat, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataHat$value =as.numeric(as.character(dataHat$value))
dataHat$LONGITUDE =as.numeric(as.character(dataHat$LONGITUDE))
dataHat$LATITUDE =as.numeric(as.character(dataHat$LATITUDE))


dataHat_year=subset(dataHat_year,!is.na(Yg_Hat))


dataHat_year<- melt(dataHat_year, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataHat_year[] <- lapply(dataHat_year, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataHat_year$value =as.numeric(as.character(dataHat_year$value))
dataHat_year$LONGITUDE =as.numeric(as.character(dataHat_year$LONGITUDE))
dataHat_year$LATITUDE =as.numeric(as.character(dataHat_year$LATITUDE))


#2)Kucharik method####
    #prep####
    #import data from paper####
setwd(normalizePath("1_input_data/", winslash = "/"))

#read data
datasoy_unc=read.csv("clean_soy_unc_ygap.csv",sep=",")
datamaize_unc=read.csv("clean_corn_unc_ygap.csv",sep=",")
datawheat_unc=read.csv("clean_wheat_unc_ygap.csv",sep=",")
datasoy_unc$crop="soybean"
datamaize_unc$crop="maize"
datawheat_unc$crop="wheat"

datasoy_r=read.csv("clean_soy_non_ygap.csv",sep=",")
datamaize_r=read.csv("clean_corn_non_ygap.csv",sep=",")
datawheat_r=read.csv("clean_wheat_non_ygap.csv",sep=",")
datasoy_r$crop="soybean"
datamaize_r$crop="maize"
datawheat_r$crop="wheat"

datasoy_i=read.csv("clean_soy_irr_ygap.csv",sep=",")
datamaize_i=read.csv("clean_corn_irr_ygap.csv",sep=",")
datawheat_i=read.csv("clean_wheat_irr_ygap.csv",sep=",")
datasoy_i$crop="soybean"
datamaize_i$crop="maize"
datawheat_i$crop="wheat"

data_unc=rbind(datasoy_unc,datamaize_unc,datawheat_unc)
data_r=rbind(datasoy_r,datamaize_r,datawheat_r)
data_i=rbind(datasoy_i,datamaize_i,datawheat_i)

data=merge(data_unc,data_r,by=c("FIPS","crop"),all=T)
data=merge(data,data_i,by=c("FIPS","crop"),all=T)

data=data[c("FIPS","crop","X2008.17.x","X2008.17.y","X2008.17")]
colnames(data)<- c("FIPS","crop","X2008.17.unc","X2008.17.r","X2008.17.i")

#import country code
code_county=read.csv("County_code.csv",sep=";")
code_states=read.csv("code_states.csv",sep=";")
code_states$State_name=str_to_title(code_states$State_name, locale = "en")
code_county=merge(code_county,code_states,by="State")
#merge both
data=merge(data,code_county, by=c("FIPS"))
    #import columns usefull from excel Ya####
buffers_soy=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_soy$crop="soybean"
buffers_maize=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_maize$crop="maize"
buffers_wheat=read.csv("Ya_buffers_wheat.csv",sep=";")
buffers_wheat$crop="wheat"
buffers_wheat$I_R="R"
buffers=rbind.fill(buffers_soy,buffers_maize,buffers_wheat)
#get the same name than "COU" column
data$COU=paste0(data$State_name,data$County_name)
#merge all
data=merge(buffers,data,by=c("crop","COU"))

    #account for Rainfed and irri####
data$Yg2008.2017.r=ifelse(is.na(data$X2008.17.r),data$X2008.17.unc,data$X2008.17.r)
data$Yg2008.2017.i=ifelse(is.na(data$X2008.17.i),data$X2008.17.unc,data$X2008.17.i)

    #weighted based on percent max area = get Yg per WS####
data$Yg2008.2017corr.r=data$Yg2008.2017.r*data$Percent_Max.Area
data$Yg2008.2017corr.i=data$Yg2008.2017.i*data$Percent_Max.Area

Yg_Kucharik=ddply(data, ~crop*BUFFER*I_R, summarise,
                Yg2008.2017.r = sum(Yg2008.2017corr.r,na.rm=T),
                Yg2008.2017.i = sum(Yg2008.2017corr.i,na.rm=T),
                correct.error=sum(Percent_Max.Area,na.rm=T))

#correct error on Rainfed Percent_Max.Area that was divided by two
Yg_Kucharik$Yg2008.2017.r=ifelse(Yg_Kucharik$correct.error<1,
            Yg_Kucharik$Yg2008.2017.r/Yg_Kucharik$correct.error,Yg_Kucharik$Yg2008.2017.r)
Yg_Kucharik$Yg2008.2017.i=ifelse(Yg_Kucharik$correct.error<1,
            Yg_Kucharik$Yg2008.2017.i/Yg_Kucharik$correct.error,Yg_Kucharik$Yg2008.2017.i)
Yg_Kucharik$correct.error=NULL


    #merge with latlong####
Latlong=read.csv("LatLong.csv",sep=",")

Yg_Kucharik$BUFFER=with(Yg_Kucharik,ifelse(crop=="wheat",
                            str_replace_all(Yg_Kucharik$BUFFER,
                                            c(Custar="Custar OH",Dazey="Dazey ND",
                                              Hay="Hays",Lisbon="Lisbon ND" 
                                              ,Pierre="Pierre SD",Wooster="Wooster OH")),BUFFER))

Yg_Kucharik=merge(Yg_Kucharik,latlong,by.x=c("BUFFER"),by.y=c("STATIONNAME"),all=T)


#go from GYGA Ya + Yg = Yw

    #import YA data####
data_rainfed=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
data_rainfed=subset(data_rainfed,HARVESTYEAR<2018)
data_rainfed=subset(data_rainfed,HARVESTYEAR>2006)
Ya_rainfed=ddply(data_rainfed, ~CROP*STATIONNAME, summarise,
           Ya_R= mean(YA,na.rm=T))
Ya_rainfed$I_R="R"
Ya_rainfed$CROP=gsub("Rainfed ", "", Ya_rainfed$CROP) 

Yg_Kucharik=merge(Yg_Kucharik,Ya_rainfed,by.x=c("crop","BUFFER","I_R"),by.y=c("CROP","STATIONNAME","I_R"),all=T)

Yg_Kucharik$Yw2008.2017.r=Yg_Kucharik$Ya_R-Yg_Kucharik$Yg2008.2017.r
Yg_Kucharik$Yw2008.2017.i=Yg_Kucharik$Ya_R-Yg_Kucharik$Yg2008.2017.i

Yg_Kucharik$Yg2008.2017.r=-Yg_Kucharik$Yg2008.2017.r
Yg_Kucharik$Yg2008.2017.i=-Yg_Kucharik$Yg2008.2017.i

    #merge YW Kucharik with GYGA data####
latlong=read.csv("LatLong.csv",sep=",")
dataGYGA=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
dataGYGA=merge(dataGYGA,latlong,by.x=c("STATIONNAME"))

Yg_Kucharik$crop_IR=paste0(Yg_Kucharik$crop,"_",Yg_Kucharik$I_R)
Yg_Kucharik$crop_IR=with(Yg_Kucharik,str_replace_all(Yg_Kucharik$crop_IR,
                                       c(maize_R="Rainfed maize",maize_I="Irrigated maize",
                                         soybean_R="Rainfed soybean",soybean_I="Irrigated soybean",
                                         wheat_R="Rainfed wheat")))

dataGYGA=ddply(dataGYGA, ~CROP*STATIONNAME, summarise,
              Yw_GYGA = mean(YW,na.rm=T),
              Ya_GYGA = mean(YA,na.rm=T))
dataGYGA$Yg_GYGA=dataGYGA$Yw_GYGA-dataGYGA$Ya_GYGA

dataKucharik=merge(dataGYGA,Yg_Kucharik,by.x=c("STATIONNAME","CROP"),by.y=c("BUFFER","crop_IR"),all=T)

#keep only buffers with values when GYGA !=NA
dataKucharik=subset(dataKucharik,!is.na(Yg_GYGA))


    #prep graph for Kucharik comparison####


dataKucharik=dataKucharik[c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yg2008.2017.r","LONGITUDE","LATITUDE","Yw2008.2017.r")]
colnames(dataKucharik)<- c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yg_Kucharik_r","LONGITUDE","LATITUDE","Yw_Kucharik_r")


dataKucharik<- melt(dataKucharik, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataKucharik[] <- lapply(dataKucharik, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataKucharik$value =as.numeric(as.character(dataKucharik$value))
dataKucharik$LONGITUDE =as.numeric(as.character(dataKucharik$LONGITUDE))
dataKucharik$LATITUDE =as.numeric(as.character(dataKucharik$LATITUDE))


#3)Mueller method####
    #download shapefile and raster data####
library(raster)
setwd(paste0(inputs.wd,"/Mueller_method"))

admin1<- readOGR("USA_adm1.shp")

#maize
maize_yg=raster("maize_yieldgap.tif")
#v1.1=aggregate(v1.1,fact=1)
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
maize_yg <- crop(maize_yg, e)
#plot(maize_yg)

maize_yp=raster("maize_yieldpotential.tif")
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
maize_yp <- crop(maize_yp, e)
plot(maize_yp)
plot(admin1,add=T)

soybean_yg=raster("soybean_yieldgap.tif")
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
soybean_yg <- crop(soybean_yg, e)
#plot(soybean_yg)

soybean_yp=raster("soybean_yieldpotential.tif")
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
soybean_yp <- crop(soybean_yp, e)
plot(soybean_yp)
plot(admin1,add=T)

wheat_yg=raster("wheat_yieldgap1.tif")
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
wheat_yg <- crop(wheat_yg, e)
#plot(wheat_yg)

wheat_yp=raster("wheat_yieldpotential.tif")
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
wheat_yp <- crop(wheat_yp, e)
#plot(wheat_yp)
plot(admin1,add=T)

# load shapefile data
admin2<- readOGR("USA_adm2.shp")
#plot(admin2)

    #extract from raster to shp Yp####
m_yp=raster::extract(maize_yp,admin2, weights=T, fun=mean,na.rm=T)
m_yg=raster::extract(maize_yg,admin2, weights=T, fun=mean,na.rm=T)
s_yp=raster::extract(soybean_yp,admin2, weights=T, fun=mean,na.rm=T)
s_yg=raster::extract(soybean_yg,admin2, weights=T, fun=mean,na.rm=T)
w_yp=raster::extract(wheat_yp,admin2, weights=T, fun=mean,na.rm=T)
w_yg=raster::extract(wheat_yg,admin2, weights=T, fun=mean,na.rm=T)


#centroids$chmMaxShape <- raster::extract(chm, centShape, weights=FALSE, fun=max)
admin2=data.frame(admin2)

admin2_maize <- cbind(admin2,m_yp,m_yg)
admin2_maize$crop="maize"
admin2_soybean <- cbind(admin2,s_yp,s_yg)
admin2_soybean$crop="soybean"
admin2_wheat <- cbind(admin2,w_yp,w_yg)
admin2_wheat$crop="wheat"

remove(m_yp,m_yg,s_yp,s_yg,w_yp,w_yg)

y_mueller=rbind.fill(admin2_maize,setnames(admin2_soybean,names(admin2_maize)),
                                  setnames(admin2_wheat,names(admin2_maize)))
names(y_mueller)[names(y_mueller) == 'm_yp'] <- 'yp_mueller'
names(y_mueller)[names(y_mueller) == 'm_yg'] <- 'yg_mueller'

y_mueller$COU=paste0(y_mueller$NAME_1,y_mueller$NAME_2)

    #save as a r object after raster extract####
#saveRDS(y_mueller,paste0(outputs.wd,"/y_mueller.RDS"))
y_mueller <- readRDS(paste0(outputs.wd,"/y_mueller.RDS"))

    #import columns usefull from excel Ya####
setwd(inputs.wd)
buffers_soy=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_soy$crop="soybean"
buffers_maize=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_maize$crop="maize"
buffers_wheat=read.csv("Ya_buffers_wheat.csv",sep=";")
buffers_wheat$crop="wheat"
buffers_wheat$I_R="R"
buffers=rbind.fill(buffers_soy,buffers_maize,buffers_wheat)
#get the same name than "COU" column
data$COU=paste0(data$State_name,data$County_name)
#merge all
data=merge(buffers,y_mueller,by=c("crop","COU"))

#remove I? 

data=subset(data,I_R!="I")

    #weighted based on percent max area = get Yg per WS####
data$Ygcorr=data$yg_mueller*data$Percent_Max.Area
data$Ypcorr=data$yp_mueller*data$Percent_Max.Area

#correct error on Rainfed Percent_Max.Area that was divided by two
Y_Mueller=ddply(data, ~crop*BUFFER, summarise,
                  Yg = sum(Ygcorr,na.rm=T),
                  Yp = sum(Ypcorr,na.rm=T),
                  correct.error=sum(Percent_Max.Area,na.rm=T))

Y_Mueller$Yg=ifelse(Y_Mueller$correct.error<1,Y_Mueller$Yg/Y_Mueller$correct.error,Y_Mueller$Yg)
Y_Mueller$Yp=ifelse(Y_Mueller$correct.error<1,Y_Mueller$Yp/Y_Mueller$correct.error,Y_Mueller$Yp)
Y_Mueller$correct.error=NULL

    #merge with latlong####
Latlong=read.csv("LatLong.csv",sep=",")

Y_Mueller$BUFFER=with(Y_Mueller,ifelse(crop=="wheat",
                                           str_replace_all(Y_Mueller$BUFFER,
                                                           c(Custar="Custar OH",Dazey="Dazey ND",
                                                             Hay="Hays",Lisbon="Lisbon ND" 
                                                             ,Pierre="Pierre SD",Wooster="Wooster OH")),BUFFER))

Y_Mueller=merge(Y_Mueller,Latlong,by.x=c("BUFFER"),by.y=c("STATIONNAME"),all=T)

    #get Ya from 1997 to 2003####
source(paste0(rScripts.wd,"/0.get_40y_Ya.R"))

data=subset(data,Year>1996)
data=subset(data,Year<2004)

data=subset(data,crop_IR!="soybean_I")
data=subset(data,crop_IR!="maize_I")

data$crop_IR=with(data,str_replace_all(data$crop_IR,
                                                 c(maize_R="Rainfed maize",
                                                   soybean_R="Rainfed soybean",
                                                   wheat_R="Rainfed wheat")))


    #merge Y Mueller with GYGA data####
latlong=read.csv("LatLong.csv",sep=",")
dataGYGA=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
dataGYGA=merge(dataGYGA,latlong,by.x=c("STATIONNAME"))

dataGYGA=merge(dataGYGA,data,by.x=c("STATIONNAME","CROP",'HARVESTYEAR'),by.y=c("BUFFER","crop_IR","Year"),all=T)

dataGYGA_9703=subset(dataGYGA,HARVESTYEAR>1996)
dataGYGA_9703=subset(dataGYGA,HARVESTYEAR<2004)

dataGYGA_9703=ddply(dataGYGA, ~CROP*STATIONNAME, summarise,
               Yw_GYGA = mean(YW,na.rm=T),
               Ya_GYGA = mean(Ya,na.rm=T))
dataGYGA_9703$Yg_GYGA=dataGYGA_9703$Yw_GYGA-dataGYGA_9703$Ya_GYGA

# Y_Mueller$crop_IR=paste0(Y_Mueller$crop,"_",Y_Mueller$I_R)
Y_Mueller$crop_IR=with(Y_Mueller,str_replace_all(Y_Mueller$crop,
                                                 c(maize="Rainfed maize",
                                                   soybean="Rainfed soybean",
                                                   wheat="Rainfed wheat")))

# Y_Mueller$crop_IR=with(Y_Mueller,str_replace_all(Y_Mueller$crop_IR,
#                                                      c(maize_R="Rainfed maize",maize_I="Irrigated maize",
#                                                        soybean_R="Rainfed soybean",soybean_I="Irrigated soybean",
#                                                        wheat_R="Rainfed wheat")))


dataMueller=merge(dataGYGA_9703,Y_Mueller,by.x=c("STATIONNAME","CROP"),
                  by.y=c("BUFFER","crop_IR"),all.y=T)

names(dataMueller)[names(dataMueller) == 'Yp'] <- 'Yw'

#keep only buffers with values when GYGA !=NA
dataMueller=subset(dataMueller,!is.na(Yg_GYGA))

#get Ya for NASS comparison later
dataMueller_Ya=dataMueller[c("STATIONNAME","CROP","Ya_GYGA")]
names(dataMueller_Ya)[names(dataMueller_Ya) == 'Ya_GYGA'] <- 'Ya_Mueller'

    #prep graph for Mueller comparison####
dataMueller=dataMueller[c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yw","Yg","LONGITUDE","LATITUDE")]
colnames(dataMueller)<- c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yw_Mueller","Yg_Mueller","LONGITUDE","LATITUDE")


dataMueller<- melt(dataMueller, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataMueller[] <- lapply(dataMueller, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataMueller$value =as.numeric(as.character(dataMueller$value))
dataMueller$LONGITUDE =as.numeric(as.character(dataMueller$LONGITUDE))
dataMueller$LATITUDE =as.numeric(as.character(dataMueller$LATITUDE))


#4)Gerber method####
    #download shapefile and raster data####
library(raster)
setwd("D:/Mes Donnees/from dropbox/USA/Gerber_data/AttainableYieldLayers")

admin1<- readOGR("USA_adm1.shp")

#maize
input_files <- c("AttainableYieldLayer_maize_2003.tif", "AttainableYieldLayer_maize_2004.tif",
                 "AttainableYieldLayer_maize_2005.tif", "AttainableYieldLayer_maize_2006.tif",
                 "AttainableYieldLayer_maize_2007.tif", "AttainableYieldLayer_maize_2008.tif",
                 "AttainableYieldLayer_maize_2009.tif", "AttainableYieldLayer_maize_2010.tif",
                 "AttainableYieldLayer_maize_2011.tif", "AttainableYieldLayer_maize_2012.tif")
rasters <- lapply(input_files, raster)# Load the raster files
raster_stack <- stack(rasters)# Stack the rasters
maize_yp <- calc(raster_stack, fun = mean, na.rm = TRUE)# Calculate the average across all rasters

e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
maize_yp <- crop(maize_yp, e)
# plot(maize_yp)
# plot(admin1,add=T)

#soybean
input_files <- c("AttainableYieldLayer_soybean_2003.tif", "AttainableYieldLayer_soybean_2004.tif",
                 "AttainableYieldLayer_soybean_2005.tif", "AttainableYieldLayer_soybean_2006.tif",
                 "AttainableYieldLayer_soybean_2007.tif", "AttainableYieldLayer_soybean_2008.tif",
                 "AttainableYieldLayer_soybean_2009.tif", "AttainableYieldLayer_soybean_2010.tif",
                 "AttainableYieldLayer_soybean_2011.tif", "AttainableYieldLayer_soybean_2012.tif")
rasters <- lapply(input_files, raster)# Load the raster files
raster_stack <- stack(rasters)# Stack the rasters
soybean_yp <- calc(raster_stack, fun = mean, na.rm = TRUE)# Calculate the average across all rasters
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
soybean_yp <- crop(soybean_yp, e)
# plot(soybean_yp)
# plot(admin1,add=T)

#wheat
input_files <- c("AttainableYieldLayer_wheat_2003.tif", "AttainableYieldLayer_wheat_2004.tif",
                 "AttainableYieldLayer_wheat_2005.tif", "AttainableYieldLayer_wheat_2006.tif",
                 "AttainableYieldLayer_wheat_2007.tif", "AttainableYieldLayer_wheat_2008.tif",
                 "AttainableYieldLayer_wheat_2009.tif", "AttainableYieldLayer_wheat_2010.tif",
                 "AttainableYieldLayer_wheat_2011.tif", "AttainableYieldLayer_wheat_2012.tif")
rasters <- lapply(input_files, raster)# Load the raster files
raster_stack <- stack(rasters)# Stack the rasters
wheat_yp <- calc(raster_stack, fun = mean, na.rm = TRUE)# Calculate the average across all rasters
e <- as(extent(-130, -60, 20, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
wheat_yp <- crop(wheat_yp, e)
# plot(wheat_yp)
# plot(admin1,add=T)

# load shapefile data
admin2<- readOGR("USA_adm2.shp")
#plot(admin2)

    #extract from raster to shp Yp####
m_yp=raster::extract(maize_yp,admin2, weights=T, fun=mean,na.rm=T)
s_yp=raster::extract(soybean_yp,admin2, weights=T, fun=mean,na.rm=T)
w_yp=raster::extract(wheat_yp,admin2, weights=T, fun=mean,na.rm=T)

# Remove zero values from the extracted data
m_yp <- lapply(m_yp, function(x) x[x != 0])
s_yp <- lapply(s_yp, function(x) x[x != 0])
w_yp <- lapply(w_yp, function(x) x[x != 0])


#centroids$chmMaxShape <- raster::extract(chm, centShape, weights=FALSE, fun=max)
admin2=data.frame(admin2)
# m_yp_df=data.frame(m_yp)
# s_yp_df=data.frame(s_yp)
# w_yp_df=data.frame(w_yp)

admin2_maize <- cbind(admin2,m_yp)
admin2_maize$crop="maize"
admin2_soybean <- cbind(admin2,s_yp)
admin2_soybean$crop="soybean"
admin2_wheat <- cbind(admin2,w_yp)
admin2_wheat$crop="wheat"

remove(m_yp,m_yg,s_yp,s_yg,w_yp,w_yg)

y_gerber=rbind.fill(admin2_maize,setnames(admin2_soybean,names(admin2_maize)),
                                  setnames(admin2_wheat,names(admin2_maize)))
names(y_gerber)[names(y_gerber) == 'm_yp'] <- 'yp_gerber'
#names(y_gerber)[names(y_gerber) == 'm_yg'] <- 'yg_gerber'

y_gerber$COU=paste0(y_gerber$NAME_1,y_gerber$NAME_2)

    #save as a r object after raster extract####
#saveRDS(y_gerber,paste0(outputs.wd,"/y_gerber.RDS"))
y_gerber <- readRDS(paste0(outputs.wd,"/y_gerber.RDS"))

    #import columns usefull from excel Ya####
setwd(inputs.wd)
buffers_soy=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_soy$crop="soybean"
buffers_maize=read.csv("Ya_buffers_soybean.csv",sep=";")
buffers_maize$crop="maize"
buffers_wheat=read.csv("Ya_buffers_wheat.csv",sep=";")
buffers_wheat$crop="wheat"
buffers_wheat$I_R="R"
buffers=rbind.fill(buffers_soy,buffers_maize,buffers_wheat)
#get the same name than "COU" column
data$COU=paste0(data$State_name,data$County_name)
#merge all
data=merge(buffers,y_gerber,by=c("crop","COU"))

#remove I? 

data=subset(data,I_R!="I")

    #weighted based on percent max area = get Yg per WS####
#data$Ygcorr=data$yg_mueller*data$Percent_Max.Area
data$Ypcorr=data$yp_gerber*data$Percent_Max.Area

#correct error on Rainfed Percent_Max.Area that was divided by two
Y_Gerber=ddply(data, ~crop*BUFFER, summarise,
                #Yg = sum(Ygcorr,na.rm=T),
                Yp = sum(Ypcorr,na.rm=T),
                correct.error=sum(Percent_Max.Area,na.rm=T))

#Y_Gerber$Yg=ifelse(Y_Gerber$correct.error<1,Y_Gerber$Yg/Y_Gerber$correct.error,Y_Gerber$Yg)
Y_Gerber$Yp=ifelse(Y_Gerber$correct.error<1,Y_Gerber$Yp/Y_Gerber$correct.error,Y_Gerber$Yp)
Y_Gerber$correct.error=NULL

    #merge with latlong####
Latlong=read.csv("LatLong.csv",sep=",")

Y_Gerber$BUFFER=with(Y_Gerber,ifelse(crop=="wheat",
                                       str_replace_all(Y_Gerber$BUFFER,
                                                       c(Custar="Custar OH",Dazey="Dazey ND",
                                                         Hay="Hays",Lisbon="Lisbon ND" 
                                                         ,Pierre="Pierre SD",Wooster="Wooster OH")),BUFFER))

Y_Gerber=merge(Y_Gerber,Latlong,by.x=c("BUFFER"),by.y=c("STATIONNAME"),all=T)

    #get Ya from 2003 to 2012####
source(paste0(rScripts.wd,"/0.get_40y_Ya.R"))

data=subset(data,Year>2002)
data=subset(data,Year<2013)

data=subset(data,crop_IR!="soybean_I")
data=subset(data,crop_IR!="maize_I")

data$crop_IR=with(data,str_replace_all(data$crop_IR,
                                       c(maize_R="Rainfed maize",
                                         soybean_R="Rainfed soybean",
                                         wheat_R="Rainfed wheat")))


    #merge Y Gerber with GYGA data####
latlong=read.csv("LatLong.csv",sep=",")
dataGYGA=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
dataGYGA=merge(dataGYGA,latlong,by.x=c("STATIONNAME"))

dataGYGA=merge(dataGYGA,data,by.x=c("STATIONNAME","CROP",'HARVESTYEAR'),by.y=c("BUFFER","crop_IR","Year"),all=T)

dataGYGA_0312=subset(dataGYGA,HARVESTYEAR>2002)
dataGYGA_0312=subset(dataGYGA,HARVESTYEAR<2013)

dataGYGA_0312=ddply(dataGYGA, ~CROP*STATIONNAME, summarise,
                    Yw_GYGA = mean(YW,na.rm=T),
                    Ya_GYGA = mean(Ya,na.rm=T))
dataGYGA_0312$Yg_GYGA=dataGYGA_0312$Yw_GYGA-dataGYGA_0312$Ya_GYGA

#added by AC 19/09/2024 to get Ya for Gerber for Yg calculation (as Gerber do not provide Yg)

# Y_Gerber$crop_IR=paste0(Y_Gerber$crop,"_",Y_Gerber$I_R)
Y_Gerber$crop_IR=with(Y_Gerber,str_replace_all(Y_Gerber$crop,
                                                 c(maize="Rainfed maize",
                                                   soybean="Rainfed soybean",
                                                   wheat="Rainfed wheat")))

# Y_Gerber$crop_IR=with(Y_Gerber,str_replace_all(Y_Gerber$crop_IR,
#                                                      c(maize_R="Rainfed maize",maize_I="Irrigated maize",
#                                                        soybean_R="Rainfed soybean",soybean_I="Irrigated soybean",
#                                                        wheat_R="Rainfed wheat")))


dataGerber=merge(dataGYGA_0312,Y_Gerber,by.x=c("STATIONNAME","CROP"),
                  by.y=c("BUFFER","crop_IR"),all.y=T)

names(dataGerber)[names(dataGerber) == 'Yp'] <- 'Yw'

#added by AC 19/09/2024 to get Ya for Gerber for Yg calculation (as Gerber do not provide Yg)
dataGerber$Yg=dataGerber$Yw-dataGerber$Ya_GYGA

#keep only buffers with values when GYGA !=NA
dataGerber=subset(dataGerber,!is.na(Yg_GYGA))

#get Ya for NASS comparison later
dataGerber_Ya=dataGerber[c("STATIONNAME","CROP","Ya_GYGA")]
names(dataGerber_Ya)[names(dataGerber_Ya) == 'Ya_GYGA'] <- 'Ya_Gerber'

    #prep graph for Gerber comparison####
dataGerber=dataGerber[c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yw","Yg","LONGITUDE","LATITUDE")]
colnames(dataGerber)<- c("STATIONNAME","CROP","Yw_GYGA","Yg_GYGA","Yw_Gerber","Yg_Gerber","LONGITUDE","LATITUDE")


dataGerber<- melt(dataGerber, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataGerber[] <- lapply(dataGerber, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataGerber$value =as.numeric(as.character(dataGerber$value))
dataGerber$LONGITUDE =as.numeric(as.character(dataGerber$LONGITUDE))
dataGerber$LATITUDE =as.numeric(as.character(dataGerber$LATITUDE))

#remove data from Gerber due to irrigated area accounted for + uncomplete map

dataGerber=dataGerber %>% filter(!(STATIONNAME == "Dazey ND" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Lisbon ND" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Holdrege NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Nemaha NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "North platte NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Clay Center NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Mccook NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Scandia KS" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Beatrice NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Concord NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Mead NE" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Silverlake KS" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Manhattan KS" & CROP == "Rainfed soybean"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Hutchinson KS" & CROP == "Rainfed soybean"))

dataGerber=dataGerber %>% filter(!(STATIONNAME == "Dazey ND" & CROP == "Rainfed maize"))
#dataGerber=dataGerber %>% filter(!(STATIONNAME == "Lisbon ND" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Holdrege NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Nemaha NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "North platte NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Clay Center NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Mccook NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Scandia KS" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Beatrice NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Concord NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Mead NE" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Silverlake KS" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Manhattan KS" & CROP == "Rainfed maize"))
dataGerber=dataGerber %>% filter(!(STATIONNAME == "Hutchinson KS" & CROP == "Rainfed maize"))

#5) Merge all methods####
    #prep GYGA data####
latlong=read.csv("LatLong.csv",sep=",")
dataGYGA=read.csv("Station Year Maize Soybean Wheat.csv",sep=";")
dataGYGA=merge(dataGYGA,latlong,by.x=c("STATIONNAME"))

dataGYGA_year=dataGYGA

dataGYGA2=ddply(dataGYGA, ~CROP*STATIONNAME, summarise,
              LONGITUDE=mean(LONGITUDE,na.rm=T),
              LATITUDE=mean(LATITUDE,na.rm=T),
              Yw_GYGA = mean(YW,na.rm=T),
              Ya_GYGA = mean(YA,na.rm=T))

dataGYGA2$Yg_GYGA=dataGYGA2$Yw_GYGA-dataGYGA2$Ya_GYGA

dataGYGA2_year=ddply(dataGYGA_year, ~CROP*STATIONNAME*HARVESTYEAR, summarise,
                LONGITUDE=mean(LONGITUDE,na.rm=T),
                LATITUDE=mean(LATITUDE,na.rm=T),
                Yw_GYGA = mean(YW,na.rm=T),
                Ya_GYGA = mean(YA,na.rm=T))

dataGYGA2_year$Yg_GYGA=dataGYGA2_year$Yw_GYGA-dataGYGA2_year$Ya_GYGA

#keep only buffers with values when GYGA !=NA
dataGYGA2=subset(dataGYGA2,!is.na(Yg_GYGA))

dataGYGA2<- melt(dataGYGA2, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataGYGA2[] <- lapply(dataGYGA2, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataGYGA2$value =as.numeric(as.character(dataGYGA2$value))
dataGYGA2$LONGITUDE =as.numeric(as.character(dataGYGA2$LONGITUDE))
dataGYGA2$LATITUDE =as.numeric(as.character(dataGYGA2$LATITUDE))

dataGYGA2$method="GYGA"


dataGYGA2_year=subset(dataGYGA2_year,!is.na(Yg_GYGA))

dataGYGA2_year<- melt(dataGYGA2_year, id=c("CROP","STATIONNAME","LONGITUDE","LATITUDE"))
dataGYGA2_year[] <- lapply(dataGYGA2_year, gsub, pattern = "NaN", replacement = NA, fixed = TRUE)
dataGYGA2_year$value =as.numeric(as.character(dataGYGA2_year$value))
dataGYGA2_year$LONGITUDE =as.numeric(as.character(dataGYGA2_year$LONGITUDE))
dataGYGA2_year$LATITUDE =as.numeric(as.character(dataGYGA2_year$LATITUDE))

dataGYGA2_year$method="GYGA"


    #prep other methods####
dataHat2=subset(dataHat,str_sub(variable,nchar(variable)-3,nchar(variable))!="GYGA")
dataHat2_year=subset(dataHat_year,str_sub(variable,nchar(variable)-3,nchar(variable))!="GYGA")
dataKucharik2=subset(dataKucharik,str_sub(variable,nchar(variable)-3,nchar(variable))!="GYGA")
dataMueller2=subset(dataMueller,str_sub(variable,nchar(variable)-3,nchar(variable))!="GYGA")
dataGerber2=subset(dataGerber,str_sub(variable,nchar(variable)-3,nchar(variable))!="GYGA")

dataHat2$method="Hatfield"
dataHat2_year$method="Hatfield"
dataKucharik2$method="Kucharik"
dataMueller2$method="Mueller"
dataGerber2$method="Gerber"

    #merge all####
dataAll=rbind(dataGYGA2,dataHat2,dataKucharik2,dataMueller2,dataGerber2)
dataAll_year=rbind(dataGYGA2_year,dataHat2_year)

dataAll=subset(dataAll,str_sub(CROP,1,1)=="R") 
dataAll_year=subset(dataAll_year,str_sub(CROP,1,1)=="R") 

#6)Figure 3 and Extended data Figure 4 - MAPS####
source(paste0(rScripts.wd,"/2.maps.R"))

#7)GYGA~methods scatter plots compararison with NASS####
    #pivot wider Yw and Ya####
dataAll_Yw=subset(dataAll,substr(variable,1,2)=="Yw")
names(dataAll_Yw)[names(dataAll_Yw) == 'value'] <- 'Yw'
dataAll_Yg=subset(dataAll,substr(variable,1,2)=="Yg")
names(dataAll_Yg)[names(dataAll_Yg) == 'value'] <- 'Yg'
#dataAll_Ya=subset(dataAll,substr(variable,1,2)=="Ya")
#names(dataAll_Ya)[names(dataAll_Ya) == 'value'] <- 'Ya'


dataAll_large=merge(dataAll_Yw,dataAll_Yg,by=c("CROP","STATIONNAME","LONGITUDE","LATITUDE","method"),all.x=T)

dataAll_large$Ya=dataAll_large$Yw-dataAll_large$Yg

    #incoporated data_Ya_Mueller NASS from 1997 to 2003####
dataAll_large=merge(dataAll_large,dataMueller_Ya,by=c("CROP","STATIONNAME"),all.x=T)

dataAll_large$Ya=ifelse(dataAll_large$method=="Mueller",dataAll_large$Ya_Mueller,dataAll_large$Ya)

    #incoporated data_Ya_Gerber NASS from 2003 to 2012####
dataAll_large=merge(dataAll_large,dataGerber_Ya,by=c("CROP","STATIONNAME"),all.x=T)

dataAll_large$Ya=ifelse(dataAll_large$method=="Gerber",dataAll_large$Ya_Gerber,dataAll_large$Ya)

    #add US average####
CZ=unique(dataGYGA[c("CROP","STATIONNAME","CLIMATEZONE")])

dataAll_large=merge(dataAll_large,CZ,by=c("CROP","STATIONNAME"),all.x=T)

#get the CV for table
dataAll_large_CV=ddply(dataAll_large, ~CROP*method, summarise,
                       Yw_cv = cv(Yw,na.rm=T)*100,
                       Yg_cv = cv(Yg,na.rm=T)*100,
                       Ya_cv = cv(Ya,na.rm=T)*100)

# dataAll_large_CV$method=with(dataAll_large_CV,str_replace_all(dataAll_large_CV$method,
#                                              c(GYGA="Crop models",Hatfield="LQ",
#                                               Kucharik="RQ",Mueller="CB")))

#bring all area crops for stations
area_buffer=read.csv(paste0(inputs.wd,"/area_buffer.csv"),sep=";")

#weight per CZ
dataAll_large=merge(dataAll_large,area_buffer,by=c("STATIONNAME","CROP"),all.x=T)

data_max=ddply(dataAll_large, ~CROP*CLIMATEZONE*method, summarise,
                  sum_AREA_IN_CLIMATEZONE_HA = sum(AREA_IN_CLIMATEZONE_HA,na.rm=T))

dataAll_large=merge(dataAll_large,data_max,by=c("CROP","CLIMATEZONE","method"),all.x=T)

dataAll_large$percMax_AREA_IN_CZ=dataAll_large$AREA_IN_CLIMATEZONE_HA/dataAll_large$sum_AREA_IN_CLIMATEZONE_HA

dataAll_large$Yw_perc=dataAll_large$Yw*dataAll_large$percMax_AREA_IN_CZ
dataAll_large$Yg_perc=dataAll_large$Yg*dataAll_large$percMax_AREA_IN_CZ
dataAll_large$Ya_perc=dataAll_large$Ya*dataAll_large$percMax_AREA_IN_CZ

dataAll_large[dataAll_large==0] <- NA

dataAll_large_CZ=ddply(dataAll_large, ~CROP*CLIMATEZONE*method, summarise,
               Yw = sum(Yw_perc,na.rm=T),
               Yg = sum(Yg_perc,na.rm=T),
               Ya = sum(Ya_perc,na.rm=T))

dataAll_large_CZ[dataAll_large_CZ==0] <- NA

#bring all CZ area
area_cz=read.csv(paste0(inputs.wd,"/area_cz.csv"),sep=";")

#weight by US
dataAll_large_CZ=merge(dataAll_large_CZ,area_cz,by=c("CLIMATEZONE","CROP"),all.x=T)

dataAll_large_CZ$AREA_SELECTED_STATIONS_HA=NULL

data_max=ddply(dataAll_large_CZ, ~CROP*method, summarise,
               sum_AREA_IN_USA_HA = sum(TOTAL_AREA_HA,na.rm=T))

dataAll_large_CZ=merge(dataAll_large_CZ,data_max,by=c("CROP","method"),all.x=T)

dataAll_large_CZ$percMax_AREA_IN_USA=dataAll_large_CZ$TOTAL_AREA_HA/dataAll_large_CZ$sum_AREA_IN_USA_HA

dataAll_large_CZ$Yw_perc=dataAll_large_CZ$Yw*dataAll_large_CZ$percMax_AREA_IN_USA
dataAll_large_CZ$Yg_perc=dataAll_large_CZ$Yg*dataAll_large_CZ$percMax_AREA_IN_USA
dataAll_large_CZ$Ya_perc=dataAll_large_CZ$Ya*dataAll_large_CZ$percMax_AREA_IN_USA

dataAll_large_CZ[dataAll_large_CZ==0] <- NA

dataAll_large_USA=ddply(dataAll_large_CZ, ~CROP*method, summarise,
                       Yw = sum(Yw_perc,na.rm=T),
                       Yg = sum(Yg_perc,na.rm=T),
                       Ya = sum(Ya_perc,na.rm=T))

dataAll_large_CZ[dataAll_large_CZ==0] <- NA

#get Yg% for the text
dataAll_large_USA$Yg_PERC=dataAll_large_USA$Yg/dataAll_large_USA$Yw*100

#export table for paper

dataAll_large_USA_table=merge(dataAll_large_USA,dataAll_large_CV,by=c("CROP","method"))
dataAll_large_USA_table=dataAll_large_USA_table[c("CROP","method","Yw","Yg_PERC","Yw_cv","Yg_cv")]
dataAll_large_USA_table$Yw=round(dataAll_large_USA_table$Yw,1)
dataAll_large_USA_table$Yg_PERC=round(dataAll_large_USA_table$Yg_PERC,0)
dataAll_large_USA_table$Yw_cv=round(dataAll_large_USA_table$Yw_cv,0)
dataAll_large_USA_table$Yg_cv=round(dataAll_large_USA_table$Yg_cv,0)
dataAll_large_USA_table=pivot_wider(dataAll_large_USA_table,names_from = "CROP", values_from = c('Yw',"Yw_cv",'Yg_PERC',"Yg_cv"))
dataAll_large_USA_table_mean=dataAll_large_USA_table[c("method","Yw_Rainfed soybean","Yg_PERC_Rainfed soybean",
                                                  "Yw_Rainfed maize","Yg_PERC_Rainfed maize",
                                                  "Yw_Rainfed wheat","Yg_PERC_Rainfed wheat")]
dataAll_large_USA_table_mean$stat="mean"
colnames(dataAll_large_USA_table_mean)<- c("method","Yw_soy","Yg_soy","Yw_maize","Yg_maize","Yw_wheat","Yg_wheat")
dataAll_large_USA_table_cv=dataAll_large_USA_table[c("method","Yw_cv_Rainfed soybean","Yg_cv_Rainfed soybean",
                                                  "Yw_cv_Rainfed maize","Yg_cv_Rainfed maize",
                                                  "Yw_cv_Rainfed wheat","Yg_cv_Rainfed wheat")]

dataAll_large_USA_table_cv$stat="cv"
colnames(dataAll_large_USA_table_cv)<- c("method","Yw_soy","Yg_soy","Yw_maize","Yg_maize","Yw_wheat","Yg_wheat")

dataAll_large_USA_table=rbind(dataAll_large_USA_table_mean,dataAll_large_USA_table_cv)


library("writexl")
write_xlsx(dataAll_large_USA_table,paste0(outputs.wd,"//Table1.xlsx"))

    #calcul to get the differences between GYGA and other####
#across all crops
methods=subset(dataAll_large_USA,method!="GYGA")
gyga=subset(dataAll_large_USA,method=="GYGA")

methods=ddply(methods, ~method, summarise,
                        Yw = mean(Yw,na.rm=T),
                        Yg = mean(Yg,na.rm=T),
                        Ya = mean(Ya,na.rm=T))
methods$type="All"

gyga=ddply(gyga, ~method, summarise,
              Yw = mean(Yw,na.rm=T),
              Yg = mean(Yg,na.rm=T),
              Ya = mean(Ya,na.rm=T))

gyga$type="All"

methods=merge(methods,gyga,by=c("type"))

methods$Yw_percdiff=methods$Yw.x/methods$Yw.y
methods$Yg_percdiff=methods$Yg.x/methods$Yg.y

methods$Yg_perc.x=methods$Yg.x/methods$Yw.x
methods$Yg_perc.y=methods$Yg.y/methods$Yw.y

remove(gyga, data_max,dataAll_large_CZ)

#across methods and crops
methods=subset(dataAll_large_USA,method!="GYGA")
gyga=subset(dataAll_large_USA,method=="GYGA")

methods=merge(methods,gyga,by=c("CROP"))

methods$Yw_percdiff=methods$Yw.x/methods$Yw.y
methods$Yg_percdiff=methods$Yg.x/methods$Yg.y

methods$Yg_perc.x=methods$Yg.x/methods$Yw.x
methods$Yg_perc.y=methods$Yg.y/methods$Yw.y


remove(gyga,data_max,dataAll_large_CZ_USA)

    #Figure 2####

#levels and methods nanmes
{
dataAll_large$CROP <- factor(dataAll_large$CROP, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))
dataAll_large_USA$CROP <- factor(dataAll_large_USA$CROP, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))

#methods names
dataAll_large$method=str_replace_all(dataAll_large$method,
                                            c(GYGA="GYGA", Hatfield="LQ",Kucharik="GR",
                                              Mueller="CB",Gerber='GQ'))
dataAll_large_USA$method=str_replace_all(dataAll_large_USA$method,
                                     c(GYGA="GYGA",Hatfield="LQ",Kucharik="GR",
                                       Mueller="CB",Gerber='GQ'))

dataAll_large$method <- factor(dataAll_large$method, levels = c("GYGA", "LQ", "GQ","GR","CB"))
dataAll_large_USA$method <- factor(dataAll_large_USA$method, levels = c("GYGA", "LQ", "GR","CB","GQ"))
}

dataAll_large=dataAll_large %>% filter(!(STATIONNAME == "Wooster OH" & CROP == "Wheat"))

#get Yg% for the text
dataAll_large_USA$Yg_PERC=dataAll_large_USA$Yg/dataAll_large_USA$Yw*100

#remove NA WARNING sites missing?
dataAll_large=subset(dataAll_large,!is.na(dataAll_large$Yw))

#get the RMSE and R_square per plot
source(paste(rScripts.wd , "evaluation.R" , sep = ""))

dataAll_large$CROP_method=paste0(dataAll_large$CROP,"_",dataAll_large$method)
for (k in unique (dataAll_large$CROP_method))
{
  dataAll_large_j=subset(dataAll_large,CROP_method==k)
  t=with(dataAll_large_j,RRMSE(Ya, Yw))
  t=data.frame(CROP_method=k,nRMSE=t)
  e=with(dataAll_large_j,R_square(Ya, Yw))
  e=data.frame(CROP_method=k,R_square=e)
  if(exists("stats") == F) { stats <- t } else { stats <- rbind(stats,t)}
  if(exists("statsR_square") == F) { statsR_square <- e } else { statsR_square <- rbind(statsR_square,e)}
  rm(t,e)
}

nRMSE=stats
R_Square=statsR_square
nRMSE=nRMSE %>% separate(CROP_method, c("CROP", "method"), sep="_")
R_Square=R_Square %>% separate(CROP_method, c("CROP", "method"), sep="_")
nRMSE$method <- factor(nRMSE$method, levels = c("GYGA", "LQ", "GR","CB","GQ"))
R_Square$method <- factor(R_Square$method, levels = c("GYGA", "LQ", "GR","CB",'GQ'))
# nRMSE=pivot_wider(stats,names_from = "variable", values_from = "nRMSE")
# R_square=pivot_wider(statsR_square,names_from = "variable", values_from = "R_square")
remove(stats,statsR_square)

#get the p value
p_values <- dataAll_large %>%
  group_by(CROP,method) %>%
  summarize(p_input = cor.test(Ya, Yw)$p.value)
#p_values$p_value=ifelse(p_values$p_input<0.05,"<0.05",round(p_values$p_input,2))
#p_values$keep_input=ifelse(p_values$p_input <0.05,"yes","no")
p_values$p_input=ifelse(p_values$p_input<0.001,"<0.001",paste0(": ",signif(p_values$p_input, digits=3)))


dataAll_large$CROP=with(dataAll_large,str_replace_all(dataAll_large$CROP,
                                            c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                              `Rainfed wheat`="Wheat")))

dataAll_large_USA$CROP=with(dataAll_large_USA,str_replace_all(dataAll_large_USA$CROP,
                                                      c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                        `Rainfed wheat`="Wheat")))

R_Square$CROP=with(R_Square,str_replace_all(R_Square$CROP,c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                          `Rainfed wheat`="Wheat")))
p_values$CROP=with(p_values,str_replace_all(p_values$CROP,c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                                `Rainfed wheat`="Wheat")))

dataAll_large$CROP <- factor(dataAll_large$CROP, levels = c("Soybean", "Maize", "Wheat"))
dataAll_large_USA$CROP <- factor(dataAll_large_USA$CROP, levels = c("Soybean", "Maize", "Wheat"))
R_Square$CROP <- factor(R_Square$CROP, levels = c("Soybean", "Maize", "Wheat"))
p_values$CROP <- factor(p_values$CROP, levels = c("Soybean", "Maize", "Wheat"))

#add missing data (4 buffers maize)

dataAll_large_new=read.csv(paste0(inputs.wd,"/missing_points.csv"),sep=";")
dataAll_large_new$X=NULL
dataAll_large_new$X.1=NULL
dataAll_large=rbind(dataAll_large,dataAll_large_new)
  
dataAll_large=subset(dataAll_large, STATIONNAME!="Whiteville NC")
dataAll_large=subset(dataAll_large, STATIONNAME!="Draughon Mille TX")

{
shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Figure_2.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 

ggplot(dataAll_large, aes(x = Ya, y = Yw))+
  geom_point(stat = "identity") +
  geom_point(data=dataAll_large_USA,size=6,fill="red",col="red",shape=3,stroke = 2)+#shape=1
  geom_abline(slope=1,intercept=0)+
  #geom_smooth(method="lm")+
  theme(plot.title=element_text(size=15,face="bold.italic"),
        axis.title.y=element_text(size=20,face="bold"),axis.title.x=element_text(size=20,face="bold"),
        axis.text.x=element_text(angle = 0, vjust = 1, hjust = 1, size=15,face="bold"),
        axis.text.y=element_text(size=15,face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),strip.text = element_text(size=20),legend.position="right",
        panel.border = element_rect(fill = NA,colour = "black"),strip.text.x=element_text(size=20),
        strip.placement = "outside",
        #strip.text.y=element_blank(),
        strip.background = element_blank())+
  theme(aspect.ratio = 1)+
  scale_y_continuous(labels=abs)+
  scale_shape_manual(values=c(0,1,2,3,4,0,1,2,3,4,5,6,8))+
  labs(y = expression(paste("Water-limited yield potential (Mg ",ha^-1,")")),
       x = expression(paste("NASS actual yield (Mg ",ha^-1,")")))+
  #facet_grid2(cols=vars(variable), scales = "free", independent = "all",drop=T)
  # +
  geom_text(data = R_Square[R_Square$CROP=="Maize",], aes(x=11, y=8.5, label = paste0('R^2: ',round(R_square,2))
                                                          ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  geom_text(data = p_values[p_values$CROP=="Maize",], aes(x=11, y=6.7, label = paste0('p',p_input)
                                                          ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  # geom_text(data = p_values[p_values$CROP=="Maize",],aes(x=9.8,y=4.7,label = CROP),size=5.5,hjust = 0)+
  geom_text(data = R_Square[R_Square$CROP=="Soybean",], aes(x=4.2, y=3.3, label = paste0('R^2: ',round(R_square,2))
                                                            ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  geom_text(data = p_values[p_values$CROP=="Soybean",], aes(x=4.2, y=2.7, label = paste0('p',p_input)
                                                            ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  # geom_text(data = p_values[p_values$CROP=="Soybean",],aes(x=4,y=2,label = CROP),size=5.5,hjust = 0)+
  geom_text(data = R_Square[R_Square$CROP=="Wheat",], aes(x=6.2, y=4.4, label = paste0('R^2: ',round(R_square,2))
                                                          ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  geom_text(data = p_values[p_values$CROP=="Wheat",], aes(x=6.2, y=3.3, label = paste0('p',p_input)
                                                          ,shape = NULL), hjust = 0,parse=T, size=4.5)+
  # geom_text(data = p_values[p_values$CROP=="Wheat",],aes(x=6,y=2,label = CROP),size=5.5,hjust = 0)+
  facet_grid2(vars(CROP),vars(method), scales = "free",axes = "all",remove_labels="x",independent = "all")+
  #,switch = "y")+
  facetted_pos_scales(
    y = list(
      CROP == "Maize" ~ scale_y_continuous(limits = c(4, 16.5)),
      CROP == "Soybean" ~ scale_y_continuous(limits = c(1.8, 6.1)),
      CROP == "Wheat" ~ scale_y_continuous(limits = c(1.5, 10))),
    x = list(
      CROP == "Maize" ~ scale_x_continuous(limits = c(4, 16.5)),
      CROP == "Soybean" ~ scale_x_continuous(limits = c(1.8, 6.1)),
      CROP == "Wheat" ~ scale_x_continuous(limits = c(1.5, 10))))
dev.off()
dev.off()
shell(file.path(outputs.wd, "Figure_2.pdf"), wait = FALSE)
}

      #table for NFOOD####

dataAll_large_toshare<- dataAll_large %>% select(CROP, method,STATIONNAME, Yw, Ya)
write.csv(dataAll_large_toshare,paste0(outputs.wd,"/Source_data_figure_2.csv"), row.names = FALSE)

#GYGA~methods scatter plots####
      #homogenise databases####
dataHat2=subset(dataHat,CROP!="Irrigated maize")
dataHat2=subset(dataHat2,CROP!="Irrigated soybean")
dataHat2$method='Hatfield'
dataHat2=pivot_wider(dataHat2,names_from = "variable", values_from = "value")
dataHat2$Ya_GYGA=NULL
names(dataHat2)[names(dataHat2) == 'Yw_Hat'] <- 'Yw'
names(dataHat2)[names(dataHat2) == 'Yg_Hat'] <- 'Yg'
dataHat2=dataHat2[c("CROP","STATIONNAME","LONGITUDE","method","Yw_GYGA","Yg_GYGA","Yw","Yg")]


dataKucharik2=subset(dataKucharik,CROP!="Irrigated maize")
dataKucharik2=subset(dataKucharik2,CROP!="Irrigated soybean")
dataKucharik2$method='Kucharik'
dataKucharik2=dataKucharik2[!duplicated(dataKucharik2), ]
dataKucharik2=pivot_wider(dataKucharik2,names_from = "variable", values_from = "value")
names(dataKucharik2)[names(dataKucharik2) == 'Yw_Kucharik_r'] <- 'Yw'
names(dataKucharik2)[names(dataKucharik2) == 'Yg_Kucharik_r'] <- 'Yg'
dataKucharik2=dataKucharik2[c("CROP","STATIONNAME","LONGITUDE","method","Yw_GYGA","Yg_GYGA","Yw","Yg")]

dataMueller2=subset(dataMueller,CROP!="Irrigated maize")
dataMueller2=subset(dataMueller2,CROP!="Irrigated soybean")
dataMueller2$method='Mueller'
dataMueller2=dataMueller2[!duplicated(dataMueller2), ]
dataMueller2=pivot_wider(dataMueller2,names_from = "variable", values_from = "value")
names(dataMueller2)[names(dataMueller2) == 'Yw_Mueller'] <- 'Yw'
names(dataMueller2)[names(dataMueller2) == 'Yg_Mueller'] <- 'Yg'
dataMueller2=dataMueller2[c("CROP","STATIONNAME","LONGITUDE","method","Yw_GYGA","Yg_GYGA","Yw","Yg")]

dataGerber2=subset(dataGerber,CROP!="Irrigated maize")
dataGerber2=subset(dataGerber2,CROP!="Irrigated soybean")
dataGerber2$method='Gerber'
dataGerber2=dataGerber2[!duplicated(dataGerber2), ]
dataGerber2=pivot_wider(dataGerber2,names_from = "variable", values_from = "value")
names(dataGerber2)[names(dataGerber2) == 'Yw_Gerber'] <- 'Yw'
names(dataGerber2)[names(dataGerber2) == 'Yg_Gerber'] <- 'Yg'
dataGerber2=dataGerber2[c("CROP","STATIONNAME","LONGITUDE","method","Yw_GYGA","Yg_GYGA","Yw","Yg")]

      #rbind all####
dataMethods=rbind(dataHat2,dataKucharik2,dataMueller2,dataGerber2)

names(dataMethods)[names(dataMethods) == 'CROP'] <- 'crop'

dataMethods$method.crop=paste0(dataMethods$method,".",dataMethods$crop)

dataMethods$method.crop=gsub("ed ","ed_",dataMethods$method.crop)

      #add US average####
CZ=unique(dataGYGA[c("CROP","STATIONNAME","CLIMATEZONE")])
names(CZ)[names(CZ) == 'CROP'] <- 'crop'

dataMethods=merge(dataMethods,CZ,by=c("crop","STATIONNAME"),all.x=T)

#bring all area crops for stations
area_buffer=read.csv(paste0(inputs.wd,"/area_buffer.csv"),sep=";")
names(area_buffer)[names(area_buffer) == 'CROP'] <- 'crop'

#weight per CZ
dataMethods=merge(dataMethods,area_buffer,by=c("STATIONNAME","crop"),all.x=T)

data_max=ddply(dataMethods, ~crop*CLIMATEZONE*method, summarise,
               sum_AREA_IN_CLIMATEZONE_HA = sum(AREA_IN_CLIMATEZONE_HA,na.rm=T))

dataMethods=merge(dataMethods,data_max,by=c("crop","CLIMATEZONE","method"),all.x=T)

dataMethods$percMax_AREA_IN_CZ=dataMethods$AREA_IN_CLIMATEZONE_HA/dataMethods$sum_AREA_IN_CLIMATEZONE_HA

dataMethods$Yw_perc=dataMethods$Yw*dataMethods$percMax_AREA_IN_CZ
dataMethods$Yg_perc=dataMethods$Yg*dataMethods$percMax_AREA_IN_CZ
dataMethods$Yw_GYGA_perc=dataMethods$Yw_GYGA*dataMethods$percMax_AREA_IN_CZ
dataMethods$Yg_GYGA_perc=dataMethods$Yg_GYGA*dataMethods$percMax_AREA_IN_CZ

dataMethods[dataMethods==0] <- NA

dataMethods_CZ=ddply(dataMethods, ~crop*CLIMATEZONE*method, summarise,
                       Yw = sum(Yw_perc,na.rm=T),
                       Yg = sum(Yg_perc,na.rm=T),
                       Yw_GYGA = sum(Yw_GYGA_perc,na.rm=T),
                       Yg_GYGA = sum(Yg_GYGA_perc,na.rm=T))

dataMethods_CZ[dataMethods_CZ==0] <- NA

#bring all CZ area
area_cz=read.csv(paste0(inputs.wd,"/area_cz.csv"),sep=";")
names(area_cz)[names(area_cz) == 'CROP'] <- 'crop'

#weight by US
dataMethods_CZ=merge(dataMethods_CZ,area_cz,by=c("CLIMATEZONE","crop"),all.x=T)

dataMethods_CZ$AREA_SELECTED_STATIONS_HA=NULL

data_max=ddply(dataMethods_CZ, ~crop*method, summarise,
               sum_AREA_IN_USA_HA = sum(TOTAL_AREA_HA,na.rm=T))

dataMethods_CZ=merge(dataMethods_CZ,data_max,by=c("crop","method"),all.x=T)

dataMethods_CZ$percMax_AREA_IN_USA=dataMethods_CZ$TOTAL_AREA_HA/dataMethods_CZ$sum_AREA_IN_USA_HA

dataMethods_CZ$Yw_perc=dataMethods_CZ$Yw*dataMethods_CZ$percMax_AREA_IN_USA
dataMethods_CZ$Yg_perc=dataMethods_CZ$Yg*dataMethods_CZ$percMax_AREA_IN_USA
dataMethods_CZ$Yw_GYGA_perc=dataMethods_CZ$Yw_GYGA*dataMethods_CZ$percMax_AREA_IN_USA
dataMethods_CZ$Yg_GYGA_perc=dataMethods_CZ$Yg_GYGA*dataMethods_CZ$percMax_AREA_IN_USA

dataMethods_CZ[dataMethods_CZ==0] <- NA

dataMethods_USA=ddply(dataMethods_CZ, ~crop*method, summarise,
                        Yw = sum(Yw_perc,na.rm=T),
                        Yg = sum(Yg_perc,na.rm=T),
                        Yw_GYGA = sum(Yw_GYGA_perc,na.rm=T),
                        Yg_GYGA = sum(Yg_GYGA_perc,na.rm=T),)

dataMethods_CZ[dataMethods_CZ==0] <- NA

      #Extended data Figure 3####
{
  dataMethods$crop <- factor(dataMethods$crop, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))
  dataMethods_USA$crop <- factor(dataMethods_USA$crop, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))
  
  #methods names
  dataMethods$method=str_replace_all(dataMethods$method,
                                 c(Hatfield="LQ",Kucharik="GR",
                                   Mueller="CB",Gerber="GQ"))
  dataMethods_USA$method=str_replace_all(dataMethods_USA$method,
                                         c(Hatfield="LQ",Kucharik="GR",
                                           Mueller="CB",Gerber="GQ"))
  
  dataMethods$method <- factor(dataMethods$method, levels = c("LQ", "GQ","GR","CB"))
  dataMethods_USA$method <- factor(dataMethods_USA$method, levels = c("LQ","GQ","GR","CB"))
}


dataMethods_labels <- mpg %>%
  mutate(year = mean(c(min(Yw_GYGA), max(Yw_GYGA))), Yw = min(cty)) %>%
  distinct(manufacturer, year, cty)

dataMethods_label=ddply(dataMethods, ~crop*method, summarise,
                        Yw = max(Yw))



dataMethods$crop=with(dataMethods,str_replace_all(dataMethods$crop,
                                                  c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                    `Rainfed wheat`="Wheat")))

dataMethods_USA$crop=with(dataMethods_USA,str_replace_all(dataMethods_USA$crop,
                                                          c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                            `Rainfed wheat`="Wheat")))

dataMethods_label$crop=with(dataMethods_label,str_replace_all(dataMethods_label$crop,
                                                              c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                                `Rainfed wheat`="Wheat")))

dataMethods$crop <- factor(dataMethods$crop, levels = c("Soybean", "Maize", "Wheat"))
dataMethods_USA$crop <- factor(dataMethods_USA$crop, levels = c("Soybean", "Maize", "Wheat"))
dataMethods_label$crop <- factor(dataMethods_label$crop, levels = c("Soybean", "Maize", "Wheat"))


#add missing points
dataMethods_new=read.csv(paste0(inputs.wd,"/missing_points_comparison.csv"),sep=";")
dataMethods=rbind(dataMethods,dataMethods_new)


shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Extended_Data_Figure_3.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 

ggplot(dataMethods, aes(x = Yw_GYGA, y = Yw)) +
  geom_point(stat = "identity") +
  geom_abline(slope=1,intercept=0)+
  geom_point(data=dataMethods_USA,size=6,fill="red",col="red",shape=3,stroke = 2)+
  #geom_smooth(method="lm")+
  theme(plot.title=element_text(size=15,face="bold.italic"),
        axis.title.y=element_text(size=20,face="bold"),axis.title.x=element_text(size=20,face="bold"),
        axis.text.x=element_text(angle = 0, vjust = 1, size=15,face="bold"),
        axis.text.y=element_text(size=15,face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),strip.text = element_text(size=15),legend.position="right",
        panel.border = element_rect(fill = NA,colour = "black"),strip.text.x=element_text(size=20),
        #strip.text.y=element_blank(),
        strip.text.y=element_text(size=20),
        strip.background = element_blank())+
  theme(aspect.ratio = 1)+
  scale_y_continuous(labels=abs)+
  scale_shape_manual(values=c(0,1,2,3,4,0,1,2,3,4,5,6,8))+
  labs(y = expression(paste("Statistical methods Yw (Mg ",ha^-1,")")),
       x = expression(paste("GYGA Yw (Mg ",ha^-1,")")))+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Maize",],aes(x=7.8,y=4.7,label = crop),size=5.5,hjust = 0)+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Soybean",],aes(x=2.5,y=2,label = crop),size=5.5,hjust = 0)+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Wheat",],aes(x=4,y=2,label = crop),size=5.5,hjust = 0)+
  # #facet_grid2(cols=vars(variable), scales = "free", independent = "all",drop=T)
  # +
  facet_grid2(vars(crop), vars(method), scales = "free",axes = "all",remove_labels="x",independent = "all")+
  facetted_pos_scales(
    y = list(
      crop == "Maize" ~ scale_y_continuous(limits = c(4, 16.5)),
      crop == "Soybean" ~ scale_y_continuous(limits = c(1.8, 6.1)),
      crop == "Wheat" ~ scale_y_continuous(limits = c(1.5, 10))),
    x = list(
      crop == "Maize" ~ scale_x_continuous(limits = c(4, 16.5)),
      crop == "Soybean" ~ scale_x_continuous(limits = c(1.8, 6.1)),
      crop == "Wheat" ~ scale_x_continuous(limits = c(1.5, 10))))
dev.off()
dev.off()
shell(file.path(outputs.wd, "Extended_Data_Figure_3.pdf"), wait = FALSE)


        #table for NFOOD####

dataMethods_toshare<- dataMethods %>% select(crop, method,STATIONNAME, Yw_GYGA, Yw)
write.csv(dataMethods_toshare,paste0(outputs.wd,"/Source_data_Extended_Data_Figure_3.csv"), row.names = FALSE)

      #Figure 4####
dataMethods=subset(dataMethods,Yg_GYGA>0)


shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Figure_4.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 

ggplot(dataMethods, aes(x = Yg_GYGA, y = Yg)) +
  geom_point(stat = "identity") +
  geom_abline(slope=1,intercept=0)+
  geom_point(data=dataMethods_USA,size=6,fill="red",col="red",shape=3,stroke = 2)+
  #geom_smooth(method="lm")+
  theme(plot.title=element_text(size=15,face="bold.italic"),
        axis.title.y=element_text(size=20,face="bold",vjust = +3),axis.title.x=element_text(size=20,face="bold"),
        axis.text.x=element_text(angle = 0, vjust = 1, size=15,face="bold"),
        axis.text.y=element_text(size=15,face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),strip.text = element_text(size=15),legend.position="right",
        panel.border = element_rect(fill = NA,colour = "black"),strip.text.x=element_text(size=20),
        #strip.text.y=element_blank(),
        strip.text.y=element_text(size=20),
        strip.background = element_blank())+
  theme(aspect.ratio = 1)+
  scale_y_continuous(labels=abs)+
  scale_shape_manual(values=c(0,1,2,3,4,0,1,2,3,4,5,6,8))+
  labs(y = expression(paste("Statistical methods yield gap (Mg ",ha^-1,")")),
       x = expression(paste("GYGA yield gap (Mg ",ha^-1,")")))+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Maize",],aes(x=0,y=8,label = crop),size=5.5,hjust = 0)+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Soybean",],aes(x=0,y=4,label = crop),size=5.5,hjust = 0)+
  # geom_text(data = dataMethods_label[dataMethods_label$crop=="Wheat",],aes(x=0,y=8,label = crop),size=5.5,hjust = 0)+
  # #facet_grid2(cols=vars(variable), scales = "free", independent = "all",drop=T)
  # +
  facet_grid2(vars(crop), vars(method),scales = "free",axes = "all",remove_labels="x",independent = "all")+
  facetted_pos_scales(
    y = list(
      crop == "Maize" ~ scale_y_continuous(limits = c(0, 8)),
      crop == "Soybean" ~ scale_y_continuous(limits = c(0, 4)),
      crop == "Wheat" ~ scale_y_continuous(limits = c(0, 8))),
    x = list(
      crop == "Maize" ~ scale_x_continuous(limits = c(0, 8)),
      crop == "Soybean" ~ scale_x_continuous(limits = c(0, 4)),
      crop == "Wheat" ~ scale_x_continuous(limits = c(0, 8))))
dev.off()
dev.off()
shell(file.path(outputs.wd, "Figure_4.pdf"), wait = FALSE)

        #table for NFOOD####

dataMethods_toshare<- dataMethods %>% select(crop, method,STATIONNAME, Yg_GYGA, Yg)
write.csv(dataMethods_toshare,paste0(outputs.wd,"/Source_Data_Figure_4.csv"), row.names = FALSE)

#Extended data Figure 2, Evaluation obs vs sim plot####
HMsim=read_delim(paste0(inputs.wd,"\\validation\\HM_sim.xlsx")
                 ,delim=";",skip =0,col_names = T,na = c("NDef","['NDef']","na" ,"n.a.", "n/a" , "NA" , "" , "-99" , "-999"  , "-9999" , "n/a", "nan" ))

HMsim=read_excel(paste0(inputs.wd,"\\validation\\HM_sim.xlsx")
                               ,skip =0,col_names = T,na = c("na" ,"n.a.", "n/a" , "NA" , "" , "-99" , "-999"  , "-9999" , "n/a", "nan" ))
APsim=read_excel(paste0(inputs.wd,"\\validation\\AP_sim.xlsx")
                 ,skip =0,col_names = T,na = c("na" ,"n.a.", "n/a" , "NA" , "" , "-99" , "-999"  , "-9999" , "n/a", "nan" ))
SMsim=read_excel(paste0(inputs.wd,"\\validation\\SM_sim.xlsx")
                 ,skip =0,col_names = T,na = c("na" ,"n.a.", "n/a" , "NA" , "" , "-99" , "-999"  , "-9999" , "n/a", "nan" ))

HMsim$crop="Rainfed maize"
APsim$crop="Rainfed soybean"
SMsim$crop="Rainfed wheat"

APsim$obs=APsim$obs/1000
APsim$sim=APsim$sim/1000

#from buschel to t
SMsim$obs=SMsim$obs/100
SMsim$sim=SMsim$sim/100

validation=rbind(HMsim,APsim,SMsim)
remove(HMsim,APsim,SMsim)

source(paste0(rScripts.wd,"/evaluation.R")) #en PJ, chemin est à adapter
#rRMSE
for (j in unique (validation$crop))
{   validation_j=subset(validation,crop==j)
    validation_j=subset(validation_j,!is.na(obs))
    t=with(validation_j,RRMSE(obs, sim))
    t=data.frame(crop=j,nRMSE=t)
    e=with(validation_j,RMSE(obs, sim))
    e=data.frame(crop=j,RMSE=e)
    m=with(validation_j,ME(obs, sim))
    m=data.frame(crop=j,ME=m)
    # n=with(validation_j,length(obs))
    # n=data.frame(crop=j,n=n)
    if(exists("statsRRMSE") == F) { statsRRMSE <- t } else { statsRRMSE <- rbind(statsRRMSE,t)}
    if(exists("statsRMSE") == F) { statsRMSE <- e } else { statsRMSE <- rbind(statsRMSE,e)}
    if(exists("statsME") == F) { statsME <- m } else { statsME <- rbind(statsME,m)}
    # if(exists("statsN") == F) { statsN <- n } else { statsN <- rbind(statsN,n)}
    rm(t,e,n,m)
  }
nRMSE=statsRRMSE
RMSE=statsRMSE
ME=statsME

# nRMSE$nRMSE=paste0('rRMSE: ',round(nRMSE$nRMSE,0),"%")
# 
# rmse=RMSE
# rmse$rmse=rmse$RMSE
# rmse$rmse=paste0('RMSE: ',round(rmse$rmse,2),expression(paste("Simulated yield (Mg ",ha^-1,")")))

remove(statsRRMSE,statsRMSE,statsME)

# validation$crop=with(validation,str_replace_all(validation$crop,
#                                                       c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
#                                                         `Rainfed wheat`="Wheat")))
# validation$crop <- factor(validation$crop, levels = c("Soybean", "Maize", "Wheat"))

shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Extended_Data_Figure_2.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 

ggplot(validation, aes(x=obs, y=sim))+
  geom_point(aes(),size=2)+
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size=0.5)+
  #scale_color_manual(values=c("red", "blue", "black", "green"))+
  geom_abline(slope=1, intercept=0, lty=2,size=1)+
  #xlim(0,10)+ylim(0,10)+
  theme_bw()+theme(legend.position="none")+theme(aspect.ratio = 1)+
  coord_cartesian(expand = F)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    strip.background = element_blank(),
                                    strip.text.x = element_text(size=20),
                                    panel.spacing = unit(2, "lines"))+
  facet_grid2(vars(crop), scales = "free",axes = "all",remove_labels="x",independent = "all")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y = expression(paste("Simulated yield (Mg ",ha^-1,")")),
       x = expression(paste("Observed experimental yield (Mg ",ha^-1,")")))+
  facet_grid2(cols=vars(crop), scales = "free",axes = "all",remove_labels="x",independent = "all")+
  facetted_pos_scales(
    y = list(
      crop == "Rainfed maize" ~ scale_y_continuous(limits = c(0, 20)),
      crop == "Rainfed soybean" ~ scale_y_continuous(limits = c(0, 8)),
      crop == "Rainfed wheat" ~ scale_y_continuous(limits = c(0, 8))),
    x = list(
      crop == "Rainfed maize" ~ scale_x_continuous(limits = c(0, 20)),
      crop == "Rainfed soybean" ~ scale_x_continuous(limits = c(0, 8)),
      crop == "Rainfed wheat" ~ scale_x_continuous(limits = c(0, 8))))+
  geom_text(data = RMSE[RMSE$crop=="Rainfed maize",], aes(x=1, y=18, label = paste0('RMSE : ',round(RMSE,2))
                                                            ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = nRMSE[nRMSE$crop=="Rainfed maize",], aes(x=1, y=16.5, label = paste0('rRMSE : ',round(nRMSE,0))
                              ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = ME[ME$crop=="Rainfed maize",], aes(x=1, y=15, label = paste0('ME : ',round(ME,2))
                                                         ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = RMSE[RMSE$crop=="Rainfed soybean",], aes(x=0.5, y=7.2, label = paste0('RMSE : ',round(RMSE,2))
                                                          ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = nRMSE[nRMSE$crop=="Rainfed soybean",], aes(x=0.5, y=6.6, label = paste0('rRMSE : ',round(nRMSE,0))
                                                            ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = ME[ME$crop=="Rainfed soybean",], aes(x=0.5, y=6, label = paste0('ME : ',round(ME,2))
                                                      ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = RMSE[RMSE$crop=="Rainfed wheat",], aes(x=0.5, y=7.2, label = paste0('RMSE : ',round(RMSE,2))
                                                            ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = nRMSE[nRMSE$crop=="Rainfed wheat",], aes(x=0.5, y=6.6, label = paste0('rRMSE : ',round(nRMSE,0))
                                                              ,shape = NULL), hjust = 0,parse=T, size=4)+
  geom_text(data = ME[ME$crop=="Rainfed wheat",], aes(x=0.5, y=6, label = paste0('ME : ',round(ME,2))
                                                        ,shape = NULL), hjust = 0,parse=T, size=4)+
  theme(text = element_text(size = 20))
  

  

# geom_text(data = RMSE[RMSE$variable=="masec"&nRMSE$phase=="calibration",], aes(x=0.2, y=8.2, label = paste0('RMSE : ',RMSE)
#                                                                                ,shape = NULL), hjust = 0,parse=T, size=3.5,color='red')+
#   geom_text(data = nRMSE[nRMSE$variable=="masec"&nRMSE$phase=="evaluation",], aes(x=0.2, y=7, label = paste0('rRMSE : ',nRMSE)
#                                                                                   ,shape = NULL, col=NULL), hjust = 0,parse=T, size=3.5,color='blue')+
#   geom_text(data = RMSE[RMSE$variable=="masec"&nRMSE$phase=="evaluation",], aes(x=0.2, y=6.2, label = paste0('RMSE : ',RMSE)
#                                                                               ,shape = NULL, col=NULL), hjust = 0,parse=T, size=3.5,color='blue')

dev.off()
dev.off()
shell(file.path(outputs.wd, "Extended_Data_Figure_2.pdf"), wait = FALSE)


        #table for NFOOD####

data_toshare<- validation %>% select(crop, sim,obs)
write.csv(data_toshare,paste0(outputs.wd,"/Source_Data_Extended_Data_Figure_2.csv"), row.names = FALSE)

