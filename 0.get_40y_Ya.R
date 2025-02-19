#prep Ya per buffer for 40y####

#import Ya irrigated and rainfed
#import data from paper (get unc and non irrigated and use unc when non-irrigated are absent ?)
setwd(inputs.wd)

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

