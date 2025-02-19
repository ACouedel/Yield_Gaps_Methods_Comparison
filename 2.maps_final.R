#Maps
dataAll2=dataAll
#Figure  3 - MAP YW all methods  CROP ~ method ####
    #soybean####
{
  dataAll2$CROP <- factor(dataAll2$CROP, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))
  
  dataAll2$CROP=with(dataAll2,str_replace_all(dataAll2$CROP,
                                  c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                    `Rainfed wheat`="Wheat")))
  
  #methods names
  dataAll2$method=str_replace_all(dataAll2$method,
                                       c(GYGA="GYGA",Hatfield="LQ",Kucharik="GR",
                                         Mueller="CB",Gerber="GQ"))
  
  dataAll2$method <- factor(dataAll2$method, levels = c("GYGA", "LQ", "GQ","GR","CB"))
 
  #add missing points maize and wheat
  dataAll2_new=read.csv(paste0(inputs.wd,"/missing_data_map.csv"),sep=";")
  dataAll2=rbind(dataAll2,dataAll2_new)
  
  
}

dataAll2soybeanYw=subset(dataAll2,str_sub(variable,1,2)=="Yw")
dataAll2soybeanYw=subset(dataAll2soybeanYw,CROP=="Soybean")
dataAll2soybeanYw=subset(dataAll2soybeanYw,STATIONNAME!="Draughon Mille TX")
#remove dazey as Gerber do not provide enough data on that station
dataAll2soybeanYw=dataAll2soybeanYw %>% filter(!(STATIONNAME == "Dazey ND" & method == "GQ"))
dataAll2soybeanYw=dataAll2soybeanYw %>% filter(!(STATIONNAME == "Lisbon ND" & method == "GQ"))


#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("illinois", "indiana", "iowa", "kentucky", "michigan",
                                           "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", "nebraska", "kansas"))

{   
  dataAll2soybeanYw_label=ddply(dataAll2soybeanYw, ~method*CROP, summarise,
                                LONGITUDE = NA)
  
  
  Yw_soybean=ggplot(data=dataAll2soybeanYw,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yw")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          strip.text.x = element_text(size = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.y=element_text(size = 20),
          strip.background = element_blank(),
          )+
      #plot.margin=unit(c(-0.30,0,0,0), "null"))+
    coord_fixed(ratio = 1,expand=F)+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+ #the order of the margins here is top, right, bottom, left.
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_text(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("Maize")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-105,-80), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(35, 50), expand=c(0,0), name = 'Latitude')+
    #geom_text(data = dataAll2soybeanYw_label[dataAll2soybeanYw_label$CROP=="Rainfed soybean",],aes(x=-105.5,y=35.5,label = CROP),size=6,hjust = 0)+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2soybeanYw$value,na.rm=T)
                         ,limits = c(min(dataAll2soybeanYw$value,na.rm=T), max(dataAll2soybeanYw$value,na.rm=T)))+
    #guides(colour = guide_legend(override.aes = list(size=20)))+
    facet_grid(CROP ~ method, switch = "y")
    #facet_grid(method ~ CROP, switch = "y")
}


    #maize####
dataAll2maizeYw=subset(dataAll2,str_sub(variable,1,2)=="Yw")
dataAll2maizeYw=subset(dataAll2maizeYw,CROP=="Maize")
dataAll2maizeYw=subset(dataAll2maizeYw,STATIONNAME!="Draughon Mille TX")

#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("illinois", "indiana", "iowa", "kentucky", "michigan",
                                           "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", "nebraska", "kansas"))

{   
  dataAll2maizeYw_label=ddply(dataAll2maizeYw, ~method*CROP, summarise,
                                LONGITUDE = NA)
  
  Yw_maize=ggplot(data=dataAll2maizeYw,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yw")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          #strip.text.x = element_text(size = 15),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.y=element_text(size = 20),
          strip.text.x=element_blank(),
          strip.background = element_blank())+
    #margin(t = 0, r = 0, b = 0, l = 0, "pt"))+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+
    coord_fixed(ratio = 1)+
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_text(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("Maize")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-105,-80), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(35, 50), expand=c(0,0), name = 'Latitude')+
    #geom_text(data = dataAll2maizeYw_label[dataAll2maizeYw_label$CROP=="Rainfed maize",],aes(x=-105.5,y=35.5,label = CROP),size=6,hjust = 0)+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2maizeYw$value,na.rm=T)
                         ,limits = c(min(dataAll2maizeYw$value,na.rm=T), max(dataAll2maizeYw$value,na.rm=T)))+
    facet_grid(CROP ~ method, switch = "y")
}



    #wheat####
dataAll2wheatYw=subset(dataAll2,str_sub(variable,1,2)=="Yw")
dataAll2wheatYw=subset(dataAll2wheatYw,CROP=="Wheat")

#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("new mexico", "oklahoma", "texas", "colorado", "wyoming",
                                           "montana", "north dakota", "south dakota", 
                                           "nebraska", "kansas"))#states <- subset(all_states, long > -104.2)

{   
  dataAll2wheatYw_label=ddply(dataAll2wheatYw, ~method*CROP, summarise,
                              LONGITUDE = NA)
  
  Yw_wheat=ggplot(data=dataAll2wheatYw,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yw")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          #strip.text.x = element_text(size = 15),
          strip.text.y=element_text(size = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.x=element_blank(),
          strip.background = element_blank())+
    #margin(t = 0, r = 0, b = 0, l = 0, "pt"))+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+
    coord_fixed(ratio = 1)+
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_textt(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("wheat")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-117,-93), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(25, 50), expand=c(0,0), name = 'Latitude')+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    coord_fixed(ratio = 1)+
    #geom_text(data = dataAll2wheatYw_label[dataAll2wheatYw_label$CROP=="Rainfed wheat",],aes(x=-116,y=27,label = CROP),size=6,hjust = 0)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2wheatYw$value,na.rm=T)
                         ,limits = c(min(dataAll2wheatYw$value,na.rm=T), max(dataAll2wheatYw$value,na.rm=T)))+
    facet_grid(CROP ~ method, switch = "y")
}

    #arrange all####
#grid.arrange(Yw_soybean,Yw_maize,Yw_wheat,ncol=1)

library("cowplot")

shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Figure_3.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 


ggdraw() +
  draw_plot(Yw_soybean, x = 0, y = 0.66, width = 1, height = 0.33) +
  draw_plot(Yw_maize, x = 0, y = 0.33, width = 1, height = 0.33) +
  draw_plot(Yw_wheat, x = 0, y = 0, width = 1, height = 0.33)+
  theme(plot.margin = margin(0, 0, 0, 0))

dev.off()
dev.off()
shell(file.path(outputs.wd, "Figure_3.pdf"), wait = FALSE)


      #table for NFOOD####

data_toshare=rbind(dataAll2soybeanYw,dataAll2maizeYw,dataAll2wheatYw)

data_toshare<- data_toshare%>% select(-variable)
colnames(data_toshare)[colnames(data_toshare) == "value"] <- "Yw"
write.csv(data_toshare,paste0(outputs.wd,"/Source_Data_Figure_3.csv"), row.names = FALSE)



#Extenced data Figure 4- MAP Yg all methods  CROP ~ method ####
    #soybean####
dataAll2=dataAll
{
  dataAll2$CROP <- factor(dataAll2$CROP, levels = c("Rainfed soybean", "Rainfed maize", "Rainfed wheat"))
  
  dataAll2$CROP=with(dataAll2,str_replace_all(dataAll2$CROP,
                                              c(`Rainfed soybean`="Soybean",`Rainfed maize`="Maize",
                                                `Rainfed wheat`="Wheat")))
  
  #methods names
  dataAll2$method=str_replace_all(dataAll2$method,
                                  c(GYGA="GYGA",Hatfield="LQ",Kucharik="GR",
                                    Mueller="CB",Gerber="GQ"))
  
  dataAll2$method <- factor(dataAll2$method, levels = c("GYGA", "LQ", "GQ","GR","CB"))
  
  #add missing points maize and wheat
  dataAll2_new=read.csv(paste0(inputs.wd,"/missing_data_map.csv"),sep=";")
  dataAll2=rbind(dataAll2,dataAll2_new)
}

dataAll2soybeanYg=subset(dataAll2,str_sub(variable,1,2)=="Yg")
dataAll2soybeanYg=subset(dataAll2soybeanYg,CROP=="Soybean")
dataAll2soybeanYg=subset(dataAll2soybeanYg,STATIONNAME!="Draughon Mille TX")
#remove dazey as Gerber do not provide enough data on that station
dataAll2soybeanYg=dataAll2soybeanYg %>% filter(!(STATIONNAME == "Dazey ND" & method == "GQ"))
dataAll2soybeanYg=dataAll2soybeanYg %>% filter(!(STATIONNAME == "Lisbon ND" & method == "GQ"))




#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("illinois", "indiana", "iowa", "kentucky", "michigan",
                                           "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", "nebraska", "kansas"))

#Yg
{   
  dataAll2soybeanYg_label=ddply(dataAll2soybeanYg, ~method*CROP, summarise,
                                LONGITUDE = NA)
  
  
  Yg_soybean=ggplot(data=dataAll2soybeanYg,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yg")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          strip.text.x = element_text(size = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.y=element_text(size = 20),
          strip.background = element_blank(),
    )+
    #plot.margin=unit(c(-0.30,0,0,0), "null"))+
    coord_fixed(ratio = 1,expand=F)+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+ #the order of the margins here is top, right, bottom, left.
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_text(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("Maize")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-105,-80), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(35, 50), expand=c(0,0), name = 'Latitude')+
    #geom_text(data = dataAll2soybeanYg_label[dataAll2soybeanYg_label$CROP=="Rainfed soybean",],aes(x=-105.5,y=35.5,label = CROP),size=6,hjust = 0)+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2soybeanYg$value,na.rm=T)
                         ,limits = c(min(dataAll2soybeanYg$value,na.rm=T), max(dataAll2soybeanYg$value,na.rm=T)))+
    #guides(colour = guide_legend(override.aes = list(size=20)))+
    facet_grid(CROP ~ method, switch = "y")
}


    #maize####
dataAll2maizeYg=subset(dataAll2,str_sub(variable,1,2)=="Yg")
dataAll2maizeYg=subset(dataAll2maizeYg,CROP=="Maize")
dataAll2maizeYg=subset(dataAll2maizeYg,STATIONNAME!="Draughon Mille TX")

#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("illinois", "indiana", "iowa", "kentucky", "michigan",
                                           "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", "nebraska", "kansas"))

{   
  dataAll2maizeYg_label=ddply(dataAll2maizeYg, ~method*CROP, summarise,
                              LONGITUDE = NA)
  
  Yg_maize=ggplot(data=dataAll2maizeYg,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yg")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          #strip.text.x = element_text(size = 15),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.y=element_text(size = 20),
          strip.text.x=element_blank(),
          strip.background = element_blank())+
    #margin(t = 0, r = 0, b = 0, l = 0, "pt"))+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+
    coord_fixed(ratio = 1)+
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_text(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("Maize")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-105,-80), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(35, 50), expand=c(0,0), name = 'Latitude')+
    #geom_text(data = dataAll2maizeYg_label[dataAll2maizeYg_label$CROP=="Rainfed maize",],aes(x=-105.5,y=35.5,label = CROP),size=6,hjust = 0)+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2maizeYg$value,na.rm=T)
                         ,limits = c(min(dataAll2maizeYg$value,na.rm=T), max(dataAll2maizeYg$value,na.rm=T)))+
    facet_grid(CROP ~ method, switch = "y")
}



    #wheat####
dataAll2wheatYg=subset(dataAll2,str_sub(variable,1,2)=="Yg")
dataAll2wheatYg=subset(dataAll2wheatYg,CROP=="Wheat")

#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c("new mexico", "oklahoma", "texas", "colorado", "wyoming",
                                           "montana", "north dakota", "south dakota", 
                                           "nebraska", "kansas"))#states <- subset(all_states, long > -104.2)

{   
  dataAll2wheatYg_label=ddply(dataAll2wheatYg, ~method*CROP, summarise,
                              LONGITUDE = NA)
  
  Yg_wheat=ggplot(data=dataAll2wheatYg,aes(LONGITUDE,LATITUDE)) +
    theme_bw(base_size = 14)+
    labs(fill = "Yg")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="right",legend.title=element_blank(), 
          legend.text = element_text(size=15),
          #strip.text.x = element_text(size = 15),
          strip.text.y=element_text(size = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.x=element_blank(),
          strip.background = element_blank())+
    #margin(t = 0, r = 0, b = 0, l = 0, "pt"))+
    theme(plot.margin = margin(-5,0,-5,0, "cm"))+
    coord_fixed(ratio = 1)+
    geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )+
    #geom_textt(data=sumWSC_GDD_EnvClass_Corn,aes(x=-Inf, y=Inf,label=paste0("wheat")),hjust = -0.02, size=5,vjust = 1.4,inherit.aes = FALSE)+
    scale_x_continuous(limits=c(-117,-93), expand=c(0,0), name = 'Longitude') +
    scale_y_continuous(limits=c(25, 50), expand=c(0,0), name = 'Latitude')+
    geom_point(aes(fill=value,color=value),color="black",size=5,pch=21)+
    coord_fixed(ratio = 1)+
    #geom_text(data = dataAll2wheatYg_label[dataAll2wheatYg_label$CROP=="Rainfed wheat",],aes(x=-116,y=27,label = CROP),size=6,hjust = 0)+
    scale_fill_gradient2(low="red", high="green", midpoint=mean(dataAll2wheatYg$value,na.rm=T)
                         ,limits = c(min(dataAll2wheatYg$value,na.rm=T), max(dataAll2wheatYg$value,na.rm=T)))+
    facet_grid(CROP ~ method, switch = "y")
}


    #arrange all####
#grid.arrange(Yg_soybean,Yg_maize,Yg_wheat,ncol=1)

library("cowplot")

shell("taskkill /IM AcroRd32.exe /F", intern = FALSE) 
#pdf("figure.pdf", width = fig_size[1], height = fig_size[2])
pdf(file.path(outputs.wd, "Extended_Data_Figure_4.pdf"), width = 13, height = 8.8)
#par(cex = 6/12) 

ggdraw() +
  draw_plot(Yg_soybean, x = 0, y = 0.66, width = 1, height = 0.33) +
  draw_plot(Yg_maize, x = 0, y = 0.33, width = 1, height = 0.33) +
  draw_plot(Yg_wheat, x = 0, y = 0, width = 1, height = 0.33)

dev.off()
dev.off()
shell(file.path(outputs.wd, "Extended_Data_Figure_4.pdf"), wait = FALSE)

      #table for NFOOD####

data_toshare=rbind(dataAll2soybeanYg,dataAll2maizeYg,dataAll2wheatYg)

data_toshare<- data_toshare%>% select(-variable)
colnames(data_toshare)[colnames(data_toshare) == "value"] <- "Yg"
write.csv(data_toshare,paste0(outputs.wd,"/Source_Data_Extended_Data_Figure_4.csv"), row.names = FALSE)

 