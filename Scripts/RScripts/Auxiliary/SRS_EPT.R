# TODO: Add comment
# 
# Author: pmaurogut
###############################################################################

library(rvest)
library(plyr)
library(sf)
library(processx)
library(jsonlite)
INDEXFOLDER<-"F:/Paco/CMSWS/FIA_lidar_chips/Data/Index_USGS"
FIADBFOLDER<-"Data/FIADB"
DESTFOLDER<-"F:/LIDAR_FIA"
FOLDER<-paste(DESTFOLDER,"PLOTSFOLDER",sep="/")
TYPES<-c("ENTWINEPLUS")

#make path to plots indexes
index_USGS<-st_read(paste(INDEXFOLDER,"/USGS_project_index_",TYPES,".gpkg",sep=""))
index_NOAA<-st_read("F:/Paco/CMSWS/FIA_lidar_chips/Data/Index_NOAA/NOAA_project_index.gpkg")

srs_USGS<-ldply(unique(index_USGS$url),function(x){
			
			a<-try({
						srs<-fromJSON(x)$srs
						horizontal<-srs$horizontal
						if(is.null(horizontal)){
							horizontal<-NA
						}
						vertical<-srs$vertical
						if(is.null(vertical)){
							vertical<-NA
						}
						EPSG<-paste0(srs$authority,":",horizontal,"+",vertical)
					})
			if(inherits(a,"try-error")){
				data.frame(url=x,EPSG=NA,horizontal=NA,vertical=NA)
			}else{
				data.frame(url=x,EPSG=EPSG,horizontal=horizontal,vertical=vertical)
			}
			
		},.progress="tk")
write.csv(srs_USGS,"/Lidar_chips_FIA/Data/SRS_EPT_USGS.csv")
srs_NOAA<-ldply(unique(index_NOAA$url),function(x){
			
			a<-try({
						srs<-fromJSON(x)$srs
						horizontal<-srs$horizontal
						vertical<-srs$vertical
						EPSG<-paste0(srs$authority,":",horizontal,"+",vertical)
					})
			if(inherits(a,"try-error")){
				data.frame(url=x,EPSG=NA,horizontal=NA,vertical=NA)
			}else{
				data.frame(url=x,EPSG=EPSG,horizontal=horizontal,vertical=vertical)
			}
			
		},.progress="tk")
write.csv(srs_NOAA,"/Lidar_chips_FIA/Data/SRS_EPT_NOAA.csv")