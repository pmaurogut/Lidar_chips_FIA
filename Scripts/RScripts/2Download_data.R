# TODO: Add comment
# 
# Author: pmaurogut
###############################################################################
library("USGSlidar")
library("plyr")
library("purrr")
library("sp")
library("rgdal")
library("sf")
library("jsonlite")
library("doParallel")


#Destination folder
DESTCRS<-"epsg:3857"
INDEXFOLDER<-"F:/Paco/CMSWS/FIA_lidar_chips/Data/Index_USGS"
FIADBFOLDER<-"Data/FIADB"
DESTFOLDER<-"F:/LIDAR_FIA"
FOLDER<-paste(DESTFOLDER,"PLOTSFOLDER",sep="/")
TYPES<-c("ENTWINEPLUS")


to_get<-st_read(to_get)


to_get<-to_get[!is.na(to_get$plot_lidar_folder),]

get_bounds<-function(x){
	
	a<-st_bbox(x$geometry[1])
	paste0("[",a[1],",",a[3],"],[",a[2],",",a[4],"]")
	
}

to_get<-to_get[!is.na(to_get$url),]
srs_NOAA<-ldply(unique(to_get$url),function(x){
			
		a<-fromJSON(x)$srs
		EPSG<-paste0(a$authority,":",a$horizontal,"+",a$vertical)
		data.frame(url=x,EPSG=EPSG)
		})

to_get<-merge(to_get,srs_NOAA,by="url")


to_get2<-adply(to_get,1,function(x){
		x<-st_sf(x)
		x<-st_transform(x,st_crs(x$EPSG))
		bounds<-st_bbox(x$geometry)
		x$bounds<-paste0("([",bounds[1],",",bounds[3],"],[",
				bounds[2],",",bounds[4],"])")	
		x	
		},.progress="tk")
to_get2<-st_sf(to_get2)

reader<-toJSON(list(type="readers.ept",filename=NA))
writer<-list(type="writers.laz",compression="lazperf")
pipeline<-list(reader,writer)
cat(toJSON(pipeline))
#{type
#	"type": "readers.ept",
#	"filename": "http://na.entwine.io/autzen/ept.json",
#	"addons": { "Classification": "~/entwine/addons/autzen/smrf" }
#},
#{
#	"type": "writers.las",
#	"filename": "autzen-ept-smrf.las"
#}

# to run the pipelines, open a miniconda prompt and activate pdalworking environment,
# then run the RunME.bat file in the pipelines folder
#
# examples:
# pdal pipeline G:\R_Stuff\FIAExample\pipelines\Plot_473338115489998.json
# pdal pipeline G:\R_Stuff\FIAExample\pipelines\Plot_196883321010854.json
