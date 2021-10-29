# TODO: Add comment
# 
# Author: gutierrf
###############################################################################
library("USGSlidar")
library("rFIA")
library("plyr")
library("purrr")
library("sp")
library("rgdal")
library("sf")
library("rgeos")
library("jsonlite")
library("doParallel")
source("Scripts/RScripts/Auxiliary_functions/Downloaders.R")

#Destination folder
DESTCRS<-"epsg:3857"
INDEXFOLDER<-"FIA_lidar_chips/Data/Index_USGS"
FIADBFOLDER<-"Data/FIADB"
DESTFOLDER<-"F:/LIDAR_FIA"
FOLDER<-paste(DESTFOLDER,"PLOTSFOLDER",sep="/")
TYPES<-c("ENTWINEPLUS")
#make path to plots shapefiles
plots_bbox<-paste(FOLDER,"BBOX","BBOX_ALL.gpkg",sep="/")
states<-unique(st_read(plots_bbox)$STATECD)
plots_bbox<-c(plots_bbox,paste(FOLDER,"STATES",states,"BBOX",paste("BBOX_",states,".gpkg",sep=""),sep="/"))

#make path to plots indexes
index_projects<-paste(INDEXFOLDER,"/USGS_project_index_",TYPES,".gpkg",sep="")

#combinations paths plots indexes
combinations<-expand.grid(plots=c(plots_bbox),
		index=index_projects,stringsAsFactors=FALSE)
#CHANGE HERE (works but not the best)
combinations$state<-c("ALL",states)
#Make destination folder name
combinations$dest_folder<-dirname(gsub("BBOX/","INTERSECTED_INDEXES/",combinations$plots))

combinations$dest_file<-paste(combinations$dest_folder,"/",
		basename(combinations$index),sep="")
combinations$dest_file<-gsub(".gpkg$","",combinations$dest_file)
combinations$dest_file<-paste(combinations$dest_file,"_",combinations$state,".gpkg",sep="")

combinations<-ddply(combinations,c("index"),function(x){
			
		plots<-st_read(x$plots[1])
		projects<-st_read(x$index[1])
		projects<-st_transform(projects,st_crs(plots))
		ddply(x,"plots",function(y){
			dir.create(y$dest_folder)
			success<-try({
						plots<-st_read(y$plots[1])
						intersects_projects<-st_intersection(plots,projects)
						intersects_projects$PC_area_proj<-st_area(intersects_projects)
						ref_area<-2000^2
						intersects_projects$PC_prop_proj<-as.numeric(intersects_projects$PC_area_proj)/ref_area
						st_write(intersects_projects,y$dest_file[1], delete_layer = TRUE)		
					})
			y$index_created<-!inherits(success,"try-error")
			y
					
		},.progress="tk")
			
		},.progress="tk")

save(combinations,file="ALL_Intersects_plots.Rdata")

