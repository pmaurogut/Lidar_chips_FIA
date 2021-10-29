# TODO: Add comment
# 
# Author: gutierrf
###############################################################################
library("plyr")
library("purrr")
library("sp")
library("rgdal")
library("sf")
library("doParallel")


#Destination folder
DESTCRS<-"epsg:3857"
INDEXFOLDER<-"F:/Paco/CMSWS/FIA_lidar_chips/Data/Index_USGS"
FIADBFOLDER<-"Data/FIADB"
DESTFOLDER<-"F:/LIDAR_FIA"
FOLDER<-paste(DESTFOLDER,"PLOTSFOLDER",sep="/")
TYPES<-c("ENTWINEPLUS")

add_intersects_fields<-function(x){
#	add plot fields and sets default values
#	FIELDS TO ADD
	x
	
}

make_folders_project_plot<-function(x){
	try({
				dir.create(x)
				dir.create(paste(x,"PC",sep="/"))
				dir.create(paste(x,"DTM",sep="/"))
				dir.create(paste(x,"METRICS",sep="/"))
			})
}



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
		combinations$state,"_",
		basename(combinations$index),sep="")

#Sets wd in DESTFOLDER
#Save only intersects for states
setwd(FOLDER)
cores<-max(1,detectCores())
cl <- makePSOCKcluster(cores,outfile="../../debug.txt")
loaded<-.packages()
doParallel::registerDoParallel(cl)
Sys.time()
combinations<-ddply(combinations[!combinations$state=="ALL",],c("index","state"),function(x){
			
		plots<-st_read(x$plots[1])
		projects<-st_read(x$index[1])
		projects<-st_transform(projects,st_crs(plots))
		success<-try({
			intersects_projects<-st_intersection(plots,projects)
			intersects_projects$PC_area_proj<-st_area(intersects_projects)
			ref_area<-2000^2
			intersects_projects$PC_prop_proj<-as.numeric(intersects_projects$PC_area_proj)/ref_area
			st_write(intersects_projects,x$dest_file[1], delete_layer = TRUE)		
				})
		x$success<-!inherits(success,"try-error")
		x	
		},.parallel=TRUE,.paropts=list(.packages=loaded))
stopCluster(cl)
Sys.time()

#Sets wd in DESTFOLDER
#Create plot lidar directories and add field for 
cores<-max(1,detectCores())
cl <- makePSOCKcluster(cores,outfile="../../debug.txt")
loaded<-.packages()
doParallel::registerDoParallel(cl)
Sys.time()
combinations_directories<-ddply(combinations[!combinations$state=="ALL",],c("index","state"),function(x){
			
			intersects<-st_read(x$dest_file[1])
			intersects$lidar_plot_folder<-paste(intersects$plot_path,
					basename(dirname(intersects$url)),sep="/")
			
			res<-ddply(intersects,c("lidar_plot_folder"),function(y){
				a<-try(make_folders_project_plot(y$lidar_plot_folder))
				y$lidar_plot_folder<-ifelse(inherits(a,"try-error"),NA,
						y$lidar_plot_folder)
				y		
					},.progress="tk")
			res<-st_sf(res)
			
			success<-try({
						st_write(intersects_projects,x$dest_file[1], delete_layer = TRUE)		
					})
			x$lidar_plots<-dim(res)[1]
			x$lidar_plots_folder_failed<-sum(is.na(res$lidar_plot_folder))
			x$index_updated<-!inherits(success,"try-error")
			x	
		},.parallel=TRUE,.paropts=list(.packages=loaded))
stopCluster(cl)
Sys.time()

save(combinations,combinations_directories,file="STATES/Intersects_states.Rdata")

