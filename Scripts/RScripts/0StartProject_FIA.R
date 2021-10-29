# TODO: Add comment
# 
# Author: gutierrf
###############################################################################
library("plyr")
library("purrr")
library("sp")
library("rgdal")
library("sf")
library("rgeos")
library("RSQLite")
library("doParallel")


#DESTCRS everything in webmercator for now
DESTCRS<-"epsg:3857"
BUFFERDIST<-1000
FIADB<-"F:/Paco/CMSWS/FIA_lidar_chips/Data/FIADB/FIADB_USA.db"
FIADB_AK<-"F:/Paco/CMSWS/FIA_lidar_chips/Data/FIADB/SQLite_FIADB_INTAK_TANANA.db"
#Destination folder
DESTFOLDER<-"F:/LIDAR_FIA/PLOTSFOLDER"
TESTFOLDER<-"F:/LIDAR_FIA/TESTFOLDER"
TESTSTATECODES<-c(41,32,10,27,8,53)
SUBFOLDERS<-c("BUFFER","BBOX","INTERSECTED_INDEXES")

#AUX FUNCTIONS TO CREATE FOLDERS AND FIELDS
add_plot_fields<-function(x){
#	add plot fields and sets default values
	x$index_used<-NA
	x$downloaded_raw_tiles<-FALSE
	x$downloaded_fom<-NA
	x$downloaded_las_version<-NA
	x$merged_lidar<-FALSE
	x$projected_lidar<-FALSE
	x$has_time_stamp<-FALSE
	x$min_time<-NA
	x$max_time<-NA
	x$has_classes<-FALSE
	x$new_classification<-FALSE
	x$DTM_created<-FALSE
	x$metrics_created
	x$plot_path<-paste("STATES",x$STATECD,"PLOTS_LIDAR",x$LOCATION_ID,sep="/")
	x
	
}

#function to make FIRST CN CROSSWALK TABLE
add_intersects_fields<-function(x){
#	add plot fields and sets default values
	x$index_used<-NA
	x$downloaded_raw_tiles<-FALSE
	x$downloaded_fom<-NA
	x$downloaded_las_version<-NA
	x$merged_lidar<-FALSE
	x$projected_lidar<-FALSE
	x$has_time_stamp<-FALSE
	x$min_time<-NA
	x$max_time<-NA
	x$has_classes<-FALSE
	x$new_classification<-FALSE
	x$DTM_created<-FALSE
	x$metrics_created
	x$plot_path<-paste("STATES",x$STATECD,x$LOCATION_ID,sep="/")
	x
	
}

#Aux functions to prepare the folders 
make_folders_region<-function(x,y="PLOTS_LIDAR"){
	try({
				dir.create(x)
				dir.create(paste(x,"BBOX",sep="/"))
				dir.create(paste(x,"INTERSECTED_INDEXES",sep="/"))
				dir.create(paste(x,y,sep="/"))
			})
}

make_folders_project_plot<-function(x){
	try({
				dir.create(x)
				dir.create(paste(x,"PC",sep="/"))
				dir.create(paste(x,"DTM",sep="/"))
				dir.create(paste(x,"METRICS",sep="/"))
			})
}

#function to make FIRST CN CROSSWALK TABLE


#GET PLOT DATA FROM FIADB
con <- dbConnect(RSQLite::SQLite(), FIADB)
plots<-dbGetQuery(con, "SELECT * FROM PLOT")[,c("STATECD","CN","UNITCD","COUNTYCD","PLOT",
				"PREV_PLT_CN","LAT","LON","MEASYEAR")]
dbDisconnect(con)
#GET PLOT DATA FROM FIADB FOR AK
conAK <- dbConnect(RSQLite::SQLite(), FIADB_AK)
plotsAK<-dbGetQuery(conAK, "SELECT * FROM PLOT")[,c("STATECD","CN","UNITCD","COUNTYCD","PLOT",
				"PREV_PLT_CN","LAT","LON","MEASYEAR")]
dbDisconnect(conAK)

plots<-rbind(plots,plotsAK)
rm(plotsAK)

#CREATES UNIQUE LOCATION ID WITH STATECD, UNITCD, COUNTYCD and PLOT
#statecd+unitcd+countycd+plot 
plots$LOCATION_ID<-paste(plots$STATECD,plots$UNITCD,plots$COUNTYCD,plots$PLOT,sep="_")

make_folders_region(DESTFOLDER,"STATES")
#Creates state folders
states<-ldply(unique(plots$STATECD),function(x){
			where<-paste("STATES",x,sep="/")
			a<-make_folders_region(where)
			data.frame(STATECD=x,FAILED=inherits(a,"try-error"))
		
		})
#crosswalk<-make_firstCN_CW(plots)

#This gets the last coordinates for a given location
cores<-max(1,detectCores())
cl <- makePSOCKcluster(cores,outfile="../../debug.txt")
loaded<-.packages()
doParallel::registerDoParallel(cl)
Sys.time()
locations<-ddply(plots,"LOCATION_ID", function(x){
		x<-x[x$MEASYEAR==max(x$MEASYEAR,na.rm=TRUE),]
		x[1,]
	},.parallel=TRUE)
stopCluster(cl)
Sys.time()


locations<-locations[!is.na(locations$LAT)&!is.na(locations$LON),]
#Adds fields for metadata
locations<-add_plot_fields(locations)
#PLOTS AS SF and TO COMMON CRS
locations<-SpatialPointsDataFrame(locations[,c("LON","LAT")],data=locations)
locations<-st_as_sf(locations)
st_crs(locations)<-4269
locations<-st_transform(locations,st_crs(DESTCRS))

#BUFFER locations
locations_buffer<-st_buffer(locations,dist=BUFFERDIST)
st_crs(locations_buffer)<-st_crs(locations)

#Creates bounding boxes
bbox_wrap <- function(x) st_as_sfc(st_bbox(x))
locations_bbox <- locations_buffer
bbox<-purrr::map(locations_bbox$geometry,bbox_wrap)
bbox<-st_as_sfc(do.call(rbind,bbox))
locations_bbox$geometry<-NULL
st_geometry(locations_bbox)<-bbox
st_crs(locations_bbox)<-st_crs(locations)
#rm(bbox)

#Creates plots folders saves bboxes for each state
cl <- makePSOCKcluster(cores,outfile="../../debug.txt")
loaded<-.packages()
doParallel::registerDoParallel(cl)
Sys.time()
locations_bbox2<-ddply(locations_bbox,c("STATECD"),function(x){
			
	outfile<-paste("STATES",x$STATECD[1],"BBOX",
			paste("BBOX_",x$STATECD[1],".gpkg",sep=""),sep="/")
	x$plots_state_loc<-outfile
	x<-st_as_sf(x)
	a<-try(st_write(x,outfile,delete_layer = TRUE))
	x$plots_state_loc<-ifelse(inherits(a,"try-error"),NA,outfile)
	
	x<-ddply(x,"LOCATION_ID",function(y){
		unlink(y$plot_path[1])
		y$folder_created<-dir.create(y$plot_path[1])
		y<-st_as_sf(y[1,])
#		outfile_plot<-paste("STATES",y$STATECD[1],"PLOTS_LIDAR",y$LOCATION_ID[1],
#			paste("BBOX_",y$LOCATION_ID[1],".gpkg",sep=""),sep="/")
#		b<-try(st_write(y,outfile_plot,delete_layer = TRUE))
#		y$plot_bbox_loc<-ifelse(inherits(b,"try-error"),NA,outfile_plot)
		y
		},.progress="tk")
	
	x
			
		},.parallel=TRUE,.paropts=list(.packages=loaded))
stopCluster(cl)
Sys.time()

#Saves bboxes for all states
locations_bbox2<-st_as_sf(locations_bbox2)
st_write(locations_bbox2,paste(DESTFOLDER,"BBOX","BBOX_ALL.gpkg",sep="/"),delete_layer = TRUE)


#This justs creates a copy with some states for testing
unlink(TESTFOLDER,recursive=TRUE)
dir.create(TESTFOLDER,recursive=TRUE)
lapply(paste(TESTFOLDER,c(SUBFOLDERS,"STATES"),sep="/"),dir.create)

locations_bbox2<-locations_bbox2[locations_bbox2$STATECD%in%TESTSTATECODES,]
st_write(locations_bbox2,paste(TESTFOLDER,"BBOX","BBOX_ALL.gpkg",sep="/"),delete_layer = TRUE)

test_states_copies<-lapply(TESTSTATECODES,function(x){
	dir.create(paste(TESTFOLDER,"STATES",x,sep="/"))
	file.copy(paste(DESTFOLDER,"STATES",x,sep="/"),
			paste(TESTFOLDER,"STATES",sep="/"),recursive=TRUE)
		})

