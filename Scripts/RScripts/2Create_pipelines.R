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
load(paste(FOLDER,"Indexes.RData",sep="/"))


#Function to replace template with options NOT USED
fill_template<-function(template,options){
	
	if(!length(template)==length(options)){
		stop("Lengths of template and options do not match")
	}
	
	for(i in 1:length(template)){
		if(is.list(template[[i]])){
			template[[i]]<-fill_template(template[[i]],options[[i]])
		}else{
			if(is.na(options[[i]])){
				next
			}else{
				template[[i]]<-options[[i]]
			}
		}
	}
	return(template)
	
}

#function to run a function that generates a json pipeline for each row of
#a data frame or ssf object. 
#For each processing step the intersections with the indexes are passed to the function
#the. The fourth argument of the funtion is another function that generates the pipeline
#based on the data stored in a row of a df or sf object. These functions should return
#a list with two strings
add_json<-function(x,name,write=TRUE, pipelinefunc,...){
	extra_args<-list(...)
#	do not rely on adply, binding rows creates a new geom col
#	make a list and rbind them works
	list<-alply(x,1,function(y,pipelinefunc,extra_args){
				args<-list(row=y)
				args<-c(args,extra_args)
				a<-try({
							pipeline<-do.call(pipelinefunc,args)
							
							if(write){
								write(pipeline[[1]],pipeline[[2]])
							}
						})
				if(inherits(a,"try-error")){
					y[,name]<-NA
				}else{
					y[,name]<-pipeline[[2]]
				}
				y
				
			},pipelinefunc=pipelinefunc,extra_args=extra_args)
	do.call(rbind,list)
	
}

download_reproject_pipeline<-function(row,dest_crs="EPSG:5070+5703"){
	geom<-st_read(row[["bbox_path"]][1])
	geom<-st_transform(geom,st_crs(row$EPSG))
	polygon<-st_as_text(st_geometry(geom))
	bbox<-st_bbox(geom)
	bbox<-paste("([",bbox[1]," , ",bbox[3],"],[",
			bbox[2]," , ",bbox[4],"])",sep="")
	out_path<-paste(row$lidar_plot_folder,"PCBBOX",basename(row[["bbox_path"]][1]),sep="/")
	out_path<-gsub(".gpkg",".laz",out_path)
	print(out_path)
	
	pipeline<-list()
	
	pipeline[[1]]<-list(
			list(type="readers.ept",
					filename=row$url,
					bounds=bbox),
			list(type="filters.crop",
					polygon=polygon),
			list(type="filters.reprojection",
					in_srs=row$EPSG,
					out_srs=dest_crs),
			list(type="writers.las",
					compression="laszip",
					scale_x=0.01,
					scale_y=0.01,
					scale_z=0.01,
					offset_x="auto",
					offset_y="auto",
					offset_z="auto",
					filename=out_path))
	pipeline<-RJSONIO:::toJSON(pipeline[[1]])
	pipeline[[2]]<-gsub(".laz",".json",out_path)
	return(pipeline)
	
}

create_dtm_pipeline<-function(row,dest_crs="EPSG:5070+5703"){
	
	in_path<-paste(row$lidar_plot_folder,"PCBBOX",basename(row[["bbox_path"]][1]),sep="/")
	in_path<-gsub(".gpkg",".laz",in_path)
	out_path<-paste(row$lidar_plot_folder,"DTM",
			basename(row[["bbox_path"]][1]),sep="/")
	out_path<-gsub(".gpkg",".tif",out_path)
	print(out_path)
	pipeline<-c()
	
	pipeline[[1]]<-list(
			in_path,
			list(type="filters.crop",
					polygon=polygon),
			list(type="filters.reprojection",
					in_srs=row$EPSG,
					out_srs=dest_crs),
			list(type="writers.las",
					compression="laszip",
					scale_x=0.01,
					scale_y=0.01,
					scale_z=0.01,
					offset_x="auto",
					offset_y="auto",
					offset_z="auto",
					filename=out_path))
	pipeline<-RJSONIO:::toJSON(pipeline[[1]])
	pipeline[[2]]<-gsub(".laz",".json",out_path)
	return(pipeline)
	
}

create_bat<-function(x,pipelines,output="test.bat"){
	writeLines(paste("pdal pipeline ","\"",x[[pipelines]],"\"",sep=""),con=output)
}

#DOWNLOAD  PIPELINES
cores<-2
cl <- makePSOCKcluster(cores,outfile="../../debug.txt")
loaded<-.packages()
doParallel::registerDoParallel(cl)
Sys.time()
list_dfs<-dlply(combinations_directories[combinations_directories$state=="ALL",],
		c("state","index"),function(x){
			to_get<-st_read(x[1,"dest_file"][1])
			if(x$index_type=="USGS"){
				to_get$EPSG<-"EPSG:3857+5703"
			}
			result<-add_json(to_get,"pipe_download_reproject",
					write=TRUE,download_reproject_pipeline)
			st_write(result,x[1,"dest_file"][1],delete_layer=TRUE)
			return(result)
		},.parallel=TRUE,.paropts=list(.packages=loaded,
				.export=c("add_json","download_reproject_pipeline")))
stopCluster(cl)
Sys.time()

a<-run("F:/Paco/anaconda3/envs/PDAL/Library/bin/pdal.exe",
		args=c("pipeline",json))






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
