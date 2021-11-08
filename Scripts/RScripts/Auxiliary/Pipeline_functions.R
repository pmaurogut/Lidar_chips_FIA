# TODO: Add comment
# 
# Author: pmaurogut
###############################################################################
library("plyr")
library("sf")
library("RJSONIO")

#Function to replace template with options NOT USED
#' Title
#'
#' @param template 
#' @param options 
#'
#' @return
#' @export
#'
#' @examples
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

#Function to pass a df or sf it writes a file with the pdal pipeline commands to run
#' 
#' @param x 
#' @param pipelines 
#' @param output 
#' @returnType 
#' @return 
#' 
#' @author pmaurogut
#' @export
create_pipelines_bat<-function(x,pipelines,output="test.bat"){
	writeLines(paste("pdal pipeline ","\"",x[[pipelines]],"\"",sep=""),con=output)
}

#function to run a function that generates a json pipeline for each row of
#a data frame or ssf object. 
#For each processing step the intersections with the indexes are passed to the function
#the. The fourth argument of the funtion is another function that generates the pipeline
#based on the data stored in a row of a df or sf object. These functions should return
#a list with two strings
#' 
#' @param x dataframe or sf object 
#' @param field_name (string) field name to store the path to the command or json pipeline
#' @param write should the pipeline be written to disk?
#' @param pipelinefunc function that creates the pipelines from rows of x
#' @param ... extra arguments to pipelinefunc
#' @returnType dataframe or sf object 
#' @return dataframe or sf object with field_name populated
#' 
#' @author pmaurogut
#' @export
add_json<-function(x,field_name,write=TRUE, pipelinefunc,...){
	extra_args<-list(...)
	adply(x,1,function(y,pipelinefunc,extra_args){
				args<-list(row=y)
				args<-c(args,extra_args)
				a<-try({
							pipeline<-do.call(pipelinefunc,args)
							
							if(write){
								write(pipeline[1],pipeline[2])
							}
						})
				if(inherits(a,"try-error")){
					y[,field_name]<-NA
				}else{
					y[,field_name]<-pipeline[2]
				}
				y
				
			},pipelinefunc=pipelinefunc,extra_args=extra_args)
	
}

#' 
#' @param row sf with a single row. If row has more than one row only the fist
#' is used
#' @param field path to the plot geometry
#' @param dest_crs 
#' @returnType 
#' @return 
#' 
#' @author pmaurogut
#' @export
download_reproject_pipeline<-function(row,field="bbox_path",dest_crs="EPSG:5070+5703"){
	row<-row[1,]
	geom<-row
	geom<-st_sf(geom)
	geom<-st_transform(geom,st_crs(row$EPSG))
	polygon<-st_as_text(st_geometry(geom))
	bbox<-st_bbox(geom)
	bbox<-paste("([",bbox[1]," , ",bbox[3],"],[",
			bbox[2]," , ",bbox[4],"])",sep="")
	out_path<-paste(row$lidar_plot_folder,"PCBBOX",basename(row[[field]][1]),sep="/")
	out_path<-gsub(".gpkg",".laz",out_path)
	print(out_path)
	pipeline<-c()
	
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
	pipeline[2]<-gsub(".laz",".json",out_path)
	return(pipeline)
	
}

