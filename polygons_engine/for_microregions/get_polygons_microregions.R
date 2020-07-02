get_polygons.microregions <- 
function(microregions, alias_list=NULL, delete_cache=TRUE, aggregate=TRUE) {

# for microregions and counties/towns we have duplicated names. In this alias list is about five. To deal off it we need that the user
# declare the geografic division above the microregions, however this processes only triggers if some of the duplicated names appears. 

      if(!require(geojsonio)) {
        install.packages("geojsonio")
      }
      if(!require(stringi)) {
        install.packages("stringi")
      }
      if(!require(sp)) {
        install.packages("sp")
      }

  names <- toupper(unique(microregions))
  names <- stri_trans_general(str = names, id = "Latin-ASCII")

  if(length(names)!=length(microregions))
  warning("There are repeated names in the array. This function will consider only the unique values.")

# Alias_list is a data.frame of names accepted to be inserted and that this function will return a spatial object.
# In the first column you should put the name that could be written by anyone to refer a microregions, including 
# acronyms. Remember that all names will be coverted to upper case and removed accent. In the second column must have the
# name wanted to be understood. In the third column you should put a link to download the geojson file and in the last
# column some reference to subset the polygon since microregions came aggregated from states. 

      if(!is.null(alias_list)){
          
          if(is.data.frame(alias_list)!=TRUE)
          stop("The class of the object is not data.frame.")

          alias_list = alias_list
          
          if(is.character(alias_list[,1])!=TRUE)
          alias_list[,1]=as.character(alias_list[,1])

          if(is.character(alias_list[,2])!=TRUE)
          alias_list[,2]=as.character(alias_list[,2])
          
          if(is.character(alias_list[,3])!=TRUE)
          alias_list[,3]=as.character(alias_list[,3])
          
          
          if(all(microregions%in%alias_list[,1])==FALSE)
          
          stop(
            paste(
              "The following microregions \n\n",
            microregions[!microregions%in%alias_list[,1]], 
            "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
            )
          )
          
          names <- alias_list[which(alias_list[,1]%in%names),2]
    } else {
          alias_list <- read.csv('alias_list.csv', header = FALSE)
          
          alias_list[,1]=as.character(alias_list[,1])
          alias_list[,2]=as.character(alias_list[,2])
          alias_list[,3]=as.character(alias_list[,3])
          
          
          if(all(microregions%in%alias_list[,1])==FALSE)
            
            stop(
              paste(
                "The following microregions \n\n",
                names[!names%in%alias_list[,1]], 
                "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
              )
            )
          
          names <- alias_list[which(alias_list[,1]%in%names),2]
      }
  
  
  
 ## if there is a cache folder with some of json files already available, it should download only those who is missing. Remember that you can delete the cache folder in the
 ## delete_cache parameter. 
  
  if('cache'%in% dir()) {
    
    setwd('./cache')
    
    links <- unique(alias_list[which(alias_list[,2]%in%names),3])
    
    for(i in 1:length(links)) {
      if(!paste0(names[i],'.json') %in% dir()) {
            download.file(links[i], paste0(names[i],'.json'))
                  }
          }
    
  } else {
     
    dir.create('./cache')
    setwd('./cache')
    
    links <- unique(alias_list[which(alias_list[,2]%in%names),3])
    
    for(i in 1:length(names)) {
      download.file(links[i], paste0(names[i],'.json'))
           }   
  
    }
  
  
  ## defining a object to store all polygons 
  
  microregions_polygons <- NULL  
  
  
  ## reading the files as Spatial Polygon 
  
      for (i in 1:length(names)) {  
          
        assign(names[i],
            geojson_read(paste0(names[i],'.json'), what = 'sp')@polygons[geojson_read(paste0(names[i],'.json'), what = 'sp')@data$codarea%in%alias_list[which(alias_list[,1]%in%names[i]),4]][[1]],
            envir = .GlobalEnv)
        
        microregions_polygons[i] <- list(get(names[i]))
        
        }
        
      for (i in 1:length(microregions_polygons)){
        slot(microregions_polygons[[i]], "ID") = names[[i]]
      }
  
      assign('microregions_polygons',
             SpatialPolygons(microregions_polygons),
             envir = .GlobalEnv)
      
      rm(list = names, envir = .GlobalEnv)
  
      
      
  setwd("..") 
  
  
  ## whipe off the cache folder
  
  if(isTRUE(delete_cache))
  unlink('./cache', recursive=TRUE)    

  ## separating all polygons as individual objects if is FALSE aggregate
  if(isFALSE(aggregate)){
    
    for (i in 1:length(names)) {
        
          assign(names[i],
               microregions_polygons[[i]],
               envir = .GlobalEnv)
        }
    
    
    rm(microregions_polygons, envir = .GlobalEnv)
      
  }
  
}






