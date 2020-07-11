get_polygons.counties <- 
function(counties, alias_list=NULL, double_list= NULL, delete_cache=TRUE, aggregate=TRUE) {

# for counties we have duplicated names. If some duplicated name appears in the array the function must
# detect and suggest new names. These names must be on the alias_list in case the user agree and modify as said. 

      if(!require(geojsonio)) {
        install.packages("geojsonio")
      }
      if(!require(stringi)) {
        install.packages("stringi")
      }
      if(!require(sp)) {
        install.packages("sp")
      }

  names <- toupper(unique(counties))
  names <- stri_trans_general(str = names, id = "Latin-ASCII")

  # Modifing the warning to include a stop in case some of these names are in the double_list.

  if(length(names)!=length(counties))
  warning("There are repeated names in the array. This function will consider only the unique values.")
  
  # this list must contain the names of the duplicated counties, the first option for tge new name and second option.
  if(!is.null(double_list)){

    if(is.data.frame(double_list)!=TRUE)
    stop("The class of the object double_list is not a data.frame.")

    double_list = double_list

    if(is.character(double_list[,1]!=TRUE))
    double_list[,1]=as.character(double_list[,1])

    if(is.character(double_list[,2])!=TRUE)
    double_list[,2]=as.character(double_list[,2])
    
    if(is.character(double_list[,3])!=TRUE)
    double_list[,3]=as.character(double_list[,3])
    
    
    if(any(counties%in%double_list[,1])==TRUE)
    stop(
            paste(
              "The following counties \n\n",
            counties[counties%in%double_list[,1]], 
            "\n\n is in the list of duplicated names you should change to\n\n",
            double_list[which(double_list[,1]%in%counties),2], '\n',
            double_list[which(double_list[,1]%in%counties),3],'\n',
            double_list[which(double_list[,1]%in%counties),4],'\n',
            double_list[which(double_list[,1]%in%counties),5],'\n'
            )
          )
      } else {
        double_list <- read.csv('double_list.csv', header = FALSE)
          
          double_list[,1]=as.character(double_list[,1])
          double_list[,2]=as.character(double_list[,2])
          double_list[,3]=as.character(double_list[,3])

          if(any(counties%in%double_list[,1])==TRUE)
    stop(
            paste(
              "The following counties \n\n",
            counties[counties%in%double_list[,1]], 
            "\n\n is in the list of duplicated names you should change to\n\n",
            double_list[which(double_list[,1]%in%counties),2], '\n',
            double_list[which(double_list[,1]%in%counties),3],'\n',
            double_list[which(double_list[,1]%in%counties),4],'\n',
            double_list[which(double_list[,1]%in%counties),5],'\n'
            )
          )
      }

# Alias_list is a data.frame of names accepted to be inserted and that this function will return a spatial object.
# In the first column you should put the name that could be written by anyone to refer a counties, including 
# acronyms. Remember that all names will be coverted to upper case and removed accent. In the second column must have the
# name wanted to be understood. In the third column you should put a link to download the geojson file and in the last
# column some reference to subset the polygon since counties came aggregated from states. 

      if(!is.null(alias_list)){
          
          if(is.data.frame(alias_list)!=TRUE)
          stop("The class of the object alias_list is not data.frame.")

          alias_list = alias_list
          
          if(is.character(alias_list[,1])!=TRUE)
          alias_list[,1]=as.character(alias_list[,1])

          if(is.character(alias_list[,2])!=TRUE)
          alias_list[,2]=as.character(alias_list[,2])
          
          if(is.character(alias_list[,3])!=TRUE)
          alias_list[,3]=as.character(alias_list[,3])
          
          
          if(all(counties%in%alias_list[,1])==FALSE)
          
          stop(
            paste(
              "The following counties \n\n",
            counties[!counties%in%alias_list[,1]], 
            "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
            )
          )
          
          names <- alias_list[which(alias_list[,1]%in%names),2]
          json_names <- alias_list[which(alias_list[,1]%in%names),4]
    } else {
          alias_list <- read.csv('alias_list.csv', header = FALSE)
          
          alias_list[,1]=as.character(alias_list[,1])
          alias_list[,2]=as.character(alias_list[,2])
          alias_list[,3]=as.character(alias_list[,3])
          
          
          if(all(counties%in%alias_list[,1])==FALSE)
            
            stop(
              paste(
                "The following counties \n\n",
                names[!names%in%alias_list[,1]], 
                "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
              )
            )
          
          names <- alias_list[which(alias_list[,1]%in%names),2]
          json_names <- alias_list[which(alias_list[,1]%in%names),4]
      }
  
 ## The slot to deal off the doubles names. 
 ## must to be a array of indexes.
  
 ## if there is a cache folder with some of json files already available, it should download only those who is missing. Remember that you can delete the cache folder in the
 ## delete_cache parameter. 
  
  if('cache'%in% dir()) {
    
    setwd('./cache')
    
    links <- unique(alias_list[which(alias_list[,2]%in%names),3])
    links_names <- unique(alias_list[which(alias_list[,2]%in%names),4])
    
    for(i in 1:length(links)) {
      if(!paste0(links_names[i],'.json') %in% dir()) {
            download.file(links[i], paste0(links_names[i],'.json'))
                  }
          }
    
  } else {
     
    dir.create('./cache')
    setwd('./cache')
    
    links <- unique(alias_list[which(alias_list[,2]%in%names),3])
    links_names <- unique(alias_list[which(alias_list[,2]%in%names),4])
    
    for(i in 1:length(links)) {
      if(!paste0(links_names[i],'.json') %in% dir()) {
        download.file(links[i], paste0(links_names[i],'.json'))
                   }
           }   
  
    }
  
  
  ## defining a object to store all polygons 
  
  counties_polygons <- NULL  
  
  
  ## reading the files as Spatial Polygon 
  
      for (i in 1:length(names)) {  
          
        assign(names[i],
            geojson_read(paste0(json_names[i],'.json'), what = 'sp')@polygons[geojson_read(paste0(json_names[i],'.json'), what = 'sp')@data$codarea%in%alias_list[which(alias_list[,1]%in%names[i]),5]][[1]],
            envir = .GlobalEnv)
        
        counties_polygons[i] <- list(get(names[i]))
        
        }
        
      for (i in 1:length(counties_polygons)){
        slot(counties_polygons[[i]], "ID") = names[[i]]
      }
  
      assign('counties_polygons',
             SpatialPolygons(counties_polygons),
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
               counties_polygons[[i]],
               envir = .GlobalEnv)
        }
    
    
    rm(counties_polygons, envir = .GlobalEnv)
      
  }
  
}






