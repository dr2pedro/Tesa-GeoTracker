
get_polygons.country <- 
function(country, alias_list=NULL, delete_cache=TRUE) {
  
      if(!require(geojsonio)) {
        install.packages("geojsonio")
      }
      if(!require(stringi)) {
        install.packages("stringi")
      }
      if(!require(sp)) {
        install.packages("sp")
      }

  names <- toupper(unique(country))
  names <- stri_trans_general(str = names, id = "Latin-ASCII")

  if(length(names)!=length(country))
  warning("There are repeated names in the array. This function will consider only the unique values.")

# Alias_list is a data.frame of names accepted to be inserted and that this function will return a spatial object.
# In the first column you should put the name that could be written by anyone to refer a country, including 
# acronyms. Remember that all names will be coverted to upper case and removed accent. 

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
          
          if(all(country%in%alias_list[,1])==FALSE)
          stop(
            paste(
              "The following country \n\n",
            country[!country%in%alias_list[,1]], 
            "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
            )
          )
          names <- ifelse(names %in% alias_list[,1]==TRUE, alias_list[,2], names)
    } else {
          alias_list <- read.csv('alias_list.csv', header = FALSE)
          
          alias_list[,1]=as.character(alias_list[,1])
          alias_list[,2]=as.character(alias_list[,2])
          alias_list[,3]=as.character(alias_list[,3])
          
          if(all(country%in%alias_list[,1])==FALSE)
          stop(
            paste(
              "The following country \n\n",
            country[!country%in%alias_list[,1]], 
            "\n\n is not defined in the alias list, please edit alias_list.csv file and include this region with a link to download the geojson."
            )
          )

          names <- ifelse(names %in% alias_list[,1]==TRUE, alias_list[,2], names)
      }


 ## if there is a cache folder with some of json files already available, it should download only those who is missing. Remember that you can delete the cache folder in the
 ## delete_cache parameter. 
  
  if('cache'%in% dir()) {
    
    setwd('./cache')
    
    for(i in 1:length(names)) {
      if(!paste0(names[i],'.json') %in% dir()) {
            download.file(alias_list[i,3], paste0(names[i],'.json'))
                  }
          }
    
  } else {
     
    dir.create('./cache')
    setwd('./cache')
    
        for(i in 1:length(names)) {
        download.file(alias_list[i,3], paste0(names[i],'.json'))
                  }   
  
    }
  
  
  
  ## reading the files as Spatial Polygon Data.frame.     
  
      for (i in 1:length(names)) {  
          assign(names[i],
            geojson_read(paste0(names[i],'.json'), what = 'sp'),
            envir = .GlobalEnv
            )               
        }
  
  setwd("..") 
  
  ## whipe off the cache folder
  
  if(isTRUE(delete_cache))
  unlink('./cache', recursive=TRUE)    

  }



