
get_polygons <- 
function(regioes) {
  
  if(!require(jsonlite)) {
    install.packages("jsonlite")
  }
  if(!require(tidyverse)) {
    install.packages("tidyverse")
  }
  if(!require(geojsonio)) {
    install.packages("geojsonio")
  }
  
  nomes <- unique(regioes)
  nomes_temp <- str_replace_all(nomes, ' ', '%20')
  querys <- paste0("http://localhost:7070/search?q=", nomes_temp, "&format=geojson&polygon_geojson=1")
 
for (i in 1:length(regioes)) {  
     
  assign(nomes[i],
         Polygons(
         list(
         Polygon(matrix(unlist(read_json(querys[i])[["features"]][[1]][["geometry"]][["coordinates"]]),ncol=2, byrow = TRUE))
         ),nomes[i])) 
         # restringir ao primeiro índice ([[1]]) pode ser um problema. Teria que pensar em algum mode guardar os outros índices também.
  
}

  poligonos <- NULL  
for (i in 1:length(regioes))  {
  poligonos[i] <- list(get(nomes[i]))
  }
  
assign('poligonos',
       SpatialPolygons(poligonos),
       envir = .GlobalEnv)
  
}













