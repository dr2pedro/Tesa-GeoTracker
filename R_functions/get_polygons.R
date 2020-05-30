
get_polygons <- 
function(regioes, whitelist=NULL) {
  
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

  # a ideia da whitelist é ter um data.frame controle no qual quando determinado nome aparece que sabidamente não vai retornar o polígono
  # desejado ele será trocado por um nome que retorne o polígono correto. Caso o usuário tenha a sua lista ele pode usar,
  # caso contrário ao longo de atualizações essa lista será confeccionada em uso.

    if(!is.null(whitelist)){
      
      if(is.data.frame(whitelist)!=TRUE)
      stop("o objeto escolhido não é um data.frame")

      if(ncol(whitelist)>2)
      stop("apenas duas colunas no data.frame são o suficiente: uma contendo o nome real e uma contendo o nome a ser trocado")

      if(is.character(whitelist[,1])!=TRUE)
      stop("a primeira coluna não é de caractéres")

      if(is.character(whitelist[,2])!=TRUE)
      stop("a segunda coluna não é de caractéres")

      whitelist = whitelist

      nomes <- ifelse(nomes %in% whitelist[,1]==TRUE, whitelist[,2], nomes)
      } else {
        whitelist <- read.csv('whitelist.csv') # tem que implementar isso remoto, para ninguém deletar o arquivo localmente
        nomes <- ifelse(nomes %in% whitelist[,1]==TRUE, whitelist[,2], nomes)
      }


  nomes_temp <- str_replace_all(nomes, ' ', '%20')
  querys <- paste0("http://localhost:7070/search?q=", nomes_temp, "&format=geojson&polygon_geojson=1")
  nomes <- unique(regioes)

for (i in 1:length(regioes)) {  
     
  assign(nomes[i],
         Polygons(
         list(
         Polygon(matrix(unlist(read_json(querys[i])[["features"]][[1]][["geometry"]][["coordinates"]]),ncol=2, byrow = TRUE))
         ),nomes[i])) # agora não importa o índice 1 já que a whitelist tem que dar conta disso.
           
}

  poligonos <- NULL  
      for (i in 1:length(regioes))  {
          poligonos[i] <- list(get(nomes[i]))
        }
  
assign('poligonos',
       SpatialPolygons(poligonos),
       envir = .GlobalEnv)
  
}













