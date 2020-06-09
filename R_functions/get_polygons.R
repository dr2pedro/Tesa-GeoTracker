
get_polygons <- 
function(regioes, alias_list=NULL) {
  
  if(!require(jsonlite)) {
    install.packages("jsonlite")
  }
  if(!require(tidyverse)) {
    install.packages("tidyverse")
  }
  if(!require(geojsonio)) {
    install.packages("geojsonio")
  }
  if(!require(sp)) {
    install.packages("sp")
  }

nomes <- unique(regioes)

  # a ideia da alias_list é ter um data.frame controle no qual quando determinado nome aparece que sabidamente não vai retornar o polígono
  # desejado ele será trocado por um nome que retorne o polígono correto. Caso o usuário tenha a sua lista ele pode usar,
  # caso contrário uma lista padrão será fornecida, inicialmente apenas com municípios, posteriormente com bairros e microrregiões,
  # vale ressaltar que para UF e países não existe esse tipo de erro.

    if(!is.null(alias_list)){
      
      if(is.data.frame(alias_list)!=TRUE)
      stop("o objeto escolhido como alias list não é um data.frame")

      if(ncol(alias_list)!=2)
      stop("tem que ter duas colunas no data.frame: uma contendo o nome real e uma contendo o nome a ser trocado")

      if(is.character(alias_list[,1])!=TRUE)
      stop("a primeira coluna não é de caractéres")

      if(is.character(alias_list[,2])!=TRUE)
      stop("a segunda coluna não é de caractéres")

      alias_list = alias_list

      nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      } else {
        alias_list <- read.csv('alias_list.csv') # tem que implementar isso remoto, para ninguém deletar o arquivo localmente
        nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      }


  
nomes_temp <- str_replace_all(unlist(nomes), ' ', '%20')
querys <- paste0("http://localhost:7070/search?q=", nomes_temp, "&format=geojson&polygon_geojson=1")
nomes <- unlist(unique(regioes))

for (i in 1:length(regioes)) {  
     
  assign(nomes[i],
         Polygons(
         list(
         Polygon(matrix(unlist(read_json(querys[i])[["features"]][[1]][["geometry"]][["coordinates"]]),ncol=2, byrow = TRUE))
         ),nomes[i]))
           
}

  poligonos <- NULL  
      for (i in 1:length(regioes))  {
          poligonos[i] <- list(get(nomes[i]))
        }
  
assign('poligonos',
       SpatialPolygons(poligonos),
       envir = .GlobalEnv)
  
}













