
get_polygons.neighborhoods <- 
function(macroregions, places, exclude_places_list=FALSE, cache=FALSE) {
  
# macroregions serão únicas e servirá para o primeiro loop que monta e desmonta a imagem do nominatim da região. 
# dentro de cada regição a busca será:

# localhost:8080/search.php?q=<nome do bairro>&format=geojson

# no caso em que o usuário ache a seleção desejada, em todas as não selecionadas o "place_id" deve ser armazenado em um array,
# que irá compor uma STRING separada por '%2C' e então o GET será novamente chamado incluindo um parâmetro de exclusão.

# localhost:8080/search.php?q=<nome do bairro>&format=geojson&exclude_place_ids=<STRING>

# esse último GET irá retornar o polígono desejado. 

# Existem duas formas de cache para esse método: 
# 1- salvar o JSON com o nome da cidade e do bairro em pastas de cache, diferente da API do IBGE isso pode ficar imenso.
# 2- salvar uma exclude_places_list na primeira coluna registra o ID selecionado, na segunda a cidade e na terceira um array dos places excluídos.



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
  if(!require(stringi)) {
    install.packages("stringi")
  }

  nomes <- toupper(unique(regioes))

  nomes <- stri_trans_general(str = nomes, id = "Latin-ASCII")

  if(length(nomes)!=length(regioes))
  stop("Existem nomes repetidos no array. Esses nomes devem ser substituidos pelo nome (referência da divisão geográfica superior), por exemplo:\n\nMESQUITA (RIO DE JANEIRO)\nMESQUITA (MINAS GERAIS).\n
       Lembre-se de incluir os nomes desses municípios na alias_list caso o polígono não seja retornado corretamente.")


  # a ideia da alias_list é ter um data.frame controle no qual quando determinado nome aparece que sabidamente não vai retornar o polígono
  # desejado ele será trocado por um nome que retorne o polígono correto. Caso o usuário tenha a sua lista ele pode usar,
  # caso contrário uma lista padrão será fornecida, inicialmente apenas com municípios, posteriormente com bairros e microrregiões,
  # vale ressaltar que para UF e países não existe esse tipo de erro.

    if(!is.null(alias_list)){
      
      if(is.data.frame(alias_list)!=TRUE)
      stop("O objeto escolhido como alias_list não é um data.frame.")

      if(ncol(alias_list)!=2)
      stop("Tem que ter duas colunas no data.frame: uma contendo o nome real e uma contendo o nome a ser trocado.")

      alias_list = alias_list
      
      if(is.character(alias_list[,1])!=TRUE)
      alias_list[,1]=as.character(alias_list[,1])

      if(is.character(alias_list[,2])!=TRUE)
      alias_list[,2]=as.character(alias_list[,2])
      

      nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      } else {
        alias_list <- read.csv('alias_list.csv', header = FALSE)
        
        alias_list[,1]=as.character(alias_list[,1])
        alias_list[,2]=as.character(alias_list[,2])
        
        nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      }


  
  nomes_temp <- str_replace_all(unlist(nomes), ' ', '%20')
  querys <- paste0("http://localhost:7070/search?q=", nomes_temp, "&format=geojson&polygon_geojson=1")

  querys[nomes=="MONTE CARMELO"] <- "https://nominatim.openstreetmap.org/search?q=MONTE%20CARMELO&format=geojson&polygon_geojson=1"
  querys[nomes=="CORUMBATAI"] <- "https://nominatim.openstreetmap.org/search?q=CORUMBATAI&format=geojson&polygon_geojson=1"
  
  # MONTE CARMELO,MONTE CARMELO tem que pegar de fora esse monte carmelo. corumbatai


  nomes <- toupper(unique(regioes))

  nomes <- stri_trans_general(str = nomes, id = "Latin-ASCII")

  for (i in 1:length(nomes)) {  
     
      assign(nomes[i],
             Polygons(
             list(
             Polygon(matrix(unlist(read_json(querys[i])[["features"]][[1]][["geometry"]][["coordinates"]]),ncol=2, byrow = TRUE))
             ),nomes[i]))
               
      }


  poligonos <- NULL  
      for (i in 1:length(nomes))  {
          poligonos[i] <- list(get(nomes[i]))
        }
  
  assign('poligonos',
         SpatialPolygons(poligonos),
         envir = .GlobalEnv)
    
  }













