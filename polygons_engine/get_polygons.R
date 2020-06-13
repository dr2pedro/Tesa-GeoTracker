
get_polygons <- 
function(regioes, alias_list=NULL, doubles_list=NULL) {
  
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

if(length(nomes)!=regioes) {
  warning("Existem nomes repetidos no array. Os nomes que estiverem na 'doubles_list.csv' serão trocados segundo critério 
  estabelecido na função, caso contrário a função apresentará erro.")

if(!is.null(doubles_list)){
      
      if(is.data.frame(doubles_list)!=TRUE)
      stop("O objeto escolhido como doubles_list não é um data.frame.")

      if(ncol(doubles_list)<3)
      stop("Tem que ter três colunas no data.frame: uma contendo o nome real e duas contendo os nomes a serem trocados.\n 
      O ideal para os codinomes é inserir uma indicação da divisão geográfica acima, por exemplo: \n
      para o município de Mesquista um nome poderia ser 'MESQUITA RIO' e o outro 'MESQUITA MINAS'.")

      if(is.character(doubles_list[,1:ncol(doubles_list)])!=TRUE)
      stop("Alguma coluna não é de caractéres.")

      doubles_list = doubles_list

      nomes <- ifelse(nomes %in% doubles_list[,1]==TRUE, doubles_list[,2], nomes)

      
      } else {
        doubles_list <- read.csv('doubles_list.csv')
        nomes <- ifelse(nomes %in% doubles_list[,1]==TRUE, doubles_list[,2], nomes)
      }

}

  # a ideia da alias_list é ter um data.frame controle no qual quando determinado nome aparece que sabidamente não vai retornar o polígono
  # desejado ele será trocado por um nome que retorne o polígono correto. Caso o usuário tenha a sua lista ele pode usar,
  # caso contrário uma lista padrão será fornecida, inicialmente apenas com municípios, posteriormente com bairros e microrregiões,
  # vale ressaltar que para UF e países não existe esse tipo de erro.

    if(!is.null(alias_list)){
      
      if(is.data.frame(alias_list)!=TRUE)
      stop("o objeto escolhido como alias_list não é um data.frame")

      if(ncol(alias_list)!=2)
      stop("tem que ter duas colunas no data.frame: uma contendo o nome real e uma contendo o nome a ser trocado")

      if(is.character(alias_list[,1])!=TRUE)
      stop("a primeira coluna não é de caractéres")

      if(is.character(alias_list[,2])!=TRUE)
      stop("a segunda coluna não é de caractéres")

      alias_list = alias_list

      nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      } else {
        alias_list <- read.csv('alias_list.csv')
        nomes <- ifelse(nomes %in% alias_list[,1]==TRUE, alias_list[,2], nomes)
      }


  
nomes_temp <- str_replace_all(unlist(nomes), ' ', '%20')
querys <- paste0("http://localhost:7070/search?q=", nomes_temp, "&format=geojson&polygon_geojson=1")

nomes<- unique(regioes)

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













