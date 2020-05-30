
  sihsus_download <-
  
  function(anos, meses, UF){
     
    #dependências >>> tem que ver a dependência para instalar o jdk8 automático caso precise.
    if(!require(read.dbc)) {
      install.packages("read.dbc")
    }
    if(!require(sparklyr)) {
      install.packages("sparklyr")
      sparklyr::spark_install() 
    }
    if(!require(parallel)) {
      install.packages("parallel")
    }
    if(!require(benchmarkme)) {
      install.packages("benchmarkme")
    }
    if(!require(tidyverse)) {
      install.packages("tidyverse")
    }
   
    #parâmetros 
    anos <- sprintf("%02d", as.numeric(anos)%%100)
    meses <- sprintf("%02d", as.numeric(meses))
    UF <- UF
    
    #instalação
    local <- getwd()
    dir.create("Datasets")
    novolocal <- paste0(local, "/Datasets")
    
    #features
    arquivos <- paste0('RD',apply(expand.grid(UF, anos, meses), 1, paste, collapse=""), '.dbc')
    links <- paste0('ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/', arquivos)
    destino <- paste0(novolocal,'/', arquivos)
    
    ####baixando os arquivos
    for(i in 1:length(links)){
      download.file(links[i], destino[i])
    }
    
    
    ###lendo todos em uma lista
    setwd('Datasets')
    input <- dir()
    L <- as.numeric(length(input))
    
    dados <- NULL
    
    for (i in 1:L){
      dados[[i]] <- read.dbc(input[i])
      cat(input[i],'\n')
    }
    
    rm(input)
    rm(i)
    rm(L)
    
    dir.create("./bancos_csv")
    setwd("./bancos_csv")
    
    L <- as.numeric(length(dados))
    
    #convertendo a lista em bancos para leitura do Spark
    for(i in 1:L){
      write.csv(dados[[i]], paste0(i, ".csv"))
      }
    
    ifelse(is.null(sparklyr::spark_installed_versions()$spark), "instalando o spark" & sparklyr::spark_install(version = 2.4), "já possui spark e hadoop")
    
    conf <- spark_config()
    
    conf$spark.driver.cores <- round((parallel::detectCores()/2)+1) 
    conf$spark.driver.memory <- paste0(round(as.numeric(benchmarkme::get_ram())*0.000000001*0.65),'g') 
    conf$spark.executor.memory <- paste0(round(as.numeric(benchmarkme::get_ram())*0.000000001*0.65),'g')
    conf$spark.memory.fraction <- 0.8
    conf$`sparklyr.shell.executor-memory`<- paste0(round(as.numeric(benchmarkme::get_ram())*0.000000001*0.65),'G')
    conf$`sparklyr.shell.driver-memory`<- paste0(round(as.numeric(benchmarkme::get_ram())*0.000000001*0.65),'G')
    
    sc <- spark_connect(master = "local",
                        config = conf)
    
    top_rows <- read.csv("1.csv", nrows = 5)
    file_columns <- top_rows%>%
      map(function(x)"character")
    rm(top_rows)
    
    AIHS <- spark_read_csv(sc,
                           name = "AIHS",
                           path = ".",
                           memory = FALSE,
                           columns = file_columns,
                           infer_schema = FALSE)
    
    ETCs <- 
      AIHS%>%
      select(CEP, MUNIC_RES, NASC, SEXO, DT_INTER, DT_SAIDA, DIAG_PRINC, CNES, CID_MORTE, VAL_SH, VAL_SP)%>%
      collect()

  setwd('..')
  setwd('..')
      
  write.table(ETCs, 'etcs.txt', row.names = FALSE)
  
  assign("SIHSUS",ETCs,envir=.GlobalEnv)
    
  }

  ### e para o download dos anos de 2017 e 2018 digite:
  
  
  ### sihsus_download(c(09,10), c(1,2), "RJ")  