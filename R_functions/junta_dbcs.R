library(read.dbc)

##### Daqui vai sair o filtro, tem que ser dinâmico, mas focado na tese. #####


drsai_mg_e_es <- 
AIHS%>%
  filter(DIAG_PRINC == "A00" | DIAG_PRINC == "A02" |
         DIAG_PRINC == "A03" | DIAG_PRINC == "A04" |
         DIAG_PRINC == "A06" | DIAG_PRINC == "A07" |
         DIAG_PRINC == "A08" | DIAG_PRINC == "A01" |
         DIAG_PRINC == "B15" | DIAG_PRINC == "A90" |
         DIAG_PRINC == "A91" | DIAG_PRINC == "A95" |
         DIAG_PRINC == "B55" | DIAG_PRINC == "B74" |
         DIAG_PRINC == "B50" | DIAG_PRINC == "B51" |
         DIAG_PRINC == "B52" | DIAG_PRINC == "B53" |
         DIAG_PRINC == "B54" | DIAG_PRINC == "B57" |
         DIAG_PRINC == "B65" | DIAG_PRINC == "A27" |
         DIAG_PRINC == "A71" | DIAG_PRINC == "H10" |
         DIAG_PRINC == "B35" | DIAG_PRINC == "B36" |
         DIAG_PRINC == "B67" | DIAG_PRINC == "B76" |
         DIAG_PRINC == "B77" | DIAG_PRINC == "B78" |
         DIAG_PRINC == "B79" | DIAG_PRINC == "B80" |
         DIAG_PRINC == "B68" | DIAG_PRINC == "B69" |
         DIAG_PRINC == "A92" | DIAG_PRINC == "U06")%>%
  select(ANO_CMPT, MES_CMPT, MUNIC_RES, NASC, SEXO, UTI_MES_TO, UTI_INT_TO, QT_DIARIAS, 
         PROC_REA, VAL_SH, VAL_SP, VAL_TOT, VAL_UTI, US_TOT, DT_INTER, DT_SAIDA, DIAG_PRINC,
         IDADE, DIAS_PERM, CBOR)

drsai_mg_e_es%>%
  tally

drsai_mg_e_es <- drsai_mg_e_es%>%
  compute("drsai_total") # aumentando a velocidade


# importando o banco para um objeto comum, j? que s? tem 70mil da pra trabalhar
# como data.frame normal.

base <- data.frame(drsai_mg_e_es)


drsai_atingidos <-  
  drsai_mg_e_es%>%
  filter(MUNIC_RES == "310110" | MUNIC_RES == "310180" |
         MUNIC_RES == "310570" | MUNIC_RES == "310630" |
         MUNIC_RES == "310780" | MUNIC_RES == "310925" |
         MUNIC_RES == "311340" | MUNIC_RES == "311840" |
         MUNIC_RES == "312000" | MUNIC_RES == "312180" |
         MUNIC_RES == "312580" | MUNIC_RES == "312730" |
         MUNIC_RES == "312770" | MUNIC_RES == "312930" |
         MUNIC_RES == "313115" | MUNIC_RES == "313130" |
         MUNIC_RES == "313410" | MUNIC_RES == "314000" |
         MUNIC_RES == "314030" | MUNIC_RES == "314435" |
         MUNIC_RES == "314995" | MUNIC_RES == "315053" |
         MUNIC_RES == "315400" | MUNIC_RES == "315430" |
         MUNIC_RES == "315490" | MUNIC_RES == "315500" |
         MUNIC_RES == "315740" | MUNIC_RES == "315895" |
         MUNIC_RES == "316100" | MUNIC_RES == "316340" |
         MUNIC_RES == "316400" | MUNIC_RES == "316556" |
         MUNIC_RES == "316770" | MUNIC_RES == "316870" |
         MUNIC_RES == "316950" | MUNIC_RES == "320060" |
         MUNIC_RES == "320080" | MUNIC_RES == "320150" |
         MUNIC_RES == "320320" | MUNIC_RES == "320335" |
         MUNIC_RES == "320490")

drsai_atingidos <- data.frame(drsai_atingidos)  


library(lubridate)

#vamos manter s? os data.frame de trabalho

spark_disconnect(sc)
rm(AIHS)
rm(conf)
rm(sc)  
rm(file_columns)
rm(drsai_mg_e_es)

write.csv(drsai_de_todos, "drsai_de_todos.csv")
write.csv(drsai_atingidos, "drsai_atingidos.csv")

drsai_de_todos$data_internacao <- 
ymd(drsai_de_todos$data_internacao)

drsai_de_todos <- 
drsai_de_todos%>%
  select(-ano,-mes,-dia)
  
drsai_de_todos$data_nascimento <- ymd(drsai_de_todos$data_nascimento)

drsai_de_todos <- 
drsai_de_todos%>%
  arrange(data_internacao)

drsai_de_todos$semana_epi <- 
epiweek(drsai_de_todos$data_internacao)

drsai_de_todos$ano_epi <- 
  epiyear(drsai_de_todos$data_internacao)

drsai_de_todos$sexo <- factor(drsai_de_todos$sexo, labels = c('Masculino', 'Feminino'))
drsai_de_todos$CID <- factor(drsai_de_todos$CID)

levels(drsai_de_todos$CID) <- c('Infec??es intestinais', 'Dengue', 'Febre hemorr?gica da dengue',
                                        'Mal?ria', 'Leishmaniose', 'Doen?a de Chagas', 'Tricur?ase',
                                        'Oxiur?ase')

drsai_de_todos$procedimento <- factor(drsai_de_todos$procedimento)


levels(drsai_de_todos$procedimento) <- c('Atendimento cl?nica pedi?trica', 
                                         'Atendimento cl?nica cir?rgica',
                                         'Atendimento cl?nica m?dica', 
                                         'Tratamento da dengue cl?ssica', 
                                         'Tratamento da dengue hemorr?gica',
                                         'Tratamento de outras doen?as bacterianas', 
                                         'Tratamento de doen?as de protozo?rios',
                                         'Tratamento de doen?as infeciosas intestinais', 
                                         'Tratamento de febre por arboviroses',
                                         'Tratamento de Helmint?ase', 
                                         'Tratamento de mal?ria', 
                                         'Tratamento de doen?as causadas por espiroquetas',
                                         'Tratamento de outras doen?as virais', 
                                         'Tratamento de anemia apl?stica', 
                                         'Tratamento de p?rpura',
                                         'Tratamento de outras doen?as no sangue', 
                                         'Tratamento de desnutri??o', 
                                         'Tratamento de diabetes', 
                                         'Tratamento de dist?rbios metab?licos', 
                                         'Tratamento de polineuropatias', 
                                         'Tratamento de choque hipovol?mico',
                                         'Tratamento de crise hipertensiva', 
                                         'Tratamento de embolia pulmonar', 
                                         'Tratamento de insufici?ncia card?aca',
                                         'Tratamento de doen?as do est?mago, es?fago e duodeno', 
                                         'Tratamento de doen?as do f?gado', 
                                         'Tratamento de outras doen?as digestivas',
                                         'Tratamento de outras doen?as do intestino', 
                                         'Tratamento de transtorno de biles ou p?ncreas', 
                                         'Tratamento de dermatites e eczemas',
                                         'Tratamento de estafilococciais', 
                                         'Tratamento estreptococciais', 
                                         'Tratamento de outras doen?as de pele', 
                                         'Tratamento de poliartropatia inflamat?rias',
                                         'Tratamento de intecorr?ncias cl?nicas pu?rperio',
                                         'Tratamento de intercorr?ncias cl?nicas na gravidez', 
                                         'Tratamento de doen?as cr?nicas vias aereas inferiores', 
                                         'Tratamento de infec??es agudas vias a?reas superiores',
                                         'Tratamento de pneuomonia ou influenza', 
                                         'tratamento de doen?as glomerulares', 
                                         'Tratamento de outras doen?as do aparelho urin?rio',
                                         'Tratamento de afec??es relacionadas ao HIV', 
                                         'Tratamento de intercorr?ncias cl?nicas de paciente oncol?gico', 
                                         'Tratamento cl?nico de paciente oncol?gico', 
                                         'Tratamento de intercorr?ncias de paciente renal', 
                                         'Tratamento de pielonefrite',
                                         'Tratamento de insufici?ncia renal aguda', 
                                         'Tratamento de insufici?ncia renal cr?nica', 
                                         'Parto normal em gesta??o de alto risco',
                                         'Deriva??o ventricular externar-subgaleal', 
                                         'Reconstru??o de c?mara anterior do olho', 
                                         'Angioplastia coronariana', 
                                         'Hernioplastia umbilical',
                                         'Laparotomia explorat?ria', 
                                         'Tratamento cir?rgico de artrite infecciosa', 
                                         'Curetagem p?s-aborto', 
                                         'Toracostomia com drenagem pleural', 
                                         'Doa??o de ?rg?os e tecidos para transplante', 
                                         'Tratamento de intercorr?ncia p?s-transplante de sangue', 
                                         'Tratamento de intercorr?ncia p?s-transplante de rim')


drsai_atingidos <- merge(drsai_de_todos, MUNICIPIOS_ATINGIDOS_1_, by='CODIBGE')

rm(drsai_de_todos)


#tem que remover os procedimentos que n?o fazem sentido
unique(drsai_atingidos$procedimento)


library(extrafont)


drsai_atingidos%>%
  mutate(anosemana=as.factor(ano_epi):as.factor(semana_epi))%>%
  arrange(ano_epi, semana_epi)%>%
  group_by(ano_epi, semana_epi,anosemana)%>%
  summarise(n=n())%>%
  mutate(anosemana=as.numeric(anosemana))%>%
  ggplot(aes(x=anosemana, y=n))+geom_line()+geom_vline(xintercept = 465, linetype='dashed')+
  ylab('Total de interna??es')+xlab('Semana epidemiol?gica')+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.title = element_text(family = "Times New Roman", size = 14, face = 'bold'),
        axis.text = element_text(family = "Times New Roman", size = 12))

  


drsai_atingidos%>%
  group_by(ano_epi,semana_epi)%>%
  summarise(n=n())%>%
  ggplot(aes(x=semana_epi, y=n))+geom_line(aes(color=factor(ano_epi)), size=0.8)+
  geom_vline(xintercept = 40, linetype='dashed')+ylab('Total de interna??es')+xlab('Semana epidemiol?gica')+
  theme_bw()+scale_color_manual(values = c('#D3D3D3','#C8C8C8','#BEBEBE','#B0B0B0','#A8A8A8','#989898','#909090','#101010','#282828','#383838','#484848','#484848', '#505050'))+
  theme(legend.title = element_blank(),
        axis.title = element_text(family = "Times New Roman", size = 14, face = 'bold'),
        axis.text = element_text(family = "Times New Roman", size = 12))


drsai_atingidos%>%
  group_by(ano_epi, CID)%>%
  summarise(n=n())%>%
  mutate(prop=prop.table(n))%>%
  ggplot(aes(x=ano_epi, y=prop, fill=CID))+geom_bar(stat = 'identity')+ylab('Preval?ncia')+xlab('Ano')+
  theme_bw()+scale_fill_grey(start = .3)+
  theme(legend.title = element_blank(),
        axis.title = element_text(family = "Times New Roman", size = 14, face = 'bold'),
        axis.text = element_text(family = "Times New Roman", size = 12))

library(geojsonio)
dir.create('json')
download.file("https://servicodados.ibge.gov.br/api/v2/malhas/31?resolucao=5&formato=application/json", "MG.json")
download.file("https://servicodados.ibge.gov.br/api/v2/malhas/32?resolucao=5&formato=application/json", "ES.json")
estadoMG<- geojson_read("MG.json", what = "sp")
estadoES<- geojson_read("json/ES.json", what = "sp")
download.file("http://servicodados.ibge.gov.br/api/v1/localidades/estados/32/municipios", 'json/ESnomes.json')
download.file("http://servicodados.ibge.gov.br/api/v1/localidades/estados/31/municipios", 'json/MGnomes.json')
nomesES <- jsonlite::read_json("json/ESnomes.json")
nomesMG <- jsonlite::read_json("json/MGnomes.json")


estadoES@data$id <- type.convert(estadoES@data$id)
estadoMG@data$id <- type.convert(estadoMG@data$id)

for (i in 1:78){
  estadoES$id[i] <- nomesES[[i]]$nome
}

for (i in 1:853){
  estadoMG@data$id[i]<- nomesMG[[i]]$nome
}

rm(i)

estadoMG <- 
estadoMG[estadoMG@data$id=='Aimor?s'|estadoMG@data$id=='Alpercata'|estadoMG@data$id=='Barra Longa'|estadoMG@data$id=='Belo Oriente'|
         estadoMG@data$id=='Bom Jesus do Galho'|estadoMG@data$id=='Bugre'|estadoMG@data$id=='Caratinga'|estadoMG@data$id=='Conselheiro Pena'|
         estadoMG@data$id=='C?rrego Novo'|estadoMG@data$id=='Dion?sio'|estadoMG@data$id=='Fernandes Tourinho'|estadoMG@data$id=='Galil?ia'|
         estadoMG@data$id=='Governador Valaderes'|estadoMG@data$id=='Iapu'|estadoMG@data$id=='Ipaba'|estadoMG@data$id=='Ipatinga'|estadoMG@data$id=='Itueta'|
         estadoMG@data$id=='Mariana'|estadoMG@data$id=='Marli?ria'|estadoMG@data$id=='Naque'|estadoMG@data$id=='Periquito'|estadoMG@data$id=="Pingo-d'?gua"|
         estadoMG@data$id=='Raul Soares'|estadoMG@data$id=='Resplendor'|estadoMG@data$id=='Rio Casca'|estadoMG@data$id=='Rio Doce'|
         estadoMG@data$id=='Santo Cruz do Escalvado'|estadoMG@data$id=='Santana do Para?so'|estadoMG@data$id=='S?o Domingo do Prata'|
         estadoMG@data$id=='S?o Jos? do Goiabal'|estadoMG@data$id=='S?o Pedro dos Ferros'|estadoMG@data$id=='Sem-Peixe'|estadoMG@data$id=='Sobr?lia'|
         estadoMG@data$id=='Tim?teo'|estadoMG@data$id=='Tumiritinga',]

estadoES <- 
estadoES[estadoES@data$id=="Aracruz"|estadoES@data$id=="Baixo Guandu"|estadoES@data$id=="Colatina"|estadoES@data$id=="Linhares"|estadoES@data$id=="Maril?ndia"|estadoES@data$id=="S?o Mateus",]

teste <- 
rbind(estadoES,estadoMG)


leaflet(teste)%>%
  setView(-55, -17, 4)%>%
  addPolygons()
