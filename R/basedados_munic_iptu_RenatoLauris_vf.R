#0. remover os dados salvos
rm(list = ls())
#1. Instalar Pacotes e carregar dados salvos previamente --------------------------------------------------------
# instala.pacotes <- c("electionsBR","ribge","tidyverse","readxl",
#                      "stringr","lubridate","Hmisc","ecoseries","dplyr","qwraps2")
# install.packages(instala.pacotes)
# install.packages("devtools")
# devtools::install_github("tbrugz/ribge")

  # carregar pacotes
  library(electionsBR)
  library(ribge)
  library(tidyverse)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(Hmisc)
  library(ecoseries)
  library(dplyr)
  library(qwraps2)

  # endereco com os arquivos de dados armazenados
  endereco <- "C:\\13.Pos PUCRS\\_Trabalho final\\Projeto no github\\iptu_trab_pos\\data\\"
  setwd(endereco)
  getwd()

  # carregar dados previamente obtidos
  load("C:\\13.Pos PUCRS\\_Trabalho final\\Projeto no github\\iptu_trab_pos\\data\\base_carregada_21_11.RData")
  # salva imagem dos dados utilizados
  save.image("base_carregada_04_12.RData")

#2. Iptu STN/Siconfi  ------------------------------------------------------------

# carregar dados SICONFI IPTU por Municipios 2013 a 2017
# dados obtidos em https://siconfi.tesouro.gov.br/siconfi/index.jsf
# arquivos armazenados no endereco definido antes

#2.1. siconfi 2013 ------------------------------------------------------------

rec.2013 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2013.zip",skip=3)

# numeros de conta SICONFI
# 1.0.0.0.00.00.00 - Receitas Correntes
# 1.1.1.0.00.00.00 - Impostos
# 1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ? IPTU
# 1.1.1.2.08.00.00 - Imposto sobre Transmiss?o Inter Vivos" de Bens Im?veis e de Direitos Reais sobre Im?veis ? ITBI"
# 1.1.1.3.05.00.00 - Imposto sobre Servi?os de Qualquer Natureza ? ISSQN
# 1.7.0.0.00.00.00 - Transfer?ncias Correntes
# 1.7.2.0.00.00.00 - Transfer?ncias Intergovernamentais
# 1.7.2.1.00.00.00 - Transfer?ncias da Uni?o
# 1.7.2.2.00.00.00 - Transfer?ncias dos Estados
# 1.7.6.0.00.00.00 - Transfer?ncias de Conv?nios

# codigos a buscar na base
buscaCodSiconfi <- c('1.0.0.0.00.00.00','1.1.1.0.00.00.00','1.1.1.2.02.00.00','1.1.1.2.08.00.00','1.1.1.3.05.00.00','1.7.0.0.00.00.00','1.7.2.0.00.00.00','1.7.2.1.00.00.00','1.7.2.2.00.00.00','1.7.6.0.00.00.00')

rec.2013 <-  mutate(rec.2013,nroConta=str_sub(Conta,1,16), ano=2013)

rec.2013.iptu <-  filter(rec.2013, nroConta %in% buscaCodSiconfi)

rec.2013.iptu <- spread(rec.2013.iptu, Coluna, Valor)

colnames(rec.2013.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2013.iptu <- rec.2013.iptu %>% select(-conta,-deducoes.receita)
rec.2013.iptu <- rec.2013.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2013.iptu <- rec.2013.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2013.iptu <- spread(rec.2013.iptu, nroConta, receitas.realizadas)

colnames(rec.2013.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                           "recCorr","impostos","iptu","itbi","issqn",
                           "transfCorr","transfIntergov","transfUniao",
                           "transfEst","transfConv")
rm(rec.2013)  

#2.2. siconfi 2014 ------------------------------------------------------------

rec.2014 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2014.zip",skip=3)

# numeros de conta SICONFI
# 1.0.0.0.00.00.00 - Receitas Correntes
# 1.1.1.0.00.00.00 - Impostos
# 1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ? IPTU
# 1.1.1.2.08.00.00 - Imposto sobre Transmiss?o Inter Vivos" de Bens Im?veis e de Direitos Reais sobre Im?veis ? ITBI"
# 1.1.1.3.05.00.00 - Imposto sobre Servi?os de Qualquer Natureza ? ISSQN
# 1.7.0.0.00.00.00 - Transfer?ncias Correntes
# 1.7.2.0.00.00.00 - Transfer?ncias Intergovernamentais
# 1.7.2.1.00.00.00 - Transfer?ncias da Uni?o
# 1.7.2.2.00.00.00 - Transfer?ncias dos Estados
# 1.7.6.0.00.00.00 - Transfer?ncias de Conv?nios

# codigos a buscar na base
buscaCodSiconfi <- c('1.0.0.0.00.00.00','1.1.1.0.00.00.00','1.1.1.2.02.00.00','1.1.1.2.08.00.00','1.1.1.3.05.00.00','1.7.0.0.00.00.00','1.7.2.0.00.00.00','1.7.2.1.00.00.00','1.7.2.2.00.00.00','1.7.6.0.00.00.00')

rec.2014 <-  mutate(rec.2014,nroConta=str_sub(Conta,1,16), ano=2014)

rec.2014.iptu <-  filter(rec.2014, nroConta %in% buscaCodSiconfi)

rec.2014.iptu <- spread(rec.2014.iptu, Coluna, Valor)

rec.2014.iptu <- rec.2014.iptu[,-c(8:9)]

# Adicionar linha de comando distinta para os anos de 2014 em diante!!!
# rec.2014.iptu <- rec.2014.iptu[,-c(8:9)]

colnames(rec.2014.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2014.iptu <- rec.2014.iptu %>% select(-conta,-deducoes.receita)
rec.2014.iptu <- rec.2014.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2014.iptu <- rec.2014.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2014.iptu <- spread(rec.2014.iptu, nroConta, receitas.realizadas)

colnames(rec.2014.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")

rm(rec.2014)  

#2.3. siconfi 2015 ------------------------------------------------------------

rec.2015 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2015.zip",skip=3)

rec.2015 <-  mutate(rec.2015,nroConta=str_sub(Conta,1,16), ano=2015)

rec.2015.iptu <-  filter(rec.2015, nroConta %in% buscaCodSiconfi)

rec.2015.iptu <- spread(rec.2015.iptu, Coluna, Valor)

rec.2015.iptu <- rec.2015.iptu[,-c(8:9)]

# Adicionar linha de comando para os anos de 2014 em diante!!!
# rec.2015.iptu <- rec.2015.iptu[,-c(8:9)]

colnames(rec.2015.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

# rec.2015.iptu <- mutate(rec.2015.iptu,
#                         valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2015.iptu <- rec.2015.iptu %>% select(-conta,-deducoes.receita)
rec.2015.iptu <- rec.2015.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2015.iptu <- rec.2015.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2015.iptu <- spread(rec.2015.iptu, nroConta, receitas.realizadas)

colnames(rec.2015.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")

rm(rec.2015)

#2.4. siconfi 2016 ------------------------------------------------------------

rec.2016 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2016.zip",skip=3)

rec.2016 <-  mutate(rec.2016,nroConta=str_sub(Conta,1,16), ano=2016)

rec.2016.iptu <-  filter(rec.2016, nroConta %in% buscaCodSiconfi)

rec.2016.iptu <- spread(rec.2016.iptu, Coluna, Valor)

rec.2016.iptu <- rec.2016.iptu[,-c(8:9)]

colnames(rec.2016.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

# rec.2016.iptu <- mutate(rec.2016.iptu,
#                         valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2016.iptu <- rec.2016.iptu %>% select(-conta,-deducoes.receita)
rec.2016.iptu <- rec.2016.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2016.iptu <- rec.2016.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2016.iptu <- spread(rec.2016.iptu, nroConta, receitas.realizadas)

colnames(rec.2016.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")
rm(rec.2016)

#2.5. siconfi 2017 ------------------------------------------------------------

rec.2017 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2017.zip",skip=3)

rec.2017 <-  mutate(rec.2017,nroConta=str_sub(Conta,1,16), ano=2017)

rec.2017.iptu <-  filter(rec.2017, nroConta %in% buscaCodSiconfi)

rec.2017.iptu <- spread(rec.2017.iptu, Coluna, Valor)

rec.2017.iptu <- rec.2017.iptu[,-c(8:9)]

colnames(rec.2017.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2017.iptu <- rec.2017.iptu %>% select(-conta,-deducoes.receita)
rec.2017.iptu <- rec.2017.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2017.iptu <- rec.2017.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2017.iptu <- spread(rec.2017.iptu, nroConta, receitas.realizadas)

colnames(rec.2017.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")

rm(rec.2017)

#2.6. rbind iptu 2013 a 2017 --------------------------------------------------

# dados de 2013 a 2017 da receita de iptu dos municipios brasileiros; fonte: SICONFI https://siconfi.tesouro.gov.br/siconfi/index.jsf

rec.2013_2017 <- rbind(rec.2013.iptu,rec.2014.iptu,rec.2015.iptu,rec.2016.iptu,rec.2017.iptu)

rm(rec.2013.iptu,rec.2014.iptu,rec.2015.iptu,rec.2016.iptu,rec.2017.iptu)

str(rec.2013_2017)
rec.2013_2017 %>% select(pop:transfConv) %>% summary()
rec.2013_2017 %>% select(iptu) %>% summary()

  #2.7 analise dos missing datas da base de receitas
  # install.packages("mice")
  library(mice)
  # funcao para calcular a proporção de NAs em uma variavel
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
  rec_select <- rec.2013_2017 %>% select(pop:transfConv)
  apply(rec_select,2,pMiss)
  
  rec_select_miss <- rec_select %>% select(iptu,transfCorr,transfIntergov)
  
  # investigar o padrao de missing datas
  md.pattern(rec_select_miss)
  
  # install.packages("VIM")
  library(VIM)
  library(sp)
  aggr_plot <- VIM::aggr(rec_select_miss, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(rec_select_miss), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  tempData <- mice(rec_select_miss,m=5,maxit=50,meth='pmm',seed=500)

#3. Igpdi e Receita em termos reais -----------------------------------------

#3.1 Extracao do 	IGP-DI - geral - centrado - fim periodo - indice (ago. 1994 = 100)
# do site IPEADATA (http://www.ipeadata.gov.br/Default.aspx)
# Para fins de transformar valores nominais em reais
igpdi <- series_ipeadata(33574, periodicity = c("Y"))

ind.igpdi <- data.frame(igpdi)
colnames(ind.igpdi) <- c("ano","valor")
ind.igpdi$ano <- year(ind.igpdi$ano)

# criacao de variavel "ind" com o valor do indice de 2017, valor presente
# e o indice do respectivo ano. assim todo valor multiplicado por "ind"
# sera trazido a valor presente (ano de 2017)
ind.igpdi$ind <- (tail(ind.igpdi$valor,1)/ind.igpdi$valor)
rec.2013_2017 <- left_join(rec.2013_2017, ind.igpdi,by=c("ano"))

rm(igpdi)

#3.2 receita em termos reais IGP-DI
rec.2013_2017_real <- rec.2013_2017
rec.2013_2017_real[6:15] <- rec.2013_2017[6:15]*rec.2013_2017$ind

  # filtro com informacoes sobre o Municipio cod ibge 4301701 (Barao de Cotegipe) para teste
  rec.2013_2017_real %>% filter(codMun7=="4301701") %>% group_by(ano) %>% 
    filter(itbi>iptu) %>% summarise(itbi_maior=n())
  
  # tabela sobre % de municipios com o itbi ou o issqn maior por ano
  rec.2013_2017_real %>% na.omit(iptu) %>% group_by(ano) %>% 
    filter(itbi>iptu) %>% summarise(itbi_maior=n())
  
  rec.2013_2017_real %>% na.omit(iptu) %>% group_by(ano) %>% 
    filter(issqn>iptu) %>% summarise(issqn_maior=n())
  
  rec.2013_2017_real %>% na.omit(iptu) %>% group_by(ano) %>% 
    summarise(n_iptu=n())

  # quadro iptu 2013 a 2017 por município exemplo
  rec.2013_2017_real %>% select(codMun7,ano,iptu) %>% spread(ano, iptu)


#4. criacao de variaveis sobre a base de receitas ----------------------------------------------------

  
#4.1 criacao da variavel iptu per capita
rec.2013_2017_real$iptuPc <- rec.2013_2017_real$iptu/rec.2013_2017_real$pop

#4.2 criacao das variaveis itbi per capita, issqn per capita, transferencias intergovernamenais per capita
rec.2013_2017_real <- mutate(rec.2013_2017_real,
                             itbiPc= itbi/pop, issqnPc= issqn/pop,
                             transfIntergovPc=transfIntergov/pop)

#4.3 criacao da variavel % IPTU sobre Receita Corrente
rec.2013_2017_real$iptuRecCorr <- rec.2013_2017_real$iptu/rec.2013_2017_real$recCorr

#4.4 criacao da variavel	% IPTU sobre Receita Própria (“Impostos”) 
rec.2013_2017_real$iptuRecProp <- rec.2013_2017_real$iptu/rec.2013_2017_real$impostos

#4.5 criacao da variavel % Transf Intergov sobre Receita Corrente
rec.2013_2017_real$transfIntergovRecCorr <- rec.2013_2017_real$transfIntergov/rec.2013_2017_real$recCorr

#4.6 criacao da variavel logaritmo natural iptu per capita
rec.2013_2017_real$lnIptuPc <- log(rec.2013_2017_real$iptuPc)

#4.7 criacao da variavel Código do Estado (2 dígitos)
rec.2013_2017_real$codUF <- str_sub(rec.2013_2017_real$codMun7,1,2)

rec.2013_2017_real$codUF <- factor(rec.2013_2017_real$codUF,
                                   levels = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                                   labels = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))

#4.8 criacao da variavel Código da Região Geográfica (1 dígito)
rec.2013_2017_real$codReg <- str_sub(rec.2013_2017_real$codMun7,1,1)

rec.2013_2017_real$codReg <- factor(rec.2013_2017_real$codReg,
                                   levels = c(1,2,3,4,5),
                                   labels = c('Norte','Nordeste','Sudeste','Sul','Centro-Oeste'))


##### 5.tabela consolidada Receitas STN/Siconfi + criacao variaveis #####

#5.1. criacao das variaveis dummies dos efeitos analise de esclarecimento, instrucao e julgamento
# fonte: dados TCE/RS sobre processos ligados a IPTU durante o período 2013 a 2016, solicitação via LAI
# rec.2013_2017_real$efeitoEsclar <- tcers_efeitos$Esclar[match(interaction(rec.2013_2017_real$ano, rec.2013_2017_real$codMun7), interaction(tcers_efeitos$Esclar, tcers_efeitos$codMun7))]


rec.2013_2017_real$efeitoEsclar <- tcers_efeitos$Esclar[match(rec.2013_2017_real$codMun7,tcers_efeitos$codMun7)]
rec.2013_2017_real$efeitoInstru <- tcers_efeitos$Instruc[match(rec.2013_2017_real$codMun7,tcers_efeitos$codMun7)]
rec.2013_2017_real$efeitoJulga <- tcers_efeitos$Julga[match(rec.2013_2017_real$codMun7,tcers_efeitos$codMun7)]

#5.2. criacao variavel dummy tratamento valendo para os anos subsequentes
rec.2013_2017_real$efeitoEsclar_t <- ifelse(rec.2013_2017_real$ano>=rec.2013_2017_real$efeitoEsclar,1,0)
rec.2013_2017_real$efeitoInstru_t <- ifelse(rec.2013_2017_real$ano>=rec.2013_2017_real$efeitoInstru,1,0)
rec.2013_2017_real$efeitoJulga_t <- ifelse(rec.2013_2017_real$ano>=rec.2013_2017_real$efeitoJulga,1,0)

rec.2013_2017_real$efeitoEsclar_t <- replace_na(rec.2013_2017_real$efeitoEsclar_t,0)
rec.2013_2017_real$efeitoInstru_t <- replace_na(rec.2013_2017_real$efeitoInstru_t,0)
rec.2013_2017_real$efeitoJulga_t <- replace_na(rec.2013_2017_real$efeitoJulga_t,0)

#5.3. criacao variavel dummy tratamento valendo para um ano especifico
rec.2013_2017_real$efeitoEsclar_m <- ifelse(rec.2013_2017_real$ano==rec.2013_2017_real$efeitoEsclar,1,0)
rec.2013_2017_real$efeitoInstru_m <- ifelse(rec.2013_2017_real$ano==rec.2013_2017_real$efeitoInstru,1,0)
rec.2013_2017_real$efeitoJulga_m <- ifelse(rec.2013_2017_real$ano==rec.2013_2017_real$efeitoJulga,1,0)

rec.2013_2017_real$efeitoEsclar_m <- replace_na(rec.2013_2017_real$efeitoEsclar_m,0)
rec.2013_2017_real$efeitoInstru_m <- replace_na(rec.2013_2017_real$efeitoInstru_m,0)
rec.2013_2017_real$efeitoJulga_m <- replace_na(rec.2013_2017_real$efeitoJulga_m,0)

rec.2013_2017_real$efeitoEsclar_m <- as.factor(rec.2013_2017_real$efeitoEsclar_m)
rec.2013_2017_real$efeitoInstru_m <- as.factor(rec.2013_2017_real$efeitoInstru_m)
rec.2013_2017_real$efeitoJulga_m <- as.factor(rec.2013_2017_real$efeitoJulga_m)

rec.2013_2017_real$efeitoEsclar_t <- as.factor(rec.2013_2017_real$efeitoEsclar_t)
rec.2013_2017_real$efeitoInstru_t <- as.factor(rec.2013_2017_real$efeitoInstru_t)
rec.2013_2017_real$efeitoJulga_t <- as.factor(rec.2013_2017_real$efeitoJulga_t)

  # exemplo para teste dos resultados da elaboracao das variaveis
  rec.2013_2017_real %>% filter(codMun7=="4300802") %>% select(nomeMun,codMun7,ano,efeitoEsclar)
  
  rec.2013_2017_real %>%  filter(codUF=="RS"&codMun7=="4300802") %>% 
    select(nomeMun,codMun7,ano,efeitoEsclar,
           efeitoEsclar_t,efeitoEsclar_m) %>% arrange(codMun7,ano)
  
# tabela de frequencia UF x Ano (2013 a 2017)
table(rec.2013_2017_real$codUF,rec.2013_2017_real$ano)

# base de dados municipios litoral e turisticos
# fonte: https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_litor%C3%A2neos_do_Brasil
# e Anuário Estatístico do Brasil 2011 - IBGE (https://biblioteca.ibge.gov.br/visualizacao/monografias/GEBIS%20-%20RJ/AEB/AEB2011.pdf)
munic_litoral2008 <- read_excel("lista_munic_litoral.xlsx")
str(munic_litoral2008)
head(munic_litoral2008)
#5.4. criacao de dummy de municipio litoraneo
rec.2013_2017_real$municLitoral <- match(rec.2013_2017_real$codMun7, munic_litoral2008$codMun7)
rec.2013_2017_real$municLitoral <- ifelse(!is.na(rec.2013_2017_real$municLitoral),1,0)
rec.2013_2017_real$municLitoral <- as.factor(rec.2013_2017_real$municLitoral)
rec.2013_2017_real %>% group_by(ano) %>% filter(municLitoral==1) %>% count(municLitoral)

# base de dados sobre a categoria de cidade turistica
# fonte: Categorização dos Municípios Turísticos - 2017
#   (http://dados.gov.br/dataset/categorizacao/resource/ac85029a-9c07-4a95-a5c6-7f842a7ddf30)
munic_turist <- read_csv2("2017-categorizacao-v2.csv")
str(munic_turist)

colnames(munic_turist) <- c("reg","uf","regiaoTur","nomeMun","codMun7",
                            "qtdEmprHosp","qtdEstabHosp","demInter",
                            "demDom","clusterTur2017")

munic_turist <- rec.2013_2017_real %>% filter(ano==2017) %>% select(codMun7,pop) %>% 
  right_join(munic_turist,rec.2013_2017_real,by="codMun7")

munic_turist <- mutate(munic_turist,emprHospPc = qtdEmprHosp/pop,demTurPc = (demInter+demDom)/pop) 

#5.5. criacao dummy municipio capital
# Fonte: https://www.ibge.gov.br/geociencias-novoportal/organizacao-do-territorio/estrutura-territorial/18354-regioes-metropolitanas-aglomeracoes-urbanas-e-regioes-integradas-de-desenvolvimento.html?=&t=downloads
capitais <- read_xlsx("capitais.xlsx")
rec.2013_2017_real$municCapital <- match(rec.2013_2017_real$codMun7, capitais$codMun7)
rec.2013_2017_real$municCapital <- ifelse(!is.na(rec.2013_2017_real$municCapital),1,0)
rec.2013_2017_real$municCapital <- as.factor(rec.2013_2017_real$municCapital)
str(rec.2013_2017_real)
#5.6. criacao dummy municipio regiao metropolina (RM/IBGE)
# Fonte: https://www.ibge.gov.br/geociencias-novoportal/organizacao-do-territorio/estrutura-territorial/18354-regioes-metropolitanas-aglomeracoes-urbanas-e-regioes-integradas-de-desenvolvimento.html?=&t=downloads
composicao_rms <- read_xlsx("composicao_RMs.xlsx")
composicao_rms <- composicao_rms %>% filter(!composicao_rms$COD_MUN %in% (capitais$codMun7)) 
rms_selecionados <- read_xlsx("RMs selecionados.xlsx")
composicao_rms <- composicao_rms %>% filter(composicao_rms$NOME_RM %in% (rms_selecionados$`RMs Selecionados`)) 

rec.2013_2017_real$municMetrop <- match(rec.2013_2017_real$codMun7, composicao_rms$COD_MUN)
rec.2013_2017_real$municMetrop <- ifelse(!is.na(rec.2013_2017_real$municMetrop),1,0)
rec.2013_2017_real$municMetrop <- as.factor(rec.2013_2017_real$municMetrop)
rm(composicao_rms,rms_selecionados,capitais)

#5.7. criacao de variavel classe porte populacional municipios
rec.2013_2017_real <- rec.2013_2017_real %>% mutate(popClass=cut(pop,breaks = c(-Inf,15000,100000,350000,800000,2000000,6000000,+Inf),
                                           labels = c("Até 15 mil","15 a 100 mil","100 a 350 mil","350 a 800 mil","800 mil a 2 mi","2 a 6 mi","Acima de 6 mi")))

#6. estatistica univariada populacao e analise cluster --------------------------------------------------
str(rec.2013_2017_real)
pop2017 <- rec.2013_2017_real %>% filter(ano==2017) %>% select(codMun7,ufSigla,pop)
summary(pop2017$pop)
boxplot(pop2017$pop)
hist(pop2017$pop)
pop2017$pop_scale <- scale(pop2017$pop)
hist(pop2017$pop_scale)

# fonte:https://www.statmethods.net/advstats/cluster.html
# Determine number of clusters

num_clusters <- function(dados){
  wss <- (nrow(dados)-1)*sum(apply(dados,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(dados,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
}
num_clusters(pop2017$pop_scale)

# K-Means Cluster Analysis
fit <- kmeans(pop2017$pop_scale, 7)
fit$centers
fit$centers * attr(pop2017$pop_scale, 'scaled:scale') + attr(pop2017$pop_scale, 'scaled:center')

# get cluster means
aggregate(pop2017$pop,by=list(fit$cluster),FUN=mean)
# append cluster assignment
pop2017$cluster <-  fit$cluster

pop2017 <- pop2017 %>% mutate(popClass=cut(pop,breaks = c(-Inf,15000,100000,350000,800000,2000000,6000000,+Inf),
                                           labels = c("Até 15 mil","15 a 100 mil","100 a 350 mil","350 a 800 mil","800 mil a 2 mi","2 a 6 mi","Acima de 6 mi")))

pop2017 %>% group_by(popClass) %>% summarise(n=n(),media=mean(pop))

# criacao da variavel popClass, grupo de municipios conforme tamanho populacional
rec.2013_2017_real <- rec.2013_2017_real %>% mutate(popClass=cut(pop,breaks = c(-Inf,15000,100000,350000,800000,2000000,6000000,+Inf),
                                           labels = c("Até 15 mil","15 a 100 mil","100 a 350 mil","350 a 800 mil","800 mil a 2 mi","2 a 6 mi","Acima de 6 mi")))

# criacao de variavel indice de vulnerabilidade social e ivs - infraestrutura
ivs_infraestrutura <- read_excel("ivs_infraestrutura.xlsx")
ivs_infraestrutura <- ivs_infraestrutura[,c(3,4,6:8)]
str(ivs_infraestrutura)
colnames(ivs_infraestrutura) <- c("codMun7","nomeMun","ano","ivs","ivsInfra")
ivs_infraestrutura <- ivs_infraestrutura %>% filter(ano==2010)

#7. estatistica descritiva Receita STN/Siconfi -------------------------------------------
# fonte: https://www.statmethods.net/stats/descriptives.html
# https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html

# filtro de municipios que nao tem o historico de informacoes FINBRA completo 2013 a 2017
rec.2013_2017_real %>% group_by(codMun7) %>% summarise(n_IPTU=length(iptu)) %>% filter(n_IPTU<5)

rec.2013_2017_real %>% group_by(codMun7) %>% summarise(n_IPTU=length(iptu)) %>% filter(n_IPTU==5)

# install.packages("psych")
# install.packages("reporttools")
library(psych)
library(Hmisc)
library(reporttools)
rec.2013_2017_real %>% filter(ano==2017) %>% Hmisc::describe()

psych::describe.by(rec.2013_2017_real, rec.2013_2017_real$ano)

Hmisc::describe(rec.2013_2017_real)

# install.packages("stargazer")
library(stargazer)

#7.1 estatística descritiva formatada variaveis selecionadas dados 2017
### inserida essa tabela no trabalho de conclusão ###
rec.2013_2017_real %>% filter(ano==2017) %>% 
      select(pop,iptuPc,itbiPc,issqnPc,transfIntergovRecCorr) %>% 
                  data.frame() %>%  
  stargazer(type = "html", title="Estatística descritiva - Variáveis selecionadas 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,notes="Fonte: SICONFI/STN,2017.",
            out="table1.doc",summary.stat = c("n","mean","min","max","sd"))

# correlacao formatada variaveis receitas municipais e populacao exemplo para analise
rec.2013_2017_real %>% select(pop,iptuPc,itbiPc,
                              issqnPc,
                              transfIntergovRecCorr) %>% 
  data.frame() %>% na.omit() %>% cor() %>% 
  stargazer(type="text")


#8. Dados IBGE SIDRA ------------------------------------------------

# Nro domicilios Ibge SIDRA
# uso do pacote sidrar
# para extrair os dados do IBGE disponiveis no SIDRA: https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2010/universo-caracteristicas-da-populacao-e-dos-domicilios

# carregamento dos pacotes SIDRA
# install.packages("sidrar")
# install.packages("dynlm")
library(tidyverse)
library(sidrar)
library(ggplot2)
library(scales)
library(dynlm)

# tabela 1134 SIDRA/IBGE numero de domicilios por municipio Censo 2010
# tabela 1134 SIDRA/IBGE n? de domicilios por munic?pio Censo 2010
# https://sidra.ibge.gov.br/tabela/1134#resultado
tabela.dom.munic <- get_sidra(api="/t/1134/n6/all/v/allxp/p/all/c1/0/c460/0/c317/0/c11561/0/c12237/0/c11562/0")
tabela.dom.munic <- tabela.dom.munic[,c(1:2,6,21)]
colnames(tabela.dom.munic) <- c("codMun7","nomeMun","ano","domPerm")

# situacao domicilio urbano
tabela.dom.munic2 <- get_sidra(api="/t/1134/n6/all/v/allxp/p/all/c1/1/c460/0/c317/0/c11561/0/c12237/0/c11562/0")
tabela.dom.munic2 <- tabela.dom.munic2[,c(1:2,6,21)]
colnames(tabela.dom.munic2) <- c("codMun7","nomeMun","ano","domPermUrb")

ibge.dom <- left_join(tabela.dom.munic,tabela.dom.munic2,by=c("codMun7","nomeMun","ano"))

rm(tabela.dom.munic,tabela.dom.munic2)

# tabela 1310 SIDRA/IBGE numero de domicilios recenseados por municipio Censo 2010
# tabela 1310 SIDRA/IBGE n? de domicilios recenseados por munic?pio Censo 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/1310
tabela.dom.recens.munic <- get_sidra(api="/t/1310/n6/all/v/allxp/p/last%201/c3/0/c1/0")
tabela.dom.recens.munic <- tabela.dom.recens.munic[,c(1:2,6,13)]
colnames(tabela.dom.recens.munic) <- c("codMun7","nomeMun","ano","domRecens")

ibge.dom <- left_join(ibge.dom,tabela.dom.recens.munic,by=c("codMun7","nomeMun","ano"))
rm(tabela.dom.recens.munic)


# situacao domicilio urbano
tabela.dom.recens.munic2 <- get_sidra(api="/t/1310/n6/all/v/allxp/p/last%201/c3/0/c1/1")
tabela.dom.recens.munic2 <- tabela.dom.recens.munic2[,c(1:2,6,13)]
colnames(tabela.dom.recens.munic2) <- c("codMun7","nomeMun","ano","domRecensUrb")

ibge.dom <- left_join(ibge.dom,tabela.dom.recens.munic2,by=c("codMun7","nomeMun","ano"))
rm(tabela.dom.recens.munic2)

# tabela 3033 SIDRA/IBGE numero de domicilios particulares ocupados por municipio Censo 2010
# tabela 3033 SIDRA/IBGE n? de domicilios particulares ocupados por munic?pio Censo 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/3033#resultado
tabela.dom.ocup.munic <- get_sidra(api="/t/3033/n6/all/v/618/p/all/c11277/9806")
tabela.dom.ocup.munic <- tabela.dom.ocup.munic[,c(1:2,6,11)]
colnames(tabela.dom.ocup.munic) <- c("codMun7","nomeMun","ano","domPartOcup")

ibge.dom <- left_join(ibge.dom,tabela.dom.ocup.munic,by=c("codMun7","nomeMun","ano"))
rm(tabela.dom.ocup.munic)

# tabela 185 SIDRA/IBGE Domic?lios particulares permanentes por situa??o e n?mero de moradores Censo 2000 e 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/185

tabela.dom.perm <- get_sidra(api="/t/185/n6/all/v/allxp/p/all/c1/0/c68/0")
tabela.dom.perm <- tabela.dom.perm[,c(1:2,6,13)]
colnames(tabela.dom.perm) <- c("codMun7","nomeMun","ano","domPartPerm")

ibge.dom <- left_join(ibge.dom,tabela.dom.perm,by=c("codMun7","nomeMun","ano"))
rm(tabela.dom.perm)


##### Sidra IBGE Domicilios
tabela.dom.perm.rural <- get_sidra(api="/t/185/n6/all/v/allxp/p/all/c1/2/c68/0")
tabela.dom.perm.rural <- tabela.dom.perm.rural[,c(1:2,6,13)]
colnames(tabela.dom.perm.rural) <- c("codMun7","nomeMun","ano","domPartPermRural")

ibge.dom <- left_join(ibge.dom,tabela.dom.perm.rural,by=c("codMun7","nomeMun","ano"))

# Tabela 6579 - População residente estimada
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/6579
pop.estim <- get_sidra(api="/t/6579/n6/all/v/all/p/all")
pop.estim <- pop.estim[,c(1:2,6,9)]
colnames(pop.estim) <- c("codMun7","nomeMun","ano","popEstim")


# tabela 156 SIDRA/IBGE Domicilios particulares ocupados, moradores em domic?lios particulares ocupados e m?dia de moradores em domic?lios particulares ocupados Censo 2000 e 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/156

tabela.dom.ocup <- get_sidra(api="/t/156/n6/all/v/134,2048/p/all")
tabela.dom.ocup <- tabela.dom.ocup[,c(1:3,6,9)]

# como a tabela possui duas variaveis é preciso usar a funcao spread
tabela.dom.ocup <- spread(tabela.dom.ocup, "Variável (Código)", Valor)
colnames(tabela.dom.ocup) <- c("codMun7","nomeMun","ano","pesDomOcup","domOcup")
ibge.dom <- left_join(ibge.dom,tabela.dom.ocup,by=c("codMun7","nomeMun","ano"))

# Tabela 1301 - Área e Densidade demográfica da unidade territorial
# https://sidra.ibge.gov.br/tabela/1301

tabela.dens.munic <- get_sidra(api="/t/1301/n6/all/v/all/p/all")
tabela.dens.munic <- tabela.dens.munic[,c(1:3,6,9)]
tabela.dens.munic <- spread(tabela.dens.munic, "Variável (Código)", Valor)

colnames(tabela.dens.munic) <- c("codMun7","nomeMun","ano","areaMunic","densPop")
#####8.1. tabela consolidada Censo IBGE sobre Domicílios #####
ibge.dom <- left_join(ibge.dom,tabela.dens.munic,by=c("codMun7","nomeMun","ano"))

str(ibge.dom)
# criacao da variavel de proporcao de domicilios urbanos - domUrbProp
ibge.dom <- ibge.dom %>% mutate(domUrbProp=domRecensUrb/domRecens)

ibge.dom %>% select(domRecens,domRecensUrb,domUrbProp) %>% head()

# tabela 3451 SIDRA/IBGE Moradores em domicílios particulares permanentes (Pessoas) Censo 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/3451

tabela.morad.dom <- get_sidra(api="/t/3451/n6/all/v/137/p/all/c1/allxt")
tabela.morad.dom <- tabela.morad.dom[,c(1:2,6,8,11)]
str(tabela.morad.dom)
# como a tabela possui duas situacoes de domicilio é preciso usar a funcao spread
tabela.morad.dom <- spread(tabela.morad.dom, "Situação do domicílio", Valor)
colnames(tabela.morad.dom) <- c("codMun7","nomeMun","ano","pesDomRural","pesDomUrb")
tabela.morad.dom <- tabela.morad.dom %>% mutate(pesDomUrbProp=pesDomUrb/(pesDomUrb+pesDomRural))
ibge.dom <- left_join(ibge.dom,tabela.morad.dom,by=c("codMun7","nomeMun","ano"))


# 8.2. SIDRA/IBGE PIB Municipal -------------------------------------------

# tabela 5938 SIDRA/IBGE PIB Municipal 2014
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/5938
# pib.munic.2014 <- get_sidra(api="/t/5938/n6/all/v/37/p/last%201/d/v37%200")

# tabela 5938 SIDRA/IBGE PIB Municipal 2002 a 2015
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/5938
pib.munic.all <- get_sidra(api="/t/5938/n6/all/v/37/p/all/d/v37%200")

pib.munic.all <- pib.munic.all[,c(1:2,6,9)]
colnames(pib.munic.all) <- c("codMun7","nomeMun","ano","pibMun")

# incluir participacoes do VAB por setor (agropecuaria, comercio, industria e governo)
# agropecuaria
pib.part.agri.munic.all <- get_sidra(api="/t/5938/n6/all/v/516/p/all/d/v516%202")
pib.part.agri.munic.all <- pib.part.agri.munic.all[,c(1:2,6,9)]
colnames(pib.part.agri.munic.all) <- c("codMun7","nomeMun","ano","partVabAgriMun")

pib.ibge <- left_join(pib.munic.all,pib.part.agri.munic.all,by=c("codMun7","nomeMun","ano"))

# industria
pib.part.ind.munic.all <- get_sidra(api="/t/5938/n6/all/v/520/p/all/d/v520%202")
pib.part.ind.munic.all <- pib.part.ind.munic.all[,c(1:2,6,9)]
colnames(pib.part.ind.munic.all) <- c("codMun7","nomeMun","ano","partVabIndMun")

pib.ibge <- left_join(pib.ibge,pib.part.ind.munic.all,by=c("codMun7","nomeMun","ano"))

# servicos
pib.part.serv.munic.all <- get_sidra(api="/t/5938/n6/all/v/6574/p/all/d/v6574%202")
pib.part.serv.munic.all <- pib.part.serv.munic.all[,c(1:2,6,9)]
colnames(pib.part.serv.munic.all) <- c("codMun7","nomeMun","ano","partVabServMun")

pib.ibge <- left_join(pib.ibge,pib.part.serv.munic.all,by=c("codMun7","nomeMun","ano"))

# adm publica
pib.part.admpub.munic.all <- get_sidra(api="/t/5938/n6/all/v/528/p/all/d/v528%202")

pib.part.admpub.munic.all <- pib.part.admpub.munic.all[,c(1:2,6,9)]
colnames(pib.part.admpub.munic.all) <- c("codMun7","nomeMun","ano","partVabGovMun")
pib.ibge <- left_join(pib.ibge,pib.part.admpub.munic.all,by=c("codMun7","nomeMun","ano"))

#####8.3. tabela consolidada IBGE sobre PIB Municipal #####

pib.ibge$ano <- as.numeric(pib.ibge$ano)
pib.ibge$codMun7 <- as.integer(pib.ibge$codMun7)

#8.3.1. inclusao de uma estimativa do pib 2016 e 2017 para municipios
# criterios: variacao por atividade por UFs em 2016 sobre 
# a respectiva participacao do VAB do municipio da UF;
# variacao por atividade do Brasil em 2017 sobre
# a respectiva participacao do VAB por atividade dos municipios
pib.ibge <- pib.ibge %>% mutate(codUF=str_sub(codMun7,1,2))

pib.ibge$codUF <- factor(pib.ibge$codUF,
                         levels = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                         labels = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))

pib.ibge2016 <- pib.ibge %>% filter(ano==2015)
pib.ibge2016$ano <- 2016

# inclusao da tabela de variacao do pib por atividade
# 2016 por UFs e 2017 somente Brasil
pib_ativ_ufs <- read_xlsx("pib_ativ_ufs_2016_17.xlsx")
pib_ativ_ufs$codUF <- factor(pib_ativ_ufs$codUF,
                             levels = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                             labels = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))

pib.ibge2016 <- inner_join(pib.ibge2016,pib_ativ_ufs,by=c("codUF"="codUF","ano"="ano"))
pib.ibge2016 <- pib.ibge2016 %>% 
  mutate(pibMun = (1+((partVabAgriMun*agr+ partVabIndMun*indus+
                         (partVabServMun+partVabGovMun)*serv)/10000))*pibMun)

pib.ibge2017 <- pib.ibge2016 %>% select(codMun7:codUF)
pib.ibge2017$ano <- 2017
pib.ibge2017 <- left_join(pib.ibge2017,pib_ativ_ufs,by=c("ano","codUF"))
pib.ibge2017 <- pib.ibge2017 %>% 
  mutate(pibMun = (1+((partVabAgriMun*agr+ partVabIndMun*indus+
                         (partVabServMun+partVabGovMun)*serv)/10000))*pibMun)
pib.ibge2016 <- pib.ibge2016 %>% select(codMun7:codUF)
pib.ibge2017 <- pib.ibge2017 %>% select(codMun7:codUF)

pib.ibge.2013_2017 <- rbind(pib.ibge,pib.ibge2016,pib.ibge2017)
pib.ibge.2013_2017 <- pib.ibge.2013_2017 %>% arrange(codMun7,ano)
pib.ibge.2013_2017 <- pib.ibge.2013_2017 %>% filter(ano>=2013)


#8.3.2. pib a valor presente
pib.ibge.2013_2017 <- left_join(pib.ibge.2013_2017, ind.igpdi,by=c("ano"))
pib.ibge.2013_2017$pibMunReal <- pib.ibge.2013_2017$pibMun*pib.ibge.2013_2017$ind

pib.ibge.2013_2017 <- rec.2013_2017_real %>% select(ano,pop,codMun7) %>%
  left_join(pib.ibge.2013_2017, rec.2013_2017_real,by=c("ano","codMun7"))

#8.3.3. criacao da variavel pib per capita em termos reais pibMunRealPc
pib.ibge.2013_2017 <- mutate(pib.ibge.2013_2017,pibMunRealPc= pibMunReal/pop)
str(pib.ibge.2013_2017)
str(rec.2013_2017_real)

#8.4. estatistica descritiva univariada Ibge/Sidra domicilios ----------------------------

dim(ibge.dom)

sapply(ibge.dom, class)

str(ibge.dom)

summary(ibge.dom)

library(purrr)
ibge.dom %>% split(.$ano) %>% map(summary)

psych::describe(ibge.dom)

psych::describeBy(ibge.dom, ibge.dom$ano)

Hmisc::describe(ibge.dom)

# install.packages("pastecs")
library(pastecs)
stat.desc(ibge.dom)

hist(ibge.dom$domPartPerm)
plot(density(ibge.dom$domPartPerm,na.rm = T))

boxplot(log(ibge.dom$domPartPerm))

#9. Atlas PNUD --------------------------------------------------------------

# carregar dados Atlas Municipal baixados fonte: PNUD http://www.atlasbrasil.org.br/2013/pt/download/ 
atlas.pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx","MUN 91-00-10")
atlas.pnud.dicvar <- read_excel("atlas2013_dadosbrutos_pt.xlsx","Siglas")

head(atlas.pnud)

# dados 2000 e 2010
atlas.pnud <- atlas.pnud %>% filter(ANO >= 2000)

colnames(atlas.pnud)[1:5] <- c("ano","ufSigla","codMun6","codMun7","nomeMun")

atlas.pnud %>% filter(ano==2010) %>% group_by(ufSigla) %>% 
  summarise(
    Esperancaaonascer = mean(ESPVIDA)
  )

atlas.pnud.2010 <- atlas.pnud %>% 
  filter(ano==2010) %>% select(ano:nomeMun,RAZDEP,GINI,PIND,PMPOB,PPOB,T_AGUA:AGUA_ESGOTO,
                               R1040,RDPC,RDPCT,THEIL,pesoRUR:PIA,IDHM)

pnud <- atlas.pnud %>% 
  filter(ano==2010) %>% select(ano:nomeMun,RDPC,RDPCT,pesotot)
str(pnud)
head(pnud)
# analise da questao da apropriacao da renda pelo municipio (renda x pib)
pib10 <- pib.ibge %>% filter(ano==2010) %>% select(codMun7:pibMun)
pib.ibge %>% filter(ano==2015) %>% select(codMun7:pibMun) %>% arrange(desc(pibMun))
pnud <- left_join(pnud,pib10,by="codMun7")

pnud <- pnud %>% select(-nomeMun.x,-ano.y,-codMun6)

pnud <- pnud %>% mutate(renda= RDPC*12*pesotot/1000)

pnud <- pnud %>% mutate(aproRenda = renda/pibMun, pibMunPc= pibMun/pesotot)

pnud %>% select(-ano.x) %>% arrange(desc(aproRenda)) %>% filter(aproRenda>=1)

pib_renda_rs<- pnud %>% filter(ufSigla==43) %>% select(-ano.x) %>% arrange(desc(pibMunPc))

library(xlsx)
write.xlsx2(pib_renda_rs,"pib_renda_rs.xlsx")

#10. Munic IBGE 2004 ---------------------------------------------------------
# install.packages("tidyverse")
library(tidyverse)
library(devtools)

# devtools::install_version("readxl", version = "1.0.0")
# carregar dados Munic 2004 baixados fonte IBGE:https://ww2.ibge.gov.br/home/estatistica/economia/perfilmunic/2004/default.shtm
unzip(zipfile="base_MUNIC_2004.zip", files = "Base 2004.xls", exdir=".")
# converte manualmente arquivo xls para xlsx
# pois o pacote readxl passou a ter problema com a nova versao
munic.2004.iptu <- read_excel("Base 2004.xlsx","IPTU")
munic.2004.varext <- read_excel("Base 2004.xlsx","Variáveis externas")

# variaveis importantes A1 e A163 para a aba variáveis externas
# obter variável "Ano de instalação do Munic??pio"

munic.2004.varext <- munic.2004.varext %>% select(A1,A163)

munic.2004 <- left_join(munic.2004.iptu,munic.2004.varext,by="A1")

# renomear names das variaveis coluna

colnames(munic.2004) <- c("codMun6","cadImobExist","cadImobInfo","unPredTerrCad",
                          "unPredCad","unTerrCad","anoAtualiPlanta",
                          "anoAtualiCad","anoInstalMunic")

munic.2004$ano <- 2004

munic.2004 <- left_join(munic.2004,ibgeTse,by="codMun6")


munic.2004 %>% filter(codMun7==1100106) %>% select(codMun7,nomeMun,anoAtualiPlanta,anoAtualiCad,anoInstalMunic)
munic.2015 %>% filter(codMun7==1100106) %>% select(codMun7,nomeMun,anoAtualiPlanta,anoAtualiCad)

rm(munic.2004.iptu,munic.2004.varext)



#11. Munic IBGE 2015 ---------------------------------------------------------

# carregar dados Munic 2015 baixados fonte IBGE:https://ww2.ibge.gov.br/home/estatistica/economia/perfilmunic/2015/default.shtm
unzip(zipfile="Base_MUNIC_2015_xls.zip", files = "Base_MUNIC_2015.xls", exdir=".")
# converte manualmente arquivo xls para xlsx
# pois o pacote readxl passou a ter problema com a nova versao

munic.2015 <- read_excel("Base_MUNIC_2015.xlsx","Recursos para gestão")

# variaveis importantes A61 a A68 para a gest?o de IPTU

munic.2015 <- munic.2015 %>% select(A1:A68)

# renomear names das variaveis coluna

colnames(munic.2015) <- c("codMun7","ufSigla","codMun3","nomeMun",
                          "cadImobExist","cadImobInfo","anoAtualiCad",
                          "cobraIptu","anoLeiIptu","pgvExist",
                          "pgvInfo","anoAtualiPlanta")

munic.2015$ano <- 2015

munic.2015.dicvar <- read_excel("Base_MUNIC_2015.xlsx","Dicionário")
munic.2015.dicvar <- munic.2015.dicvar %>% filter(!is.na(variável))

# descricao das variaveis selecionadas A1 a A68
data.frame(munic.2015.dicvar[69:76,c(1,2,6)])



#12. Dados Eleitorais TSE (Nao utilizados no estudo!) ----------------------------------------------------
# dados baixados fonte: TSE http://www.tse.jus.br/eleitor-e-eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

# Fonte consulta: http://fmeireles.com/blog/rstats/electionsbr-um-pacote-para-baixar-dados-eleitorais-do-tse
# http://fmeireles.com/blog/rstats/electionsBR-versao-0.3.0
# http://fmeireles.com/blog/rstats/electionsbr-analisando-a-apuracao-das-eleicoes-para-a-camara-dos-deputados
# install.packages("electionsBR")


# Carrega o pacote
library(electionsBR)

# Carrega dados do TSE tabulados em outra oportunidade
load("dadosTSE.RData")
# Baixa os dados sobre candidaturas federais
# dados <- candidate_fed(year = 2002)

# Cria um vetor armazenando os anos
anos <- seq(2000, 2016, by = 4)

# votos eleicoes municipais desagregados de 2016
# votos eleicoes municipais desagregados de 2016
mun <- vote_mun_zone_local(2016)

# Baixa os dados com lapply e vote_mun_zone_local Votos Elei??es Municipais
votaMunicAll <- lapply(anos, vote_mun_zone_local)

# Une as bases (se foram baixadas separadamente, comente a linha abaixo)
votaMunicAll <- bind_rows(mutate_all(votaMunicAll,as.character))

library(plyr)
votaMunicAll <- rbind.fill(votaMunicAll)

# criar variavel eleito, pois ha diferenca de codigo entre os dados antes de 2008 e depois
# filtro CODIGO_SIT_CANDIDATO = 2 (DEFERIDO), 4 (SUB JUDICE), 16 (DEFERIDO COM RECURSO)
# filtro CODIGO_SIT_CAND_TOT = 1 (ELEITO), 2 (ELEITO POR QP) - ap?s 2012, 3 (ELEITO POR M?DIA) - ap?s 2012
# 5 (ELEITO POR M?DIA) - antes 2008

votaMunicAll <- votaMunicAll %>% mutate(eleito= ifelse((ANO_ELEICAO<=2008 & (CODIGO_SIT_CANDIDATO %in% c(2,4,16)) & CODIGO_SIT_CAND_TOT==1)|(ANO_ELEICAO>2008 & (CODIGO_SIT_CANDIDATO %in% c(2,4,16)) & (CODIGO_SIT_CAND_TOT %in% 1:3)),1,0))

head(votaMunicAll)

# cria variavel de competicao eleitoral pela diferenca de votos
# entre o primeiro e segundo colocados
compEleDifVots <- votaMunicAll %>% group_by(SIGLA_UE,ANO_ELEICAO) %>% summarise(primeiro = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[1]])), segundo = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[2]])),total = sum(as.numeric(TOTAL_VOTOS)),competEleicaoDif =  round((primeiro-segundo)/total*100,2))
# detalhes de apuracao eleicoes municipais

compEleDifVots <- votaMunicAll %>% group_by(SIGLA_UE,ANO_ELEICAO) %>% summarise(primeiro = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[1]])), segundo = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[2]])),total = sum(as.numeric(TOTAL_VOTOS)),competEleicaoDif =  round((primeiro-segundo)/total*100,2))

# detalhes de apura??o elei?oes municipais
# detalhesMun <- details_mun_zone_local(2016)
# head(detalhesMun)

# dados sobre candidaturas individuais municipais
candLocal2012 <- candidate_local(2012,uf="RS")
candLocal2000 <- candidate_local(2000)
candLocal2000rs <- candidate_local(2000,uf="RS")

# Baixa os dados com lapply e candidate_local Votos Elei??es Municipais
candLocalall <- lapply(anos, candidate_local)

# Une as bases (se foram baixadas separadamente, comente a linha abaixo)
candLocalall <- bind_rows(candLocalall)
head(candLocalall)

# mun.portoalegre <- candLocalall %>% filter(DESCRICAO_UE=='PORTO ALEGRE' & CODIGO_CARGO==11 & COD_SIT_TOT_TURNO==1)
# write.csv2(mun.portoalegre,"portoalegre.csv")

# select(mun.portoalegre,SIGLA_UE,CPF_CANDIDATO,ANO_ELEICAO)

# for (year in c(2010,2011,2012,2013,2014,2015)){
#   print(paste("The year is", year))
# }
# 
# by_day <- group_by(munRs, year, month, day)
# summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# inclusao da variavel eleito para corrigir a 
# difenca de enquadramentos entre os dados antes de 2008
# e depois de 2008
# filtro COD_SITUACAO_CANDIDATURA = 2 (DEFERIDO), 4 (SUB JUDICE), 16 (DEFERIDO COM RECURSO)
# filtro CODIGO_SIT_CAND_TOT = 1 (ELEITO), 2 (ELEITO POR QP) - ap?s 2012, 3 (ELEITO POR M?DIA) - ap?s 2012
# 5 (ELEITO POR M?DIA) - antes 2008

candLocalall <- candLocalall %>% mutate(eleito= ifelse((ANO_ELEICAO<=2008 & (COD_SITUACAO_CANDIDATURA %in% c(2,4,16)) & (COD_SIT_TOT_TURNO==1|COD_SIT_TOT_TURNO==5))|(ANO_ELEICAO>2008 & (COD_SITUACAO_CANDIDATURA %in% c(2,4,16)) & (COD_SIT_TOT_TURNO %in% 1:3)),1,0))

# teste <- candLocalall %>% filter(SIGLA_UF=="RR")
# write.csv2(teste,"test.csv")
teste <- candLocalall %>% filter(SIGLA_UF=="RR")
write.csv2(teste,"test.csv")

# filtro dos eleitos para o cargo de prefeito (cod=11)
execEleitos <- candLocalall %>% filter(CODIGO_CARGO==11 & eleito==1)

# inclusao da variavel dummy de candidato ? prefeito reeleito
# e candidato com possibilidade de reeleicao
execEleitos <- arrange(execEleitos,SIGLA_UE,ANO_ELEICAO) %>% mutate(antes=lag(CPF_CANDIDATO),depois=lead(CPF_CANDIDATO),reeleicaoCand=ifelse(lag(CPF_CANDIDATO)==CPF_CANDIDATO,0,1),reeleitoPref=ifelse(lag(CPF_CANDIDATO)==CPF_CANDIDATO,1,0))

# correcao das variaveis na primeira linha
execEleitos[1,"reeleicaoCand"] <- 1
execEleitos[1,"reeleitoPref"] <- 0

# selecao das variaveis de interesse em relacao aos prefeitos
# eleitos
execEleitos <- execEleitos %>% select(ANO_ELEICAO,SIGLA_UF:DESCRICAO_UE,
                                      NOME_CANDIDATO,CPF_CANDIDATO,
                                      NUMERO_PARTIDO,SIGLA_PARTIDO,
                                      NOME_COLIGACAO,DATA_NASCIMENTO,
                                      CODIGO_SEXO:DESCRICAO_ESTADO_CIVIL,
                                      reeleicaoCand,reeleitoPref)
# filter(execEleitos,SIGLA_UE=="43494"& ANO_ELEICAO==2008) %>% as.data.frame()

# para testar a quantidade de prefeitos "eleitos",
# ha casos de ter sido necessaria a realizacao de 
# eleicoes complementares
# totalprefeitos <- group_by(execEleitos, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalPref = length(unique(CPF_CANDIDATO)))
# write.csv2(totalprefeitos,"prefeitos.csv")


# inclusao da variavel de competicao eleitoral a partir do numero de
# candidatos em disputa para prefeitura
nroCand <- candLocalall %>% filter(CODIGO_CARGO==11) %>% group_by(SIGLA_UE,ANO_ELEICAO) %>% summarise(competEleicaoNrCandPref = length(unique(CPF_CANDIDATO)))
nroCand <- candLocalall %>% filter(CODIGO_CARGO==11) %>% group_by(SIGLA_UE,ANO_ELEICAO) %>% summarise(competEleicaoNrCandPref = length(unique(CPF_CANDIDATO)))

# filtro dos eleitos para o cargo de vereador (cod=13)
legisEleitos <- candLocalall%>% filter(CODIGO_CARGO==13 & eleito==1)

# inclusao da variavel para calcular o numero de vereadores
# eleitos por partido
legisEleitosPart <- group_by(legisEleitos, SIGLA_UE,ANO_ELEICAO,NUMERO_PARTIDO) %>% summarise(verEleitos = length(unique(CPF_CANDIDATO)))

# filter(legis_por_partido,SIGLA_UE=="61050"& ANO_ELEICAO==2000)
# 
# write.csv2(legis_por_partido,"partido.csv")

# a partir do join entre a base de dados do executivo e do legislativo
# chega-se ? qtd total de vereadores eleitos, qdt de eleitos do mesmo
# partido do prefeito e o percentual de forma a indicar o controle do
# prefeito sobre o legislativo municipal
legis_partido <- left_join(legisEleitosPart,execEleitos,by=c("SIGLA_UE","ANO_ELEICAO"))

# filter(uniao_legis_exec,SIGLA_UE=="61050"& ANO_ELEICAO==2000)
legis_partido <- group_by(legis_partido, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalVer = sum(verEleitos), partidoPref = sum(verEleitos[NUMERO_PARTIDO.x==NUMERO_PARTIDO.y]),maiorCamaraPref = round(partidoPref/totalVer*100,2))

legis_partido <- group_by(legis_partido, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalVer = sum(verEleitos), partidoPref = sum(verEleitos[NUMERO_PARTIDO.x==NUMERO_PARTIDO.y]),maiorCamaraPref = round(partidoPref/totalVer*100,2))

# filter(uniao_por_ano_munic,SIGLA_UE=="61050")

# inclusao da variavel contar para calcular o numero de vereadores
# eleitos por coligacao
legis_coligacao <- group_by(legisEleitos, SIGLA_UE,ANO_ELEICAO,NOME_COLIGACAO) %>% summarise(verColigacao = length(unique(CPF_CANDIDATO)))
legis_coligacao <- left_join(legis_coligacao,execEleitos,by=c("SIGLA_UE","ANO_ELEICAO"))
legis_coligacao <- group_by(legis_coligacao, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalVer = sum(verColigacao), colig = sum(verColigacao[NOME_COLIGACAO.x==NOME_COLIGACAO.y]),maiorCamaraColig = round(colig/totalVer*100,2))

# mapear codigos de municipios ibge e tse
legis_coligacao <- group_by(legisEleitos, SIGLA_UE,ANO_ELEICAO,NOME_COLIGACAO) %>% summarise(verColigacao = length(unique(CPF_CANDIDATO)))
legis_coligacao <- left_join(legis_coligacao,execEleitos,by=c("SIGLA_UE","ANO_ELEICAO"))
legis_coligacao <- group_by(legis_coligacao, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalVer = sum(verColigacao), colig = sum(verColigacao[NOME_COLIGACAO.x==NOME_COLIGACAO.y]),maiorCamaraColig = round(colig/totalVer*100,2))

ibgeTse <- read.csv("ibge-tse-map.csv",encoding = "UTF-8")
colnames(ibgeTse) <- c("ufSigla","codMun7","codMunTSE","nomeMun","nomeMunTSE")
# preencher com zeros a variavel dos codigos de municipios TSE para ficar 
# com 5 digitos
library(stringr)
ibgeTse$codMunTSE <- str_pad(ibgeTse$codMunTSE, 5, pad = "0")
ibgeTse$codMun6 <- trunc(ibgeTse$codMun7/10,6)
str(ibgeTse)
# join de codigos de municipios TSE e IBGE unificar com o codigo IBGE
compEleDifVots <- left_join(compEleDifVots,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))

nroCand <- left_join(nroCand,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))

legis_partido <- left_join(legis_partido,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))

legis_coligacao <- left_join(legis_coligacao,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))

execEleitos<- left_join(execEleitos,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))
execEleitos$ANO_ELEICAO <- as.numeric(execEleitos$ANO_ELEICAO)
str(execEleitos)

execEleitos.1 <- do.call("rbind", replicate(4, execEleitos, simplify = FALSE))
execEleitos.1 <- execEleitos.1 %>% arrange(SIGLA_UE,ANO_ELEICAO)
execEleitos.1$ANO_ELEICAO <- execEleitos.1$ANO_ELEICAO + (sequence(4))
execEleitos.1 <- execEleitos.1 %>% filter(ANO_ELEICAO<=2017)

juncao <- left_join(rec.2013_2016,execEleitos.1,by=c("codMun7","ano"="ANO_ELEICAO"))
juncao <- juncao %>% arrange(codMun7,ano)

# join de codigos de municipios TSE e IBGE unificar com o codigo IBGE
legis_coligacao <- left_join(legis_coligacao,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))

# Salvar multiplos objetos da base de dados TSE
save(compEleDifVots, nroCand,legis_partido,legis_coligacao,
     execEleitos,execEleitos.1,juncao, file = "dadosTSE.RData")

#13. Dados TCE-RS ------------------------------------------------------------
library(readxl)
tcers <- read_excel("Processos-IPTU - versão final.xlsx")
colnames(tcers) <- c("processo","dtIniExerc","dtFimExerc","relator","munic","codMun7","codMun6","org","procElet",
                     "dtRelAud","qtd","dtEsclar","qtd1","dtInstruc","qtd2","dtMpc",
                     "qtd3","anoSessao","numSessao","dtSessao","cdJulgador","Julgador")
colnames(tcers)
tcers$Esclar <- year(tcers$dtEsclar)+1
tcers$Instruc <- year(tcers$dtInstruc)+1
tcers$Julga <- year(tcers$dtSessao)+1

summary(tcers)

table(tcers$Esclar)

table(tcers$Instruc)

table(tcers$Julga)

tcers_efeitos <- tcers[,c(1,6:7,23:25)]

# inclusao da base de dados do efeito 'tratamento' das fiscalizacoes do tcers
rec.2013_2017_real$codMun7 <- as.numeric(rec.2013_2017_real$codMun7)
rec.2013_2017_real$efeitoEsclar <- as.factor(rec.2013_2017_real$efeitoEsclar)
rec.2013_2017_real$efeitoInstru <- as.factor(rec.2013_2017_real$efeitoInstru)
rec.2013_2017_real$efeitoJulga <- as.factor(rec.2013_2017_real$efeitoJulga)


#14. Criacao de base de dados UNIFICADA 2013-2017 ----------------------------

str(rec.2013_2017_real)
vars_2013_2017 <- rec.2013_2017_real %>% 
  select(nomeMun:transfConv,iptuPc:efeitoJulga_m)

# base IBGE domicilios
vars_ibge_dom_2010 <- ibge.dom %>% 
  select(codMun7,nomeMun,domRecensUrb,areaMunic,densPop,domUrbProp,pesDomUrb,pesDomUrbProp)
vars_ibge_dom_2010$codMun7 <- as.numeric(vars_ibge_dom_2010$codMun7)
# uniao dados receitas siconfi e ibge domicilios
vars_2013_2017 <- left_join(vars_2013_2017,vars_ibge_dom_2010,by=c("codMun7"))

# base IBGE PIB
vars_ibge_pib_2013_2017 <- pib.ibge.2013_2017 %>% filter(ano>=2013) %>% 
  select(ano,codMun7:partVabGovMun,pibMunReal,pibMunRealPc)
# uniao dados receitas siconfi, ibge domicilios e ibge pib
vars_2013_2017 <- left_join(vars_2013_2017,vars_ibge_pib_2013_2017,by=c("codMun7","ano"))

# base munic 2004
vars_munic_2004 <- munic.2004 %>% 
  select(anoAtualiPlanta:codMun7)
# uniao dados receitas siconfi, ibge domicilios, ibge pib e Munic2004
vars_2013_2017 <- left_join(vars_2013_2017,vars_munic_2004,by=c("codMun7"))

# base munic 2015
vars_munic_2015 <- munic.2015 %>% 
  select(codMun7:anoAtualiPlanta)
# uniao dados receitas siconfi, ibge domicilios, ibge pib, Munic2004 e Munic2015
vars_2013_2017 <- left_join(vars_2013_2017,vars_munic_2015,by=c("codMun7"))

# base turismo
vars_munic_turist <- munic_turist %>% 
  select(codMun7,clusterTur2017:demTurPc)
# uniao dados receitas siconfi, ibge domicilios, ibge pib, Munic2004,
# Munic2015 e dummy turista
vars_2013_2017 <- left_join(vars_2013_2017,vars_munic_turist,by=c("codMun7"))

# remover variaveis duplicadas
# str(vars_2013_2017)
vars_2013_2017 <- select(vars_2013_2017,-nomeMun.x,-ufSigla.x,
                         -nomeMun.x.x,-codMun3,
                         -ufSigla,-nomeMun.y.y,-nomeMun.y.y)

vars_2013_2017 <- rename(vars_2013_2017,ano=ano.x,
                         ufSigla=ufSigla.y,
                         anoAtualiPlanta2004=anoAtualiPlanta.x,
                         anoAtualiCad2004=anoAtualiCad.x,
                         anoAtualiPlanta2015=anoAtualiPlanta.y,
                         anoAtualiCad2015=anoAtualiCad.y,nomeMun=nomeMun.y)

vars_2013_2017$anoLeiIptu <- as.numeric(vars_2013_2017$anoLeiIptu)
vars_2013_2017$anoAtualiPlanta2004 <- as.numeric(vars_2013_2017$anoAtualiPlanta2004)
# correcao de valores zero ("0") por NAs na variavel anoAtualiCad2004
vars_2013_2017$anoAtualiCad2004 <- na_if(vars_2013_2017$anoAtualiCad2004, "0")
vars_2013_2017$anoAtualiCad2004 <- as.numeric(vars_2013_2017$anoAtualiCad2004)
# troca da resposta "Não foi atualizada" por "1900", pois nao e a mesma coisa que NAs
vars_2013_2017$anoAtualiPlanta2015 <- gsub("Não foi atualizada", "1900", vars_2013_2017$anoAtualiPlanta2015)
vars_2013_2017$anoAtualiPlanta2015 <- as.numeric(vars_2013_2017$anoAtualiPlanta2015)
# criacao variavel dummy atualizacao planta nos ultimos 5 anos desde 2015 (acima do ano de 2010)
vars_2013_2017$anoAtualiPlanta2015_5anos <- if_else(vars_2013_2017$anoAtualiPlanta2015>2010,1,0)
vars_2013_2017$anoAtualiPlanta2015 <- na_if(vars_2013_2017$anoAtualiPlanta2015,1900)
vars_2013_2017$anoAtualiCad2015 <- as.numeric(vars_2013_2017$anoAtualiCad2015)
# criacao variavel dummy atualizacao cadastro nos ultimos 5 anos desde 2015 (acima do ano de 2010)
vars_2013_2017$anoAtualiCad2015_5anos <- if_else(vars_2013_2017$anoAtualiCad2015>2010,1,0)

# criacao das variaveis iptu sobre o pib e densidade populacional (populacao/area do municipio)
vars_2013_2017 <- mutate(vars_2013_2017,
       iptuPib = iptu/pibMunReal,
       densPop2 = pop / areaMunic)

# base indice de vulnerabilidade social (ivs)
ivs_infraestrutura1 <- ivs_infraestrutura %>% select(codMun7,ivs,ivsInfra)
ivs_infraestrutura1$codMun7 <- as.numeric(ivs_infraestrutura1$codMun7)
# uniao dados receitas siconfi, ibge domicilios, ibge pib, Munic2004,
# Munic2015, dummy turista e ivs
vars_2013_2017 <- left_join(vars_2013_2017,ivs_infraestrutura1,by=c("codMun7"))
vars_2013_2017$ivs <- as.numeric(vars_2013_2017$ivs)
vars_2013_2017$ivsInfra <- as.numeric(vars_2013_2017$ivsInfra)

# base atlas pnud
atlas.pnud.2010 <- atlas.pnud.2010 %>% select(-ano,-ufSigla,-codMun6,-nomeMun)
# uniao bases
vars_2013_2017 <- left_join(vars_2013_2017,atlas.pnud.2010,by=c("codMun7"))

vars_2013_2017$codMun7 <- as.character(vars_2013_2017$codMun7)

str(vars_2013_2017)

library("dplyr")
vars_2013_2017_numeric <- select_if(vars_2013_2017, is.numeric)

# analise dos missing datas
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(vars_2013_2017,2,pMiss)

# correcao dos missing datas das variaveis AtualiPlanta2015 e AtualiCad2015
# quando missing em dados de 2015 substituir pelos valores equivalentes de 2004

vars_2013_2017 %>% select(anoAtualiPlanta2004,anoAtualiPlanta2015,
                          anoAtualiCad2004,anoAtualiCad2015,
                          emprHospPc,demTurPc) %>% summary()

vars_2013_2017 %>% filter(codUF=="RS"& ano==2017) %>% 
  select(codMun7,ano,anoAtualiPlanta2004,anoAtualiPlanta2015,
                          anoAtualiPlanta2015_2)

vars_2013_2017 %>% 
  select(codMun7,ano,anoAtualiPlanta2004,anoAtualiPlanta2015,
         anoAtualiPlanta2015_2) %>% apply(2,pMiss)

vars_2013_2017 %>% 
  select(codMun7,ano,anoAtualiCad2004,anoAtualiCad2015,
         anoAtualiCad2015_2) %>% apply(2,pMiss)

vars_2013_2017$anoAtualiPlanta2015_2 <- if_else(is.na(vars_2013_2017$anoAtualiPlanta2015),vars_2013_2017$anoAtualiPlanta2004,vars_2013_2017$anoAtualiCad2015)

vars_2013_2017$anoAtualiCad2015_2 <- if_else(is.na(vars_2013_2017$anoAtualiCad2015),vars_2013_2017$anoAtualiCad2004,vars_2013_2017$anoAtualiCad2015)


#15. analise descritiva Univariada -------------------------------------------

# referencia tabela sumario estatistico: https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html

# referencia: https://www.statmethods.net/stats/descriptives.html
itbi_maior <- rec.2013_2017_real %>% 
  mutate(itbi_maior=itbi>iptu) %>% 
  select(nomeMun:itbi,itbi_maior) %>%
  filter(itbi_maior==T) %>% 
  group_by(ano) %>% 
  summarise(n_itbi_maior = n())

n_itbi <- rec.2013_2017_real %>% 
  select(ano,itbi) %>%
  filter(itbi>0) %>% 
  group_by(ano) %>% 
  summarise(n_itbi = n())

issqn_maior <- rec.2013_2017_real %>% 
  mutate(iss_maior=issqn>iptu) %>% 
  select(nomeMun:itbi,iss_maior) %>%
  filter(iss_maior==T) %>% 
  group_by(ano) %>% 
  summarise(n_issqn_maior = n())

n_issqn <- rec.2013_2017_real %>% 
  select(ano,issqn) %>%
  filter(issqn>0) %>% 
  group_by(ano) %>% 
  summarise(n_issqn = n())

iptu_compara <- cbind(itbi_maior,n_itbi[,2],issqn_maior[,2],n_issqn[,2])
iptu_compara <- iptu_compara %>% mutate(itbi_maior_perc=n_itbi_maior/n_itbi, issqn_maior_perc=n_issqn_maior/n_issqn)

iptu_compara %>% 
  data.frame() %>%  
  stargazer(summary=FALSE, rownames=FALSE,type = "text", title="Estatística descritiva - Variáveis selecionadas 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,notes="Fonte: SICONFI/STN,2017.")

# mean,median,25th and 75th quartiles,min,max
summary(rec.2013_2017_real)

summary(rec.2013_2017$iptu)

tapply(rec.2013_2017$iptu, rec.2013_2017$ano, summary)

# n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles
# 5 lowest and 5 highest scores
describe(rec.2013_2017$iptu)

head(munic.2004)
summary(munic.2004)
table(munic.2004$cadImobInfo)
table(munic.2004$cadImobExist,munic.2004$cadImobInfo)
table(as.numeric(munic.2004$anoAtualiPlanta))

hist1 <- hist(as.numeric(munic.2004$anoAtualiPlanta),breaks = c(1970,1990,1995,2000,2004),
              main = "Atualização Planta Genérica de Valores - Munic 2004", freq = T)
hist1$counts

table(as.numeric(munic.2004$anoAtualiCad))

table(as.numeric(munic.2004$anoInstalMunic))

hist2 <- hist(as.numeric(munic.2004$anoAtualiCad),breaks = c(1970,1980,1990,2000,2010),
              main = "Atualização Cadastro Imobiliário - Munic 2004", freq = F)

vars_2015 <- vars_2013_2017 %>% filter(ano==2015)
str(vars_2015)

summary(vars_2015$pop)
describe(vars_2015$pop)
boxplot(vars_2015$pop)
boxplot(log(vars_2015$pop))

vars_2015 %>% ggplot(aes(y = log(pop))) +
  geom_boxplot() +
  scale_y_continuous(name = "População dos Municípios do Brasil") + 
    ggtitle("Boxplot da Populacao")

vars_2015 %>% ggplot(aes(y = pop)) +
  geom_boxplot() +
  scale_y_continuous(name = "População dos Municípios do Brasil") + 
  ggtitle("Boxplot da Populacao")


#16. correlacoes -------------------------------------------------------------

summary(vars_2015$anoInstalMunic)
summary(as.numeric(munic.2015$anoAtualiPlanta))

vars_2015 %>% mutate(grupo_planta_15= cut(anoAtualiPlanta2015, breaks=c(-Inf,2000,2005,2010,2015), right = T)) %>% 
  group_by(grupo_planta_15) %>% 
  summarise(n_mun=n(),inst_Munic=mean(anoInstalMunic,na.rm = T),pop=mean(pop),
            pib_pc=mean(pibMunRealPc))

vars_2015 %>% mutate(grupo_cad_15= cut(anoAtualiCad2015, breaks=c(-Inf,2000,2005,2010,2015), right = T)) %>% 
  group_by(grupo_cad_15) %>% 
  summarise(n_mun=n(),inst_Munic=mean(anoInstalMunic,na.rm = T),pop=mean(pop),
            pib_pc=mean(pibMunRealPc),iptu_pc=mean(iptuPc,na.rm = T))

vars_2015 %>%
  ggplot(aes(x = pop , y = anoAtualiCad2015 )) +
  geom_point(colour = "red")

lm_cad <- lm(log(anoAtualiCad2015)~log(pop) + 
               log(anoInstalMunic) + log(pibMunReal) +
               emprHospPc +
               densPop + transfIntergovRecCorr,data = vars_2015)

summary(lm_cad)

#17. regressoes por mqo --------------------------------------------------------------

str(vars_2013_2017)

vars_2013_2017 %>% filter(ano=="2017") %>% select(iptuPc,pop,densPop2,domUrbProp,
    transfIntergovRecCorr,codReg,municLitoral,emprHospPc,municCapital,municMetrop,
    pibMunRealPc,RDPC,partVabAgriMun,GINI,PMPOB,ivsInfra,anoInstalMunic,
    anoAtualiPlanta2015_2,anoLeiIptu,anoAtualiCad2015_2)%>% data.frame() %>%   
  stargazer(type = "html", title="Estatística descritiva - Variáveis do modelo em 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,notes="Fontes: Informadas no apêndice.",
            out="tab_descr_modelo.doc",summary.stat = c("n","mean","min","max","sd"))


# vars_2013_2017_naomit <- na.omit(vars_2013_2017)

lm_lniptuPc  <- lm(log(iptuPc) ~ log(pop) + densPop2 + 
                     domUrbProp + transfIntergovRecCorr + 
                   codReg + municLitoral + emprHospPc + municCapital + municMetrop +
                   log(pibMunRealPc)+ log(RDPC) + partVabAgriMun + GINI + PMPOB + ivsInfra +
                   anoInstalMunic + anoAtualiPlanta2015_2 + anoLeiIptu + anoAtualiCad2015_2, 
                   data = vars_2013_2017)

summary(lm_lniptuPc)

stargazer(lm_lniptuPc, title="Resultados Regressão por MQO",
          single.row=TRUE, no.space=TRUE,
          digits=2,decimal.mark = ",",digit.separator = ".",
          type="html",
          out="reg_linear.doc")

# step(lm_lniptuPc, direction = "backward")
# 
# fitstart <- lm(log(iptuPc) ~ 1,na.action=na.omit, data = vars_2013_2017)
# summary(fitstart)
# step(fitstart, direction = "forward", scope = formula(lm_lniptuPc))
# 
# # limpeza de NAs da base de dados vars_2013_2017
# vars_2013_2017 %>% drop_na(iptuPc)


# Stepwise Regression
library(MASS)
step <- stepAIC(lm_lniptuPc, direction="both")
step$anova # display results

# regressao quantilica
library(quantreg)
qs <- 1:9/10
qr_lniptuPc <- rq(log(iptuPc) ~ log(pop) + 
                    pesDomUrbProp + transfIntergovRecCorr + 
                    codReg + municLitoral + emprHospPc +
                    log(pibMunRealPc) + GINI + PMPOB + partVabAgriMun + ivsInfra +
                    anoInstalMunic + anoAtualiPlanta2015 + anoAtualiCad2015, 
                  data = vars_2013_2017, tau = qs)
summary(qr_lniptuPc)
plot.summary.rqs(summary(qr_lniptuPc))

# Salvar base de variáveis de interesse de 2013 a 2017
save(vars_2013_2017, file = "vars_2013_2017_iptu.RData")


#18. regressao por fronteira estocastica -------------------------------------

library(frontier)
library(plm)
library(pps)


#18.1 fronteira estocastica padrao
sfa_lniptuPc  <- sfa(log(iptuPc) ~ log(pop) + densPop2 + 
                       domUrbProp + transfIntergovRecCorr + 
                       codReg + municLitoral + emprHospPc +
                       municCapital + municMetrop +
                       log(pibMunRealPc) + log(RDPC) + partVabAgriMun + GINI + PMPOB  + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + anoAtualiCad2015_2, 
                     data = vars_2013_2017)

summary(sfa_lniptuPc)

dim(efficiencies(sfa_lniptuPc))
efficiencies(sfa_lniptuPc)
head(efficiencies(sfa_lniptuPc))
eff<- as.matrix(efficiencies(sfa_lniptuPc))
eff<-as.data.frame(eff)
vars_2013_2017$id <- rownames(vars_2013_2017)
eff$id<-rownames(eff)
colnames(eff)<-c("efic_sfa","id")
vars_2013_2017 <- left_join(vars_2013_2017,eff,by=c("id"))


# transformar em variavel de dados em painel
# vars_2013_2017 <- plm.data(vars_2013_2017, c("codMun7","ano"))
vars_2013_2017_pdata <- pdata.frame(vars_2013_2017, c("codMun7","ano"))

# retirar os NAs e balancear o painel de dados
# vars_2013_2017_na <- vars_2013_2017 %>% drop_na(iptuPc)
# vars_2013_2017_na <- make.pbalanced(vars_2013_2017_na,balance.type = "shared.individuals")
# vars_2013_2017_na <- make.pconsecutive(vars_2013_2017_na,balance = T)

# form.model1 <- log(iptuPc) ~ log(pop) + densPop2 + 
#   pesDomUrbProp + transfIntergovRecCorr + 
#   codReg + municLitoral + emprHospPc +
#   log(pibMunRealPc) + GINI + PMPOB + partVabAgriMun + ivsInfra +
#   anoInstalMunic + anoAtualiPlanta2015 + anoAtualiCad2015

#18.2. fronteira estocastica com dados em painel (efeito tempo)
# vars_2013_2017 %>% filter(ano==2017) %>% 
#   group_by(popClass) %>% summarise(n())
# vars_2013_2017 %>% group_by(ano) %>% describe()
# vars_2013_2017_100mil <- vars_2013_2017 %>% filter(pop>=100000)
# vars_2013_2017_100mil <- pdata.frame(vars_2013_2017_100mil, c("codMun7","ano"))

sfa_t_lniptuPc  <- sfa(log(iptuPc) ~ log(pop) + densPop2 + 
                         domUrbProp + transfIntergovRecCorr + 
                         codReg + municLitoral + emprHospPc +
                         municCapital + municMetrop +
                         log(pibMunRealPc) + log(RDPC) + partVabAgriMun + GINI + PMPOB  + ivsInfra +
                         anoInstalMunic + anoAtualiPlanta2015_2 + anoAtualiCad2015_2, 
                       data = vars_2013_2017_pdata,
                     timeEffect = TRUE)

summary(sfa_t_lniptuPc)
dim(efficiencies(sfa_t_lniptuPc))

head(efficiencies(sfa_t_lniptuPc))
eff2<- as.matrix(efficiencies(sfa_t_lniptuPc))
eff2<-as.data.frame(eff2)
eff2$codMun7<-rownames(eff2)
eff2<-melt(eff2,id.vars="codMun7")
colnames(eff2)<-c("codMun7","ano","efic_dsfa")
vars_2013_2017_pdata <- left_join(vars_2013_2017_pdata,eff2,by=c("codMun7","ano"))

detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)
#18.3. ranking dos 10 piores do RS
vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
  select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa) %>% 
  arrange(efic_dsfa) %>% top_n(-10,efic_dsfa) %>%  
  stargazer(type = "html", title="10 menores índices de eficiência arrecadatória de IPTU do RS em 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,
            out="down10_rs.doc",summary=FALSE, rownames=FALSE)


#18.4. ranking dos 10 melhores do RS
vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
  select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa) %>% 
  arrange(desc(efic_dsfa)) %>% top_n(10,efic_dsfa) %>%  
  stargazer(type = "html", title="10 maiores índices de eficiência arrecadatória de IPTU do RS em 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,
            out="top10_rs.doc",summary=FALSE, rownames=FALSE)


#18.5. ranking indices do RS
vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
  select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa) %>% 
  arrange(desc(efic_dsfa))%>%  
  stargazer(type = "html", title="Lista de índices de eficiência arrecadatória de IPTU do RS em 2017",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,
            out="lista_rs.doc",summary=FALSE, rownames=FALSE)

vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
  select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa)

#18.6. Estima o IPTU otimo com base no indice de eficiencia
vars_2013_2017_pdata$iptuOtimo<-(1/vars_2013_2017_pdata$efic_dsfa)*vars_2013_2017_pdata$iptu

vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
  select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa,iptu,iptuOtimo)%>% 
  summarise(iptuReal=sum(iptu,na.rm = T),iptuOtimo=sum(iptuOtimo,na.rm = T),
            iptuPotencial=sum(iptuOtimo,na.rm = T)/sum(iptu,na.rm = T)*100) %>%  
  stargazer(type = "html", title="RS 2017: IPTU Real x Ótimo, % potencial",
            digits=2,decimal.mark = ",",digit.separator = ".",no.space=T,
            out="iptu_otimo_rs.doc",summary=FALSE, rownames=FALSE)
  
  vars_2013_2017_pdata %>% filter(ufSigla=="RS" & ano==2017) %>%
    select(codMun7,nomeMun,iptuPc,efic_sfa,efic_dsfa,iptu,iptuOtimo) %>% summary()
