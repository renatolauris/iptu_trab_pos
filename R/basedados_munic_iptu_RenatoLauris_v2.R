# remover os dados salvos
# rm(list = ls())

# devtools::install_github("tbrugz/ribge")

# carregar pacotes
library(electionsBR)
library(ribge)
library(tidyverse)
library(readxl)
library(stringr)
# endereco <- choose.dir()
endereco <- "C:\\13.Pós PUCRS\\4. Trabalho final\\Projeto no github\\iptu_trab_pos\\data\\"
setwd(endereco)

# SICONFI IPTU ------------------------------------------------------------

# carregar dados SICONFI IPTU por Munic?pios 2013 a 2016

# dados 2013

# siconfi 2013 ------------------------------------------------------------

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

# Adicionar linha de comando para os anos de 2014 em diante!!!
# rec.2014.iptu <- rec.2014.iptu[,-c(6:7)]

colnames(rec.2013.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2013.iptu <- mutate(rec.2013.iptu,
       valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2013.iptu <- rec.2013.iptu %>% select(-conta,-deducoes.receita,-receitas.realizadas)
rec.2013.iptu <- rec.2013.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2013.iptu <- rec.2013.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2013.iptu <- spread(rec.2013.iptu, nroConta, valor)

colnames(rec.2013.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                           "recCorr","impostos","iptu","itbi","issqn",
                           "transfCorr","transfIntergov","transfUniao",
                           "transfEst","transfConv")
                             
# siconfi 2014 ------------------------------------------------------------

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

# Adicionar linha de comando para os anos de 2014 em diante!!!
# rec.2014.iptu <- rec.2014.iptu[,-c(8:9)]

colnames(rec.2014.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2014.iptu <- mutate(rec.2014.iptu,
                        valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2014.iptu <- rec.2014.iptu %>% select(-conta,-deducoes.receita,-receitas.realizadas)
rec.2014.iptu <- rec.2014.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2014.iptu <- rec.2014.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2014.iptu <- spread(rec.2014.iptu, nroConta, valor)

colnames(rec.2014.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")



# versao antiga siconfi 2014 -----------------------------------------------------------
# 
# rec.2014.iptu <- rec.2014 %>% filter(str_detect(Conta, '1.1.1.2.02.00.00'))
# 
# rec.2014.iptu <- spread(rec.2014.iptu, Coluna, Valor)
# 
# rec.2014.iptu <- rec.2014.iptu[,-c(6:7)]
# 
# colnames(rec.2014.iptu) <- c("instituicao","cod.IBGE","uf","populacao","conta","deducoes.receita","receitas.realizadas")
# 
# rec.2014.iptu <- mutate(rec.2014.iptu,
#                         iptu = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE),
#                         ano = 2014)
# 
# rec.2014.iptu <- rec.2014.iptu %>% select(-conta)
# rec.2014.iptu <- rec.2014.iptu %>% mutate(instituicao=gsub("Prefeitura Municipal de ","",instituicao))
# rec.2014.iptu <- rec.2014.iptu %>% mutate(instituicao=gsub(" - ..","",instituicao))

# siconfi 2015 ------------------------------------------------------------

rec.2015 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2015.zip",skip=3)

rec.2015 <-  mutate(rec.2015,nroConta=str_sub(Conta,1,16), ano=2015)

rec.2015.iptu <-  filter(rec.2015, nroConta %in% buscaCodSiconfi)

rec.2015.iptu <- spread(rec.2015.iptu, Coluna, Valor)

rec.2015.iptu <- rec.2015.iptu[,-c(8:9)]

# Adicionar linha de comando para os anos de 2014 em diante!!!
# rec.2015.iptu <- rec.2015.iptu[,-c(8:9)]

colnames(rec.2015.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2015.iptu <- mutate(rec.2015.iptu,
                        valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2015.iptu <- rec.2015.iptu %>% select(-conta,-deducoes.receita,-receitas.realizadas)
rec.2015.iptu <- rec.2015.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2015.iptu <- rec.2015.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2015.iptu <- spread(rec.2015.iptu, nroConta, valor)

colnames(rec.2015.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")

# siconfi 2016 ------------------------------------------------------------

rec.2016 <- read_csv2("finbra_MUN_ReceitasOrcamentarias(AnexoI-C)2016.zip",skip=3)

rec.2016 <-  mutate(rec.2016,nroConta=str_sub(Conta,1,16), ano=2016)

rec.2016.iptu <-  filter(rec.2016, nroConta %in% buscaCodSiconfi)

rec.2016.iptu <- spread(rec.2016.iptu, Coluna, Valor)

rec.2016.iptu <- rec.2016.iptu[,-c(8:9)]

# Adicionar linha de comando para os anos de 2014 em diante!!!
# rec.2016.iptu <- rec.2016.iptu[,-c(8:9)]

colnames(rec.2016.iptu) <- c("nomeMun","codMun7","ufSigla","pop","conta","nroConta","ano","deducoes.receita","receitas.realizadas")

rec.2016.iptu <- mutate(rec.2016.iptu,
                        valor = mapply(sum, receitas.realizadas,(-1)*deducoes.receita, na.rm=TRUE))

rec.2016.iptu <- rec.2016.iptu %>% select(-conta,-deducoes.receita,-receitas.realizadas)
rec.2016.iptu <- rec.2016.iptu %>% mutate(nomeMun=gsub("Prefeitura Municipal de ","",nomeMun))
rec.2016.iptu <- rec.2016.iptu %>% mutate(nomeMun=gsub(" - ..","",nomeMun))

rec.2016.iptu <- spread(rec.2016.iptu, nroConta, valor)

colnames(rec.2016.iptu) <- c("nomeMun","codMun7","ufSigla","pop","ano",
                             "recCorr","impostos","iptu","itbi","issqn",
                             "transfCorr","transfIntergov","transfUniao",
                             "transfEst","transfConv")


# rbind anos 2013 a 2016 --------------------------------------------------
# dados de 2013 a 2016 da receita de iptu dos municipios brasileiros; fonte: SICONFI https://siconfi.tesouro.gov.br/siconfi/index.jsf

rec.2013_2016 <- rbind(rec.2013.iptu,rec.2014.iptu,rec.2015.iptu,rec.2016.iptu)
rec.2013_2016 <- rec.2013_2016[,-1]
# N? domicilios Ibge SIDRA ------------------------------------------------

# uso do pacote sidrar
# para usar dados do IBGE dispon?veis no SIDRA: https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2010/universo-caracteristicas-da-populacao-e-dos-domicilios

# carregamento dos pacotes SIDRA ------------------------------------------------
library(tidyverse)
library(sidrar)
library(ggplot2)
library(scales)
library(dynlm)


# tabela 1134 SIDRA/IBGE n? de domicilios por munic?pio Censo 2010
# https://sidra.ibge.gov.br/tabela/1134#resultado
tabela.dom.munic <- get_sidra(api="/t/1134/n6/all/v/allxp/p/all/c1/0/c460/0/c317/0/c11561/0/c12237/0/c11562/0")

tabela.dom.munic <- tabela.dom.munic[,c(1:2,6,21)]
colnames(tabela.dom.munic) <- c("codMun7","nomeMun","ano","domPerm")


# tabela 1310 SIDRA/IBGE n? de domicilios recenseados por munic?pio Censo 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/1310
tabela.dom.recens.munic <- get_sidra(api="/t/1310/n6/all/v/allxp/p/last%201/c3/0/c1/0")

tabela.dom.recens.munic <- tabela.dom.recens.munic[,c(1:2,6,13)]
colnames(tabela.dom.recens.munic) <- c("codMun7","nomeMun","ano","domRecens")

# tabela 3033 SIDRA/IBGE n? de domicilios particulares ocupados por munic?pio Censo 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/3033#resultado
tabela.dom.ocup.munic <- get_sidra(api="/t/3033/n6/all/v/618/p/all/c11277/9806")

tabela.dom.ocup.munic <- tabela.dom.ocup.munic[,c(1:2,6,11)]
colnames(tabela.dom.ocup.munic) <- c("codMun7","nomeMun","ano","domPartOcup")

# tabela 185 SIDRA/IBGE Domic?lios particulares permanentes por situa??o e n?mero de moradores Censo 2000 e 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/185

tabela.dom.perm <- get_sidra(api="/t/185/n6/all/v/allxp/p/all/c1/0/c68/0")

tabela.dom.perm <- tabela.dom.perm[,c(1:2,6,13)]
colnames(tabela.dom.perm) <- c("codMun7","nomeMun","ano","domPartPerm")

# tabela 156 SIDRA/IBGE Domic?lios particulares ocupados, moradores em domic?lios particulares ocupados e m?dia de moradores em domic?lios particulares ocupados Censo 2000 e 2010
# para descobrir os parametros para api ir no botao "links para compartilhar" 
# https://sidra.ibge.gov.br/tabela/156

tabela.dom.ocup <- get_sidra(api="/t/156/n6/all/v/134,2048/p/all")

tabela.dom.ocup <- tabela.dom.ocup[,c(1:3,6,9)]

# como a tabela possui duas variaveis é preciso usar a funcao spread
tabela.dom.ocup <- spread(tabela.dom.ocup, "Variável (Código)", Valor)
colnames(tabela.dom.ocup) <- c("codMun7","nomeMun","ano","pesDomOcup","domOcup")

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

# industria
pib.part.ind.munic.all <- get_sidra(api="/t/5938/n6/all/v/520/p/all/d/v520%202")
pib.part.ind.munic.all <- pib.part.ind.munic.all[,c(1:2,6,9)]
colnames(pib.part.ind.munic.all) <- c("codMun7","nomeMun","ano","partVabIndMun")

# servicos
pib.part.serv.munic.all <- get_sidra(api="/t/5938/n6/all/v/6574/p/all/d/v6574%202")
pib.part.serv.munic.all <- pib.part.serv.munic.all[,c(1:2,6,9)]
colnames(pib.part.serv.munic.all) <- c("codMun7","nomeMun","ano","partVabIndMun")

# adm publica
pib.part.admpub.munic.all <- get_sidra(api="/t/5938/n6/all/v/528/p/all/d/v528%202")
pib.part.admpub.munic.all <- pib.part.admpub.munic.all[,c(1:2,6,9)]
colnames(pib.part.admpub.munic.all) <- c("codMun7","nomeMun","ano","partVabIndMun")


# Atlas PNUD --------------------------------------------------------------

# carregar dados Atlas Municipal baixados fonte: PNUD http://www.atlasbrasil.org.br/2013/pt/download/ 
atlas.pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx","MUN 91-00-10")
atlas.pnud.dicvar <- read_excel("atlas2013_dadosbrutos_pt.xlsx","Siglas")

head(atlas.pnud)

# dados 2000 e 2010
atlas.pnud <- atlas.pnud %>% filter(ANO >= 2000)

colnames(atlas.pnud)[1:5] <- c("ano","ufSigla","codMun6","codMun7","nomeMun")


# Munic IBGE 2015 ---------------------------------------------------------

# carregar dados Munic 2015 baixados fonte IBGE:https://ww2.ibge.gov.br/home/estatistica/economia/perfilmunic/2015/default.shtm
unzip(zipfile="Base_MUNIC_2015_xls.zip", files = "Base_MUNIC_2015.xls", exdir=".")
munic.2015 <- read_excel("Base_MUNIC_2015.xls","Recursos para gest?o")

# variaveis importantes A61 a A68 para a gest?o de IPTU

munic.2015 <- munic.2015 %>% select(A1:A68)

# renomear names das variaveis coluna

colnames(munic.2015) <- c("codMun7","ufSigla","codMun3","nomeMun",
                          "cadImobExist","cadImobInfo","anoAtualiCad",
                          "cobraIptu","anoLeiIptu","pgvExist",
                          "pgvInfo","anoAtualiPlanta")

munic.2015.dicvar <- read_excel("Base_MUNIC_2015.xls","Dicion?rio")
munic.2015.dicvar <- munic.2015.dicvar %>% filter(!is.na(vari?vel))

# descri??o das variaveis selecionadas A1 a A68
data.frame(munic.2015.dicvar[69:76,c(1,2,6)])



# Dados Eleitorais TSE ----------------------------------------------------
# dados baixados fonte: TSE http://www.tse.jus.br/eleitor-e-eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

# Fonte consulta: http://fmeireles.com/blog/rstats/electionsbr-um-pacote-para-baixar-dados-eleitorais-do-tse
# http://fmeireles.com/blog/rstats/electionsBR-versao-0.3.0
# http://fmeireles.com/blog/rstats/electionsbr-analisando-a-apuracao-das-eleicoes-para-a-camara-dos-deputados
# install.packages("electionsBR")

# Carrega o pacote
library(electionsBR)

# Baixa os dados sobre candidaturas federais
# dados <- candidate_fed(year = 2002)

# Cria um vetor armazenando os anos
anos <- seq(2000, 2016, by = 4)

# votos elei??es municipais desagregados de 2016
mun <- vote_mun_zone_local(2016)

# Baixa os dados com lapply e vote_mun_zone_local Votos Elei??es Municipais
votaMunicAll <- lapply(anos, vote_mun_zone_local)

# Une as bases (se foram baixadas separadamente, comente a linha abaixo)
votaMunicAll <- bind_rows(votaMunicAll)

# criar variavel eleito, pois ha diferenca de codigo entre os dados antes de 2008 e depois
# filtro CODIGO_SIT_CANDIDATO = 2 (DEFERIDO), 4 (SUB JUDICE), 16 (DEFERIDO COM RECURSO)
# filtro CODIGO_SIT_CAND_TOT = 1 (ELEITO), 2 (ELEITO POR QP) - ap?s 2012, 3 (ELEITO POR M?DIA) - ap?s 2012
# 5 (ELEITO POR M?DIA) - antes 2008

votaMunicAll <- votaMunicAll %>% mutate(eleito= ifelse((ANO_ELEICAO<=2008 & (CODIGO_SIT_CANDIDATO %in% c(2,4,16)) & CODIGO_SIT_CAND_TOT==1)|(ANO_ELEICAO>2008 & (CODIGO_SIT_CANDIDATO %in% c(2,4,16)) & (CODIGO_SIT_CAND_TOT %in% 1:3)),1,0))

head(votaMunicAll)

# cria variavel de competicao eleitoral pela diferenca de votos
# entre o primeiro e segundo colocados

compEleDifVots <- votaMunicAll %>% group_by(SIGLA_UE,ANO_ELEICAO) %>% summarise(primeiro = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[1]])), segundo = (as.numeric(TOTAL_VOTOS[order(as.numeric(TOTAL_VOTOS),decreasing=T)[2]])),total = sum(as.numeric(TOTAL_VOTOS)),competEleicaoDif =  round((primeiro-segundo)/total*100,2))

# detalhes de apura??o elei?oes municipais
# detalhesMun <- details_mun_zone_local(2016)
# head(detalhesMun)

# dados sobre candidaturas individuais municipais
candLocal2016 <- candidate_local(2016,uf="RS")

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
# h? casos de ter sido necessaria a realizacao de 
# eleicoes complementares
# totalprefeitos <- group_by(execEleitos, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalPref = length(unique(CPF_CANDIDATO)))
# write.csv2(totalprefeitos,"prefeitos.csv")



# inclusao da variavel de competicao eleitoral a partir do numero de
# candidatos em disputa para prefeitura
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

# filter(uniao_por_ano_munic,SIGLA_UE=="61050")

# inclusao da variavel contar para calcular o numero de vereadores
# eleitos por coligacao
legis_coligacao <- group_by(legisEleitos, SIGLA_UE,ANO_ELEICAO,NOME_COLIGACAO) %>% summarise(verColigacao = length(unique(CPF_CANDIDATO)))
legis_coligacao <- left_join(legis_coligacao,execEleitos,by=c("SIGLA_UE","ANO_ELEICAO"))
legis_coligacao <- group_by(legis_coligacao, SIGLA_UE,ANO_ELEICAO) %>% summarise(totalVer = sum(verColigacao), colig = sum(verColigacao[NOME_COLIGACAO.x==NOME_COLIGACAO.y]),maiorCamaraColig = round(colig/totalVer*100,2))


ibgeTse <- read.csv("ibge-tse-map.csv",encoding = "UTF-8")
colnames(ibgeTse) <- c("ufSigla","codMun7","codMunTSE","nomeMun","nomeMunTSE")
# preencher com zeros a variavel dos codigos de municipios TSE para ficar 
# com 5 digitos
library(stringr)
ibgeTse$codMunTSE <- str_pad(ibgeTse$codMunTSE, 5, pad = "0")

# join de codigos de municipios TSE e IBGE unificar com o codigo IBGE
legis_coligacao <- left_join(legis_coligacao,ibgeTse,by=c("SIGLA_UE"="codMunTSE"))


# resto (desconsiderar) ---------------------------------------------------

# candidato eleito a prefeito porto alegre 2016
# mun.portoalegre <- mun %>% filter(NOME_MUNICIPIO=='PORTO ALEGRE' & CODIGO_CARGO==11 & CODIGO_SIT_CAND_TOT==1)


# exemplo de quebra em factors (categorias)
# rec.2006 <- read_excel("finbra_receitas_2006.xlsx")
# 
# head(rec.2006)
# 
# rec.2006.pq <- rec.2006 %>% select(CD_UF:IPTU)
# 
# # inclusao de variavel categorica da faixa populacional
# quebra.pop <- c(0,20000,50000,100000,250000,500000,1000000,2000000,12000000)
# rotulo.quebra <- c("0 a 20 mil","20 a 50 mil","50 a 100 mil","100 a 250 mil","250 a 500 mil","500 mil a 1 milhao","1 a 2 milhoes","2 a 12 milhoes")
# rec.2006.pq$'faixa pop' <- cut(rec.2006.pq$Populacao, quebra.pop,labels = rotulo.quebra)
# 

# install.packages("congressbr")
# library(congressbr)
# cham_votes(type = "PL",  number = "6787",  year = "2016")
