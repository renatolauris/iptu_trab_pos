

# install.packages("MatchIt")
library(MatchIt)
library(dplyr)
library(MASS)

# inicio avaliacao impacto auditoria no RS --------------------------------

vars_2014_2017_rs <- vars_2013_2017 %>% filter(codUF=="RS" & ano>=2014)
str(vars_2014_2017_rs)


# vars_2014_2017_rs <- vars_2014_2017_rs %>%
#   drop_na(iptuPc,pop,pesDomUrbProp,
#           transfIntergovRecCorr,
#           pibMunRealPc,PMPOB,partVabAgriMun,
#           anoInstalMunic, anoAtualiPlanta2015, anoAtualiCad2015)

treated <- (vars_2014_2017_rs$efeitoEsclar_t==1)
cov <- vars_2014_2017_rs %>% select(iptuPc,pop,pesDomUrbProp,
                                    transfIntergovRecCorr,
                                    pibMunRealPc,PMPOB,partVabAgriMun,
                                    anoInstalMunic, anoAtualiPlanta2015, anoAtualiCad2015)

str(cov)

cov <- c("iptuPc","pop","domUrbProp",
         "transfIntergovRecCorr",
         "pibMunRealPc","PMPOB","partVabAgriMun",
         "anoInstalMunic", "anoAtualiPlanta2015", "anoAtualiCad2015")


cov2 <- c("pop","densPop2","domUrbProp","transfIntergovRecCorr", 
          "emprHospPc",
          "pibMunRealPc","RDPC",
          "partVabAgriMun","GINI","PMPOB","ivsInfra",
          "anoInstalMunic","anoAtualiPlanta2015_2",
          "anoLeiIptu","anoAtualiCad2015_2")

# "municLitoral","municCapital","municMetrop",

vars_2014_2017_rs %>% select(codMun7,nomeMun,ano,efeitoEsclar_t) %>% head()

#1. Diferenca padronizada ---------------------------------------------------

vars_2014_2017_rs %>%
  group_by(efeitoEsclar_t) %>%
  select(one_of(cov2)) %>%
  summarise_all(funs(mean(., na.rm = T)))

vars_2014_2017_rs %>%
  group_by(efeitoInstru_t) %>%
  select(one_of(cov2)) %>%
  summarise_all(funs(mean(., na.rm = T)))

vars_2014_2017_rs %>%
  group_by(efeitoJulga_t) %>%
  select(one_of(cov2)) %>%
  summarise_all(funs(mean(., na.rm = T)))

t.test(vars_2014_2017_rs$iptuPc ~ vars_2014_2017_rs$efeitoEsclar_t)

multi.tests(fun = t.test,
            df = vars_2014_2017_rs,
            vars = cov2,
            group.var = "efeitoEsclar_t",
            var.equal = TRUE)

multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
         vars,                                                # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
         }
  )
}


#2. Resultado do modelo usando regressao ---------------------------------------------------

summary(vars_2014_2017_rs)

lm_iptuPc_eval_Esclar <- lm(log(iptuPc) ~ efeitoEsclar_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc + municCapital +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2, data = vars_2014_2017_rs)

summary(lm_iptuPc_eval_Esclar)

lm_iptuPc_eval_Instru <- lm(log(iptuPc) ~ efeitoInstru_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc + municCapital +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2, data = vars_2014_2017_rs)

summary(lm_iptuPc_eval_Instru)

lm_iptuPc_eval_Julga <- lm(log(iptuPc) ~ efeitoJulga_t + log(pop) + 
                              densPop2 + domUrbProp + transfIntergovRecCorr + 
                              municLitoral + emprHospPc + municCapital +
                              municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                              partVabAgriMun + GINI + PMPOB + ivsInfra +
                              anoInstalMunic + anoAtualiPlanta2015_2 + 
                              anoLeiIptu + anoAtualiCad2015_2, data = vars_2014_2017_rs)

summary(lm_iptuPc_eval_Julga)

stargazer(lm_iptuPc_eval_Esclar,lm_iptuPc_eval_Instru,
          lm_iptuPc_eval_Julga, title="Resultados Regressões Efeito Esclarecimento, Instrução e Julgamento dos processos de auditoria",
          no.space=TRUE,
          digits=2,decimal.mark = ",",digit.separator = ".",
          type="html",
          out="reg_linear_eval.doc")


#3. Calculo do escore de propensao ---------------------------------------------------

psm_iptu_rs <- glm(efeitoEsclar_t ~ log(pop) + 
              densPop2 + domUrbProp + transfIntergovRecCorr + 
              municLitoral + emprHospPc + municCapital +
              municMetrop + log(pibMunRealPc)+ log(RDPC) + 
              partVabAgriMun + GINI + PMPOB + ivsInfra +
              anoInstalMunic + anoAtualiPlanta2015_2 + 
              anoLeiIptu + anoAtualiCad2015_2,
            family = binomial(), data = vars_2014_2017_rs)
summary(psm_iptu_rs)
dim(psm_iptu_rs$data)
length(psm_iptu_rs$fitted.values)
length(predict(psm_iptu_rs, type = "response"))
length(psm_iptu_rs$model$efeitoEsclar_t)

psm_iptu_rs_df <- data.frame(pr_score = predict(psm_iptu_rs, type = "response"),
                     efeitoEsclar_t = psm_iptu_rs$model$efeitoEsclar_t)
head(psm_iptu_rs_df)

vars_2014_2017_rs$id <- row.names(vars_2014_2017_rs)
psm_iptu_rs_df$id <- row.names(psm_iptu_rs_df)
vars_2014_2017_rs <- left_join(vars_2014_2017_rs,psm_iptu_rs_df,by="id")
vars_2014_2017_rs$efeitoEsclar_t.y <- NULL
colnames(vars_2014_2017_rs)[colnames(vars_2014_2017_rs) == 'efeitoEsclar_t.x'] <- "efeitoEsclar_t"

psm_iptu_rs2 <- glm(efeitoInstru_t ~ log(pop) + 
                     densPop2 + domUrbProp + transfIntergovRecCorr + 
                     municLitoral + emprHospPc + municCapital +
                     municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                     partVabAgriMun + GINI + PMPOB + ivsInfra +
                     anoInstalMunic + anoAtualiPlanta2015_2 + 
                     anoLeiIptu + anoAtualiCad2015_2,
                   family = binomial(), data = vars_2014_2017_rs)
summary(psm_iptu_rs2)
dim(psm_iptu_rs2$data)
length(psm_iptu_rs2$fitted.values)
length(predict(psm_iptu_rs2, type = "response"))
length(psm_iptu_rs2$model$efeitoInstru_t)

psm_iptu_rs_df2 <- data.frame(pr_score2 = predict(psm_iptu_rs2, type = "response"),
                             efeitoInstru_t = psm_iptu_rs2$model$efeitoInstru_t)

psm_iptu_rs_df2$id <- row.names(psm_iptu_rs_df2)
vars_2014_2017_rs <- left_join(vars_2014_2017_rs,psm_iptu_rs_df2,by="id")
vars_2014_2017_rs$efeitoInstru_t.y <- NULL
colnames(vars_2014_2017_rs)[colnames(vars_2014_2017_rs) == 'efeitoInstru_t.x'] <- "efeitoInstru_t"

psm_iptu_rs3 <- glm(efeitoJulga_t ~ log(pop) + 
                      densPop2 + domUrbProp + transfIntergovRecCorr + 
                      municLitoral + emprHospPc + municCapital +
                      municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                      partVabAgriMun + GINI + PMPOB + ivsInfra +
                      anoInstalMunic + anoAtualiPlanta2015_2 + 
                      anoLeiIptu + anoAtualiCad2015_2,
                    family = binomial(), data = vars_2014_2017_rs)
summary(psm_iptu_rs3)
dim(psm_iptu_rs3$data)
length(psm_iptu_rs3$fitted.values)
length(predict(psm_iptu_rs3, type = "response"))
length(psm_iptu_rs3$model$efeitoJulga_t)

psm_iptu_rs_df3 <- data.frame(pr_score3 = predict(psm_iptu_rs3, type = "response"),
                              efeitoJulga_t = psm_iptu_rs3$model$efeitoJulga_t)

psm_iptu_rs_df3$id <- row.names(psm_iptu_rs_df3)
vars_2014_2017_rs <- left_join(vars_2014_2017_rs,psm_iptu_rs_df3,by="id")
vars_2014_2017_rs$efeitoJulga_t.y <- NULL
colnames(vars_2014_2017_rs)[colnames(vars_2014_2017_rs) == 'efeitoJulga_t.x'] <- "efeitoJulga_t"

stargazer(psm_iptu_rs,psm_iptu_rs2,
          psm_iptu_rs3, title="Resultados PSM Efeito Esclarecimento, Instrução e Julgamento dos Processos de Auditoria",
          no.space=TRUE,
          digits=2,decimal.mark = ",",digit.separator = ".",
          type="html",
          out="psm_eval.doc")

#4. Distribuicao do escore de propensao antes do matching ---------------------------------------------------

# back to back histogram
# histbackback(split(lalonde$psvalue,lalonde$treat), main = "Propensity score before matching",
#   xlab = c("control","treatment"))

par(mfrow=c(1,3))
histbackback(split(psm_iptu_rs_df$pr_score,psm_iptu_rs_df$efeitoEsclar_t), main = "Distribuição do Escore de propensão antes do matching - Efeito Esclarecimento",
             xlab = c("controle","tratamento"))
histbackback(split(psm_iptu_rs_df2$pr_score2,psm_iptu_rs_df2$efeitoInstru_t), main = "Distribuição do Escore de propensão antes do matching - Efeito Instrução",
             xlab = c("controle","tratamento"))
histbackback(split(psm_iptu_rs_df3$pr_score3,psm_iptu_rs_df3$efeitoJulga_t), main = "Distribuição do Escore de propensão antes do matching - Efeito Julgamento",
             xlab = c("controle","tratamento"))


labs <- paste("Tipo de Situação:", c("Fiscalizado", "Não fiscalizado"))
psm_iptu_rs_df %>%
  mutate(efeitoEsclar_t = ifelse(efeitoEsclar_t == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~efeitoEsclar_t) +
  xlab("Propabilidade de ser fiscalizado IPTU") +
  theme_bw()
  
#5. Matching do escore de propensao ---------------------------------------------------


vars_2014_2017_rs_nomiss <- vars_2014_2017_rs %>% 
  select(codMun7,nomeMun,ano,id,iptuPc,efeitoEsclar_t,efeitoInstru_t,
         efeitoJulga_t,pop,popClass,densPop2,domUrbProp,transfIntergovRecCorr,
         municLitoral,emprHospPc,municCapital,municMetrop,pibMunRealPc,
         RDPC,partVabAgriMun,GINI,PMPOB,ivsInfra,anoInstalMunic,
         anoAtualiPlanta2015_2,anoLeiIptu,anoAtualiCad2015_2,
         pr_score,pr_score2,pr_score3) %>% drop_na(pr_score,iptuPc)
summary(vars_2014_2017_rs_nomiss)
# apply(vars_2014_2017_rs_nomiss,2,pMiss)



# MatchIt does not allow missing values

# cov2 <- c("pop","densPop2","domUrbProp","transfIntergovRecCorr", 
#           "municLitoral","emprHospPc","municCapital",
#           "municMetrop","pibMunRealPc","RDPC",
#           "partVabAgriMun","GINI","PMPOB","ivsInfra",
#           "anoInstalMunic","anoAtualiPlanta2015_2",
#           "anoLeiIptu","anoAtualiCad2015_2")
# 

iptu_rs_ratio_match_e <- matchit(efeitoEsclar_t ~ log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc + municCapital +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2,
                       method = "nearest", data = vars_2014_2017_rs_nomiss, ratio=1)

summary(iptu_rs_ratio_match)
stargazer(iptu_rs_ratio_match)
plot(iptu_rs_ratio_match)
plot(iptu_rs_ratio_match, type = "jitter")
par(adj = 0)
plot(iptu_rs_ratio_match,type = "hist",line=-1)
iptu_rs_ratio_match_data_e <- match.data(iptu_rs_ratio_match_e)

labs <- paste("Tipo de Situação:", c("Fiscalizado", "Não fiscalizado"))
iptu_rs_ratio_match_data_e %>%
  mutate(efeitoEsclar_t = ifelse(efeitoEsclar_t == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~efeitoEsclar_t) +
  xlab("Propabilidade de ser fiscalizado IPTU: Pós Pareamento") +
  theme_bw()

iptu_rs_ratio_match_i <- matchit(efeitoInstru_t ~ log(pop) + 
                                   densPop2 + domUrbProp + transfIntergovRecCorr + 
                                   municLitoral + emprHospPc + municCapital +
                                   municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                                   partVabAgriMun + GINI + PMPOB + ivsInfra +
                                   anoInstalMunic + anoAtualiPlanta2015_2 + 
                                   anoLeiIptu + anoAtualiCad2015_2,
                                 method = "nearest", data = vars_2014_2017_rs_nomiss, ratio=1)

iptu_rs_ratio_match_data_i <- match.data(iptu_rs_ratio_match_i)

iptu_rs_ratio_match_j <- matchit(efeitoJulga_t ~ log(pop) + 
                                   densPop2 + domUrbProp + transfIntergovRecCorr + 
                                   municLitoral + emprHospPc + municCapital +
                                   municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                                   partVabAgriMun + GINI + PMPOB + ivsInfra +
                                   anoInstalMunic + anoAtualiPlanta2015_2 + 
                                   anoLeiIptu + anoAtualiCad2015_2,
                                 method = "nearest", data = vars_2014_2017_rs_nomiss, ratio=1)

iptu_rs_ratio_match_data_j <- match.data(iptu_rs_ratio_match_j)


#6. Metodo de diferenças em diferencas com multiplos períodos ---------------------------------------------------
# uso do método de dados em painel

# analise inicial da heterogeneidade
# install.packages("gplots")
library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(lmtest)    # For hetoroskedasticity analysis

# heterogeneidade por municipio
plotmeans(vars_2014_2017_rs$iptuPc ~ vars_2014_2017_rs$codMun7, data = vars_2014_2017_rs)

# heterogeneidade por ano
plotmeans(vars_2014_2017_rs$iptuPc ~ vars_2014_2017_rs$ano, data = vars_2014_2017_rs)

# fonte: https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
# regressao de dados em painel sem psm
vars_2014_2017_rs_pdata <- pdata.frame(vars_2014_2017_rs, index=c("codMun7","ano"))
pdata_sem_psm_e <- plm(log(iptuPc) ~ efeitoEsclar_t + log(pop) + 
                    densPop2 + domUrbProp + transfIntergovRecCorr + 
                    municLitoral + emprHospPc + municCapital +
                    municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                    partVabAgriMun + GINI + PMPOB + ivsInfra +
                    anoInstalMunic + anoAtualiPlanta2015_2 + 
                    anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=vars_2014_2017_rs_pdata, model="within")
summary(pdata_sem_psm_e)

pdata_sem_psm_i <- plm(log(iptuPc) ~ efeitoInstru_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc + municCapital +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=vars_2014_2017_rs_pdata, model="within")
summary(pdata_sem_psm_i)

pdata_sem_psm_j <- plm(log(iptuPc) ~ efeitoJulga_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc + municCapital +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=vars_2014_2017_rs_pdata, model="within")
summary(pdata_sem_psm_j)

stargazer(pdata_sem_psm_e,pdata_sem_psm_i,
          pdata_sem_psm_j, title="Resultados Diferença em Diferenças sem PSM Efeito Esclarecimento, Instrução e Julgamento dos Processos de Auditoria",
          no.space=TRUE,
          digits=2,decimal.mark = ",",digit.separator = ".",
          type="html",
          out="did_spsm_eval.doc")

# regressao de dados em painel com psm
iptu_rs_ratio_match_data_pdata_e <- pdata.frame(iptu_rs_ratio_match_data_e, index=c("codMun7","ano"))
pdata_com_psm_e <- plm(log(iptuPc) ~ efeitoEsclar_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=iptu_rs_ratio_match_data_pdata_e, model="within")
summary(pdata_com_psm_e)

iptu_rs_ratio_match_data_pdata_i <- pdata.frame(iptu_rs_ratio_match_data_i, index=c("codMun7","ano"))
pdata_com_psm_i <- plm(log(iptuPc) ~ efeitoInstru_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=iptu_rs_ratio_match_data_pdata_i, model="within")
summary(pdata_com_psm_i)

iptu_rs_ratio_match_data_pdata_j <- pdata.frame(iptu_rs_ratio_match_data_j, index=c("codMun7","ano"))
pdata_com_psm_j <- plm(log(iptuPc) ~ efeitoJulga_t + log(pop) + 
                       densPop2 + domUrbProp + transfIntergovRecCorr + 
                       municLitoral + emprHospPc +
                       municMetrop + log(pibMunRealPc)+ log(RDPC) + 
                       partVabAgriMun + GINI + PMPOB + ivsInfra +
                       anoInstalMunic + anoAtualiPlanta2015_2 + 
                       anoLeiIptu + anoAtualiCad2015_2 + factor(ano), data=iptu_rs_ratio_match_data_pdata_j, model="within")
summary(pdata_com_psm_j)

stargazer(pdata_com_psm_e,pdata_com_psm_i,
          pdata_com_psm_j, title="Resultados Diferença em Diferenças com PSM Efeito Esclarecimento, Instrução e Julgamento dos Processos de Auditoria",
          no.space=TRUE,
          digits=2,decimal.mark = ",",digit.separator = ".",
          type="html",
          out="did_psm_eval.doc")

