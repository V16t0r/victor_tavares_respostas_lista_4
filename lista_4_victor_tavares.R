
#### Questão 1

https://github.com/V16t0r/victor_tavares_respostas_lista_4


#### Questão 2

install.packages('tidyverse'); require(tidyverse)
install.packages('readxl'); require(readxl)


# arquivos do censo escolar 2016

setwd("C:/Users/DELL/Documents/2019.1/Análise de dados/dados_encontro_2_ufpe")
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")


# os dados do PNUD
require(ffbase)
library(readr)
setwd("C:/Users/DELL/Documents/2019.1/Análise de dados/dados_lista_4")
pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)
head(pnud)
unique(pnud$ANO)

# selecionando dados de 2010 e do UF Pernambuco 
pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)

# removendo a base pnud
rm(pnud)


# processando os dados do censo escolar 

# turmas

turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_turmas = n(), 
            turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

dim(turmas_pe_sel)[1] == length(unique(turmas_pe$CO_MUNICIPIO))
summary(turmas_pe_sel)

# escolas

escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))

dim(escolas_pe_sel) [1] == length(unique(escolas_pe$CO_MUNICIPIO))
summary(escolas_pe_sel)

# docentes 

docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>%
summarise(n_docentes = n(), 
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))
            
dim(docentes_pe_sel)[1] == length(unique(docentes_pe$CO_MUNICIPIO))
summary(docentes_pe_sel)

# matriculas 

matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(), 
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T))

dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))
summary(matriculas_pe_sel)

# Unindo as bases de dados do censo e pnud

# matricula

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO"))

dim(pnud_pe_2010)
dim(matriculas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# escolas

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))
dim(escolas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# turmas 

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))
dim(turmas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)


# docentes 

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))
dim(docentes_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# salvando a nova base

setwd("C:/Users/DELL/Documents/2019.1/Lista 4")
save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")
write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)



#carregando nova base

setwd("C:/Users/DELL/Documents/2019.1/Lista 4")
load("2016_censo_pnud_pe_sel.RData")

head(censo_pnud_pe_sel)


# não deve haver docentes com mais de 70 ou menos de 18 anos
# carreguei a base\os dados docentes_pe diretamente da parte do enviroment para nao mudar de diretório

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)
dim(docentes_pe_selecao)

# não deve haver aluno com mais de 25 e menos de 1 ano
# carreguei a base\dados matriculas diretamento do environment para não mudar de diretorio


matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)
dim(matricula_pe_selecao)


# criando a variável de alunos por docentes

alunos_por_docentes <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes.x

# Estatística descritivas dos alunos por docentes no Estado

View(alunos_por_docentes)

summary(alunos_por_docentes)
plot(alunos_por_docentes)
y <- alunos_por_docentes
var(y)  # variância
sd(y)   #desvio padrao
100*sd(y)/mean(y)   #coeficiente de variação

# municipio com o maior número de alunos por docentes e o seu idhm
# unindo as variáveis 

censo_pnud_pe_sel_alunos_docentes <- censo_pnud_pe_sel %>% mutate(alunos_por_docentes)
View(alunos_por_docentes)
View(censo_pnud_pe_sel_alunos_docentes)
censo_pnud_pe_sel_alunos_docentes["177", ]
View(censo_pnud_pe_sel$IDHM)

# A cidade é tupanatinga e o idmh é 0.519

# correlação de person

cor(censo_pnud_pe_sel_alunos_docentes$aluno_por_docente, censo_pnud_pe_sel_alunos_docentes$IDHM)
cor.test(censo_pnud_pe_sel_alunos_docentes$aluno_por_docente, censo_pnud_pe_sel_alunos_docentes$IDHM)


#### questão 3

install.packages("ggplot2")
library(ggplot2)
ggplot(censo_pnud_pe_sel_alunos_docentes, aes(alunos_por_docentes, IDHM, color = IDHM))+geom_point()



