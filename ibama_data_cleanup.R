
# Code Overview -----------------------------------------------------------

# Code to clean data from IBAMA on wildlife crimes
# Script created by Emilio M. Bruna, embruna@ufl.edu

# Load libraries ----------------------------------------------------------
library(tidyverse)


# DATA ENTRY AND NAME CLEANUP ---------------------------------------------
# TODO: should be able to pull these in directly from website but need 
#  to deal with SSL peer certificate issue

# load the CSV file
# Autos de infração - espécimes
espec <- read_delim('./data_raw/especime.csv',delim=";")
enquad <- read_delim('./data_raw/enquadramento.csv',delim=";")

# names(espec)
# names(enquad)


# Combining datasets ------------------------------------------------------

# All that have 'SEQ_AUTO_INFRACAO' in common
dados<-inner_join(espec,enquad, by=c("SEQ_AUTO_INFRACAO","ULTIMA_ATUALIZACAO_RELATORIO"))
names(dados)


# cleanup -----------------------------------------------------------------
# make col headings lower case
names(dados)<-tolower(names(dados))
# all data lower case and remove any ws before and after entry
dados <- dados %>% 
  mutate(across(everything(), trimws)) %>% 
  mutate(across(everything(), tolower)) %>% 
  mutate(seq_auto_infracao=as.factor(seq_auto_infracao))


# split df by decreto or lei -----------------------------------------------

dados_decreto<-dados %>% 
  filter(tipo_norma=="decreto")
dados_lei<-dados %>% 
  filter(tipo_norma=="lei")

# identifying duplicates --------------------------------------------------
dupes <- dados %>% 
  group_by(seq_auto_infracao) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))
dupes

# summaries --------------------------------------------------------

# Number of different seq_auto_infracao
dados %>% summarize(n=n_distinct(seq_auto_infracao))

# Number of species (by scientific name, note all NA pooled)
dados %>% 
  summarize(n=n_distinct(nome_cientifico))

# Number of species (by scientific name, note all NA pooled)
dados %>% 
  summarize(n=n_distinct(nome_popular))


summary_test <- dados_lei %>% 
  group_by(seq_auto_infracao,tipo,grupo,nome_cientifico,nome_popular) %>% 
  summarize(n=n()) %>% 
  arrange(desc(seq_auto_infracao))
summary_test

write_csv(summary_test,"./data_clean/summary_test.csv")

