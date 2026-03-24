#install.packages(c("tm", "topicmodels", "textmineR", "text2vec", "SnowballC"))
#install.packages("tidyr")
#install.packages("devtools")
#install.packages("ldatuning") 
#install.packages("ggthemes")
#install.packages("DT")
#install.packages("flextable")
# install klippy for copy-to-clipboard button in code chunks
#install.packages("remotes")
#remotes::install_github("rlesur/klippy")
library(ldatuning)  
library(tm)
library(topicmodels)
library(SnowballC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(flextable)
klippy::klippy()
library(DT)
library(ggthemes)
library(ldatuning)


################------------ LDA MODEL-------------------############################

#-----------------------------------------------------------
#Seleccionar el texto al macrocaso correspondiente:
texto_completo <- readLines("C:Desktop/Casos_JEP/textos_depurados_final/texto_sin_stopwords_8.txt", encoding = "UTF-8")

#------------------------------------------------------------
#Crear corpus
corpus <- VCorpus(VectorSource(texto_completo))

#-------------------------------------------------------------
# Pre-procesamiento


corpus <- tm_map(corpus, content_transformer(tolower))       
corpus <- tm_map(corpus, removePunctuation)                  
corpus <- tm_map(corpus, removeNumbers)                      
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))  
corpus <- tm_map(corpus, stripWhitespace)                    
dtm <- DocumentTermMatrix(corpus)

#--------------------------------------------------------------
# Filtro de términos 


freq <- colSums(as.matrix(dtm))
umbral_inferior <- 3     
umbral_superior <- 0.9   

keep_terms <- names(freq)[freq >= umbral_inferior]

dtm_filtrado <- dtm[, keep_terms]
dtm_filtrado <- dtm_filtrado[rowSums(as.matrix(dtm_filtrado)) > 0, ]

# -------------------------------------------------------------
# Selección del número de tópicos 

valores_k <- 2:20 ### se fija un máximo debido a la interpretación,
resultados_k <- FindTopicsNumber(
  dtm = dtm_filtrado,
  topics = valores_k,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 7)

FindTopicsNumber_plot(resultados_k)


# --------------------------------------------------------------
#  Ajustar un modelo LDA con el k elegido

k_optimo <- 6  # reemplazar conforme lo indican los gráficos de cada uno de los Macrocasos
modelo_lda <- LDA(dtm_filtrado, k = k_optimo, method = "Gibbs",
                  control = list(seed = 1234)) 
terms(modelo_lda, 8)
tidy_lda <- tidy(modelo_lda)
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_viridis_d(option = "cividis")
  labs(title = "",
       x = "Término",
       y = "Probabilidad β")



################------------ LDA MODEL- M03-------------------############################


#Seleccionar los textos del macrocaso 03 (depurados individualmente:)
  
ruta03 <- "C:/Users/Desktop/Casos_JEP/caso03"
archivos <- list.files(ruta03, pattern = "\\.txt$", full.names = TRUE)
textos <- lapply(archivos, readLines, encoding = "UTF-8")
texto_unido <- unlist(textos)
texto_completo <- paste(texto_unido, collapse = " ")


# Crear corpus
corpus <- VCorpus(VectorSource(texto_completo))

# =------------------------------------------------------------
# Pre-procesamiento


corpus <- tm_map(corpus, content_transformer(tolower))       
corpus <- tm_map(corpus, removePunctuation)                  
corpus <- tm_map(corpus, removeNumbers)                      
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))  
corpus <- tm_map(corpus, stripWhitespace)                    

# Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# -----------------------------------------------------------
# Filtrat términos 

freq <- colSums(as.matrix(dtm))
umbral_inferior <- 3     # mínimo 3 apariciones
umbral_superior <- 0.9   # aparece en >90% documentos (no lo usamos todavía)

keep_terms <- names(freq)[freq >= umbral_inferior]
dtm_filtrado <- dtm[, keep_terms]
dtm_filtrado <- dtm_filtrado[rowSums(as.matrix(dtm_filtrado)) > 0, ]

# -----------------------------------------------------------
# Selección del número de tópicos 

valores_k <- 2:20
resultados_k <- FindTopicsNumber(
  dtm = dtm_filtrado,
  topics = valores_k,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 7)

FindTopicsNumber_plot(resultados_k)


# ------------------------------------------------------------
#  Ajustar un modelo LDA con el k elegido

k_optimo <- 4  # reemplazar conforme lo indican los gráficos
modelo_lda <- LDA(dtm_filtrado, k = k_optimo, method = "Gibbs",
                  control = list(seed = 123))
terms(modelo_lda, 8)
tidy_lda <- tidy(modelo_lda)

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_viridis_d(option = "cividis")
labs(title = "",
     x = "Término",
     y = "Probabilidad β")





