#####################################################################################
#-------------- Aplicación modelo de sociabilidad - Macrocaso 02--------------------
#####################################################################################


setwd("C:/Users/yulis/OneDrive/Desktop/Casos_JEP/sociality")
rm(list = ls())

#install.packages("stringr")
#install.packages("RColorBrewer")
suppressMessages(suppressWarnings(library(igraph)))
suppressMessages(suppressWarnings(library(sand)))
suppressMessages(suppressWarnings(library(coda)))
suppressMessages(suppressWarnings(library(corrplot)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(gridExtra)))
suppressMessages(suppressWarnings(library(cluster)))
suppressMessages(suppressWarnings(library(mclust)))
suppressMessages(suppressWarnings(library(truncnorm)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(igraph)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(ggplot2)))

## Funciones (modelo de sociabilidad:)

source("VI.R")

# Data Macrocaso 02 
case <- as.matrix(read.csv("C:/Users/yulis/OneDrive/Desktop/Casos_JEP/adj_matrix_22.csv", 
                           row.names = 1, 
                           check.names = FALSE))

G <- graph_from_adjacency_matrix(case, mode = "undirected", diag = FALSE)
n=nrow(case)
y <- as.matrix(as_adjacency_matrix(G, sparse = FALSE))
grado <- degree(G)


### Parámetros del modelo con base en el Paper original:
a_sigma <- 2 
b_sigma <- 1/3
a_tau   <- 2 
b_tau   <- 1/3

n_iter  <- 250000 + 10000
n_burn  <- 10000
n_thin  <- 10
global_bound <- 3
epsilon <- 1e-06
max_iter <- 1000


# Métricas observadas de la red:

metricas <- data.frame(
  Métrica = c("Nodos", "Aristas", "Densidad", "Diámetro", "Distancia media",
              "Transitividad", "Asortatividad", "Grado medio", "SD Grado"),
  Valor = c(
    vcount(G),
    ecount(G),
    edge_density(G),
    diameter(G),
    mean_distance(G, directed = FALSE),
    transitivity(G, type = "global"),
    assortativity_degree(G, directed = FALSE),
    mean(degree(G)),
    sd(degree(G))
  )
)
print(metricas)

## Centralidad
(betweenness_centrality <- betweenness(G))
(closeness_centrality   <- closeness(G))
(eigenvector_centrality <- eigen_centrality(G)$vector)

## Identificación de comunidades (clustering)
set.seed(123)
communities <- cluster_fast_greedy(G)
(num_communities <- length(communities))
(modularity_value <- modularity(communities))

#------ Ajustar el modelo mediante inferencia variacional (VI)------------------

######################### Implementación algoritmo ##############################

start.time <- Sys.time()
variational <- vi_sociality(y, a_sigma, b_sigma, a_tau, b_tau, global_bound, epsilon, max_iter)
end.time <- Sys.time()
end.time - start.time


####################### Inferencia sobre los parámetros #######################

# Inferencia sobre mu --------------

mu_mean_vi <- variational$mu_mu  
mu_sd_vi <- sqrt(variational$sigma_mu2)

## Calcular la densidad Variac.-----
x_vals <- seq(-1.2, -0.6, length.out = 1000)
vi_density <- dnorm(x_vals, mean = mu_mean_vi, sd = mu_sd_vi)
vi_df <- data.frame(x = x_vals, y = vi_density, Type = "VI")
combined_df <- rbind(vi_df)


##  Grafico dis post.

pdf(file = "M02_posterior_mu.pdf")
ggplot(combined_df, aes(x = x, y = y, color = Type, linetype = Type)) +
     geom_line(size = 0.5) +
     scale_color_manual(values = c("blue", "red")) +
     scale_linetype_manual(values = c("solid", "solid")) +
     xlim(-1.2, -0.6) +
     labs(
          title = "",
          x = expression(mu),
          y = "Densidad"
     ) +
     theme_minimal(base_size = 30) +
     theme(
          legend.position = c(1, 1),
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          legend.background = element_blank()
     )
dev.off()

# Inferencia sobre sigma^2 ---------------------------------------

## Calcular la densidad variacional (aproximación gamma inversa)
x_vals <- seq(0, 2, length.out = 1000)
vi_density <- invgamma::dinvgamma(x_vals, shape = variational$alpha_sigma, rate = variational$beta_sigma)
vi_df <- data.frame(x = x_vals, y = vi_density, Type = "VI")

combined_df <- rbind(vi_df)

## Grafico dist. posterior.

pdf(file = "M02_posterior_sigma2.pdf")
ggplot(combined_df, aes(x = x, y = y, color = Type, linetype = Type)) +
     geom_line(size = 0.5) +
     scale_color_manual(values = c("blue", "red")) +
     scale_linetype_manual(values = c("solid", "solid")) +
     xlim(0, 2) +
     labs(
          title = "",
          x = expression(sigma^2),
          y = "Densidad"
     ) +
     theme_minimal(base_size = 30) +
     theme(
       legend.position = "none"
     )
dev.off()


# Inferencia sobre Tau^2---------------------------------

##Calcular la densidad variacional (aproximación gamma inversa)
x_vals <- seq(0, 0.5, length.out = 1000)
vi_density <- invgamma::dinvgamma(x_vals, shape = variational$alpha_tau, rate = variational$beta_tau)
vi_df <- data.frame(x = x_vals, y = vi_density, Type = "VI")

combined_df <- rbind(vi_df)

#Graf distr. post.

pdf(file = "M02_posterior_tau2.pdf")
ggplot(combined_df, aes(x = x, y = y, color = Type, linetype = Type)) +
     geom_line(size = 0.5) +
     scale_color_manual(values = c("blue", "red")) +
     scale_linetype_manual(values = c("solid", "solid")) +
     xlim(0, 0.5) +
     labs(
          title = "",
          x = expression(tau^2),
          y = "Densidad"
     ) +
     theme_minimal(base_size = 30) +
     theme(
       legend.position = "none"
     )
dev.off()

# Inferencia sobre mu, sigma^2 and tau^2 -------------------------------------------
vi_mu_mean <- variational$mu_mu
vi_mu_sd <- sqrt(variational$sigma_mu2)
vi_mu_ci <- qnorm(c(0.025, 0.975), mean = vi_mu_mean, sd = vi_mu_sd)

vi_sigma2_mean <- variational$beta_sigma / (variational$alpha_sigma - 1) # E[X] = beta / (alpha - 1) for alpha > 1
vi_sigma2_sd <- sqrt(variational$beta_sigma^2 / ((variational$alpha_sigma - 1)^2 * (variational$alpha_sigma - 2))) # Var[X] = beta^2 / ((alpha - 1)^2 * (alpha - 2)), for alpha > 2
vi_sigma2_ci <- invgamma::qinvgamma(c(0.025, 0.975), shape = variational$alpha_sigma, rate = variational$beta_sigma)

vi_tau2_mean <- variational$beta_tau / (variational$alpha_tau - 1) # E[X] = beta / (alpha - 1) for alpha > 1
vi_tau2_sd <- sqrt(variational$beta_tau^2 / ((variational$alpha_tau - 1)^2 * (variational$alpha_tau - 2))) # Var[X] = beta^2 / ((alpha - 1)^2 * (alpha - 2)), for alpha > 2
vi_tau2_ci <- invgamma::qinvgamma(c(0.025, 0.975), shape = variational$alpha_tau, rate = variational$beta_tau)

##Tabla resumen
vi_table <- data.frame(
     Parameter = c("mu", "sigma^2", "tau^2"),
     Mean = c(vi_mu_mean, vi_sigma2_mean, vi_tau2_mean),
     SD = c(vi_mu_sd, vi_sigma2_sd, vi_tau2_sd),
     CI95_Lower = c(vi_mu_ci[1], vi_sigma2_ci[1], vi_tau2_ci[1]),
     CI95_Upper = c(vi_mu_ci[2], vi_sigma2_ci[2], vi_tau2_ci[2]))

cat("\nVI Table:\n")
print(vi_table)



# Inferencia sobre delta  --------------------------------------------------

## Extraer los delta estimados:
delta_mean_vi <- variational$mu_delta  #Media posterior variacional de delta
delta_sd_vi <- sqrt(variational$sigma_delta2)  #Desviaciones estándar posteriores variacionales
delta_ci95_vi <- t(apply(cbind(delta_mean_vi, delta_sd_vi), 1, function(x) {
  c(Lower = x[1] - 1.96 * x[2], Upper = x[1] + 1.96 * x[2])
}))

##  Crear un dataframe con los estimados mediante VI
node_names <- rownames(y)

node_names <- rownames(y)
delta_df_vi <- data.frame(
  Node = node_names,
  Delta_Est = delta_mean_vi,
  CI95_Lower = delta_ci95_vi[, 1],
  CI95_Upper = delta_ci95_vi[, 2]
)

delta_df_vi <- delta_df_vi[order(delta_df_vi$Delta_Est), ]
delta_df_vi$Order <- 1:length(delta_mean_vi)

## Identificar los intervalor que contienen al cero, superiores e inferiores a este
delta_df_vi$IntervalType <- ifelse(
  delta_df_vi$CI95_Lower > 0, "Above 0",
  ifelse(delta_df_vi$CI95_Upper < 0, "Below 0", "Contains 0"))


#Gráfico IC------------------------------------------------       
#Graficar los intervalos de credibilidad (dado que es una red semántica este
#Este gráfico está muy saturado)

pdf(file = "M02_posterior_delta_vi.pdf")
ggplot(delta_df_vi, aes(x = Order)) +
  # Intervalos de credibilidad 95%
  geom_segment(aes(
    x = Order, xend = Order,
    y = CI95_Lower, yend = CI95_Upper,
    color = IntervalType
  ), linewidth = 0.8) +
  
  geom_segment(aes(
    x = Order - 0.2, xend = Order + 0.2, y = CI95_Lower, yend = CI95_Lower,
    color = IntervalType
  ), linewidth = 0.8) +
  
  geom_segment(aes(
    x = Order - 0.2, xend = Order + 0.2, y = CI95_Upper, yend = CI95_Upper,
    color = IntervalType
  ), linewidth = 0.8) +
  
  # Graficar por color el tipo de intervalo:
  geom_point(aes(y = Delta_Est, color = IntervalType), size = 2) +
  scale_color_manual(values = c("Above 0" = "green", "Below 0" = "red", "Contains 0" = "gray70")) +
  labs(
    title = "",
    x = NULL,
    y = expression(delta)
  ) +
  ylim(-1.25, 1.25) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()


#-------------- Clustering empleando loe efectos individuales (delta VI) -------------------

## Extraer los valores:
mu_delta <- variational$mu_delta

## Determinar el número óptimo de clusteres por el método del codo
wss <- numeric(10)
for (k in 1:10) {
  kmeans_result <- kmeans(mu_delta, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

## Seleccionar el número óptimo de clusteres:
optimal_k <- which(diff(diff(wss)) == min(diff(diff(wss)))) + 1

## algoritmo K-means sobre los delta estimados:
kmeans_final <- kmeans(mu_delta, centers = optimal_k, nstart = 25)
clusters <- kmeans_final$cluster
clusters_vi <- clusters

## Extract VI estimates for delta
delta_mean_vi <- variational$mu_delta  # Variational posterior mean of delta
delta_sd_vi <- sqrt(variational$sigma_delta2)  # Variational posterior standard deviations
delta_ci95_vi <- t(apply(cbind(delta_mean_vi, delta_sd_vi), 1, function(x) {
  c(Lower = x[1] - 1.96 * x[2], Upper = x[1] + 1.96 * x[2])
}))

## Crear un data frame que incluya los intervalos de credibilidad y el clúster
## al que se asignó cada nodo con base en k-means.
delta_df_vi <- data.frame(
  node_names <- rownames(y), 
  Delta_Est = delta_mean_vi,
  CI95_Lower = delta_ci95_vi[, 1],
  CI95_Upper = delta_ci95_vi[, 2],
  Cluster = clusters_vi  
)

delta_df_vi$IntervalType <- ifelse(
  delta_df_vi$CI95_Lower > 0, "Above 0",
  ifelse(delta_df_vi$CI95_Upper < 0, "Below 0", "Contains 0")
)

## Ordenar por la media posterior
delta_df_vi <- delta_df_vi[order(delta_df_vi$Delta_Est), ]
delta_df_vi$Order <- 1:length(delta_mean_vi)
cluster_colors <- scales::hue_pal()(length(unique(delta_df_vi$Cluster)))


####Guardar el archivo para graficar en python:
write.csv(delta_df_vi,
          file = "C:/Users/yulis/OneDrive/Desktop/Casos_JEP/delta_vi.csv",
          row.names = FALSE)










