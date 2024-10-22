#  MUESTRA DE ESTUDIO: ANÁLISIS DE FACTORES DE ATAQUE ENTRE LOS CENTRO CAMPISTAS CON MÁS DE 1000´EN EL CAMPEONATO DE LIGA.


# Librerias de interés
library(dplyr)
library(stats)
library(graphics)
library(ggplot2)
library(GGally)
library(Hmisc)
library(cluster)
library(purrr)
library(factoextra)
library(dplyr)
library(knitr)


# LECTURA y ANÁLISIS EXPLORATORIO DE DATOS

getwd()
setwd("C:/Users/alexf/Desktop/MASTER BIG DATA/PRE MASTER/R/MOD")
df<- read.csv(file= 'FBREF_players.csv', sep=";", header=TRUE,encoding='UTF-8')
head(df[,c(1:12)]) 
data_LaLiga <- df[df$Competition == 'La Liga',]
cat("Número de jugadores de La Liga:", nrow(data_LaLiga))
data_filter_LaLiga <- data_LaLiga[data_LaLiga$Min >= 1000,]
cat("Número de jugadores que hayan jugado más de 1000 minutos:", 
    nrow(data_filter_LaLiga))
##Selección de centro campistas
positions <- unique(data_filter_LaLiga$Pos)
data_LaLiga_DF <- data_filter_LaLiga %>% 
  filter(grepl("MF", Pos))

cat("NÚmero de CENTROCAMPISTAS que han jugado más de mil minutos de La Liga:", nrow(data_LaLiga_DF))
head(data_LaLiga_DF[,c(1:12)])
# Seleccionamos los atributos que nos interesan
cols <- c("Player", "MP", "Min", "Passes.","Gls","Ast","Sh","Sh.90","SoT.90", 
             "PassesCompleted.90", "LongPasses.", "LongPassesCompleted.90", "ShortPasses.","MediumPasses.","LongPasses.",
             "PassesProgressive.90","PassesAttempted.90","ShortPassesCompleted.90", "MediumPassesCompleted.90","TotDistPasses.90",
             "FinalThirdPasses.90")
#Creamos una nueva variable
data_DF <- data_LaLiga_DF %>%
  mutate("xA+xG/90" = `xG.90` + `xA.90`) %>%
  select(c(all_of(cols), "xA+xG/90")) %>%
  select(-c("MP","Min")) %>%
  rename(`Passes%` = "Passes.", 
         `LongPasses%` = "LongPasses.")
tail(data_DF)


sapply(data_DF,typeof)
rownames(data_DF)<-data_DF$Player
data_DF <- data_DF %>% select(-Player)
cat("Número de jugadores:",nrow(data_DF))
head(data_DF)
########################## Descripción de algunas variables numéricas
summary(data_DF[,c(1:5)])

## Estandarización de las variables
df_teams_norm <- scale(data_DF, center = TRUE, scale = TRUE)



## Comprobación:
N <- ncol(df_teams_norm)
media = rep(NA, N)
desv.estandar = rep(NA, N)
for (i in 1:N) {
  mean_i = mean(df_teams_norm[,i])
  sd_i = sd(df_teams_norm[,i])
  media[i] = mean_i
  desv.estandar[i] = sd_i
}

media # vector igual a 0
desv.estandar # vector igual a 1


## Valores atípicos

### El análisis cluster es muy sensible a la inclusión de valores
### atípicos ya que pueden distorsionar la verdadera estructura de 
### la población

par(mfrow=(c(3, 4)))
for (i in 1:12){
  plot_variable <- boxplot(
    df_teams_norm[,i], col = "firebrick",
    main = paste("Gráfico Q-Q \n", colnames(df_teams_norm)[i]))
}
par(mfrow=c(1,1))


# 3. Evaluación de la tendencia de agrupamiento

## ¿Son los datos agrupables?

get_clust_tendency(
  df_teams_norm, n = 18,
  gradient = list(low = "steelblue", high = "white"))



#Análisis de Clusterización
## 4.1 Cálculo matriz de distancia: euclidea
dist_eucl <- dist(df_teams_norm, method="euclidean")

## 4.1 Cálculo de conglomerados
hc_ward_eucl <- hclust(dist_eucl, method = "ward.D2")
hc_single_eucl <- hclust(dist_eucl, method ="single")
hc_complete_eucl <- hclust(dist_eucl, method ="complete")
hc_average_eucl <- hclust(dist_eucl, method ="average")

## 4.2 Evaluación del método de clusterización
ew <- cor(dist_eucl, cophenetic(hc_ward_eucl))
es <- cor(dist_eucl, cophenetic(hc_single_eucl))
ec <- cor(dist_eucl, cophenetic(hc_complete_eucl))
ea <- cor(dist_eucl, cophenetic(hc_average_eucl))
resumen <- rbind(ew, es, ec, ea)
row.names(resumen) <- c("Ward Euclidean", "Single Euclidean",
                        "Complete Euclidean", "Average Euclidean")
kable(resumen, align='c', 
      col.names = c("Distancias cophenetic"))


## 4.3 Representación dendograma
### Usamos el método Single Euclidean

### ¿Cuáles son las primeras uniones? ?A qué distancias?
cbind(hc_average_eucl$merge, hc_average_eucl$height)

### Representación
# merge: unión entre los conglomerados (usando la union media)
# height: alturas en las que se van realizando las uniones entre observaciones

plot(hc_average_eucl, col="firebrick",
     main="Dendograma Centrocampistas")


## 4.3 Dendograma con diferentes clusters. Por ejemplo: 3 clusters
set.seed(123)
fviz_dend(x = hc_average_eucl, k=3, cex = 0.8) +
  geom_hline(yintercept = 6.1, linetype = "dashed")

### M?s visualizaciones. Usando PCA
install.packages("ggplot2")
library(ggplot2)
fviz_cluster(
  object = list(
    data = df_teams_norm, cluster = cutree(hc_average_eucl, k = 3)),
  repel = TRUE, show.clust.cent = FALSE) +
  theme_bw() + 
  labs(title = "Agrupaci?n jer?rquica - 3 clusters")

### ?Y si quisieramos 5 clusters?
### M?s visualizaciones. Usando PCA
fviz_cluster(
  object = list(
    data = df_teams_norm, cluster = cutree(hc_average_eucl, k = 5)),
  repel = TRUE, show.clust.cent = FALSE) +
  theme_bw() + 
  labs(title = "Agrupaci?n jer?rquica - 5 clusters")


## 4.4 ?C?mo de bien est?n clasificadas las observaciones?
### Usaremos el indicador de la silueta.

sil_3clusters <- eclust(
  data_DF, "hclust", k = 3, hc_metric = "euclidean",
  hc_method = "average", graph = FALSE)
fviz_silhouette(sil_3clusters)

sil_5clusters <- eclust(
  data_DF, "hclust", k = 5, hc_metric = "euclidean",
  hc_method = "average", graph = FALSE)
fviz_silhouette(sil_5clusters)


# 5. M?todos no jer?rquicos (o de partici?n). K medias
## 5.1 Determinar el valor de K. Regla del codo
fviz_nbclust(
  x = df_teams_norm, FUNcluster = kmeans, method = "wss",
  diss = dist_eucl) +
  labs(title = "Elecci?n ?ptima de n?mero de clusters - Elbow Method")

### Atendido a la regla del codo: K = 4


## 5.2 Ejecuci?n 4-Medias
set.seed(123) # fijamos una semilla
km_clusters4 <- kmeans(df_teams_norm, centers = 4)


### Algunos elementos importantes:
km_clusters4$centers # valores de los centroides de cada grupo
km_clusters4$size # n? de individuos (equipos) en cada cluster
km_clusters4$cluster # ?a qu? cluster pertenece cada equipo?


## 5.3 Visualizaci?n

### Haciendo uso de PCA
fviz_cluster(
  object = km_clusters4, data = df_teams_norm, 
  show.clust.cent = TRUE, star.plot = TRUE, repel = TRUE) + 
  theme_bw() +
  labs(title = "Agrupaci?n K-Medias - 4 conglomerados")

### Tambi?n podemos seleccionar nosotros las variables
library(ggrepel)

data_plot <- cbind(data_DF[,c("Gls","Ast")], 
                   as.data.frame(km_clusters4$cluster))
colnames(data_plot) <- c("Gls","Ast")
data_plot <- cbind(Equipo = rownames(df_teams_OF), data_plot)
data_plot$Cluster <- as.factor(data_plot$Cluster)
ggplot(data_plot, aes(x="Gls", y="Ast", 
                      color = Cluster)) +
  geom_point(size=2) +
  geom_label_repel(aes(label = Equipo),
                   box.padding   = 0.2, 
                   point.padding = 0.3,
                   segment.color = 'grey50') +
  xlab("Goles (- goles penalty) por 90 minutos") +
  ylab("Goles - Goles esperados") +
  ggtitle("Eficacia goles y asistencias") +
  theme_classic()

## 5.4 Evaluaci?n de las particiones
res_kmeans4 <- eclust(
  df_teams_norm, FUNcluster="kmeans", k = 4, graph = FALSE)
fviz_silhouette(res_kmeans4)


# Accedemos a la silueta individual
silueta_individual <- res_kmeans4$silinfo$widths
silueta_individual[silueta_individual$cluster == 1,]

