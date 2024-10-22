#  MUESTRA DE ESTUDIO: ANÁLISIS DE FACTORES DE ATAQUE ENTRE LOS CENTRO CAMPISTAS CON MÁS DE 1000´EN EL CAMPEONATO DE LIGA.


# Librerias de interés
library(dplyr)
library(stats)
library(graphics)
library(ggplot2)
library(GGally)

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
metrics <- c("Player", "Squad", "MP", "Min", "Passes.","Gls","Ast","Sh","Sh.90","SoT.90", 
             "PassesCompleted.90", "LongPasses.", "LongPassesCompleted.90", "ShortPasses.","MediumPasses.","LongPasses.",
             "PassesProgressive.90","PassesAttempted.90","ShortPassesCompleted.90", "MediumPassesCompleted.90","TotDistPasses.90",
             "FinalThirdPasses.90")
#Creamos una nueva variable
data_DF <- data_LaLiga_DF %>%
  mutate("xA+xG/90" = `xG.90` + `xA.90`) %>%
  select(c(all_of(metrics), "xA+xG/90")) %>%
  select(-c("MP","Min")) %>%
  rename(`Passes%` = "Passes.", 
         `LongPasses%` = "LongPasses.")
tail(data_DF)

#Descripción de los datos.
summary(data_DF[,c(4:length(data_DF))])

data_DF %>%
  select(`Passes%`, `Gls`, `Ast`, `Sh`, 
         `SoT.90`,) %>%
  ggpairs(diag = list(continuous ='barDiag'), 
          title = "Relación entre las variables numéricas")
#Existencia de valores Nan
cat("Número de registros completos:", sum(complete.cases(data_DF)))
NaN_values_cols <- rep(NaN, length(data_DF))
for (c in 1:length(data_DF)){
  NaN_values_cols[c] <- sum(is.na(data_DF[,c]))}
cat("N?mero de registros NaN:", sum(NaN_values_cols))
df_NaN <- data.frame("NaN_values" = c(NaN_values_cols))
rownames(df_NaN) <- colnames(data_DF)
df_NaN

#Estudio de distribución de variables
plot.new() # inicializamos el gr?fico
data_plot <- data_DF[, c(3:19)]
par(mfrow = c(3,3))
rename <- c("% Pases", "Goles", "Asistencias","Disparos","Disparos en 90",
            "Disparos a puerta", "Pases completados por 90'", "%Pases en largo",
            "Pases en largo completados en 90´", "Pases en Corto", "Pases medios",
            "Pases de progresion", "Pases conseguidos","Pases cortos completados en 90´", 
            "Pases medios completados en 90´","Total Pasess  90´", "Entre los 3 pases finales")
for (i in 1:length(data_plot)) { # algunas variables
  x <- data_plot[,i]
  h<-hist(x, main = "",
          xlab = rename[i], 
          ylab = "Frecuencia",
          col = "darkgreen")
}
mtext("Histograma de las variables de analisis",
      outer=TRUE,  cex=1.2, font = 2, line=-2)

# Identificación de valores outliers
## Realizamos un gráfico boxplot para algunas de las medidas de análisis
outliers_metrics <- c("Player", "Squad", "PassesCompleted.90", 
                      "Ast", 
                      "SoT.90", "FinalThirdPasses.90")
rename_metrics <- c("Jugador", "Equipo", "Pases completados por 90'", 
                    "Asistencias", 
                    "Tiros a portería", "Últimos 3 pases")
                    
data_boxplot <- data_DF[,c(outliers_metrics)]

plot.new()
par(mfrow = c(2,3))
for (c in 3:length(data_boxplot)){
  boxplot(x = data_boxplot[,c], 
          main = rename_metrics[c], 
          col = "#4271AE")
}
mtext("Identificación de outliers",
      outer=TRUE,  cex=1.2, font = 2, line=-2)

## Analizamos la variable Pases completados en 90 '
ast_df <- data_DF[, c("Player", "Squad", "PassesCompleted.90")]

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))}
plot_boxplot <- function(df, color, title){
  df %>%
    mutate(outlier = ifelse(is_outlier(`PassesCompleted.90`), Player, as.numeric(NA))) %>%
    ggplot(., aes(x = "", label = Player, y = `PassesCompleted.90`)) +
    stat_boxplot(geom = "errorbar",      # Bigotes
                 width = 0.2) +
    geom_boxplot(fill = color,       # Color de la caja
                 outlier.colour = "red", # Color de los valores at?picos
                 alpha = 0.9) +          # Transparencia del color de la caja
    geom_text(aes(label = outlier), na.rm = TRUE, vjust = -0.7, size = 3) +
    ggtitle(title) + # T?tulo del plot
    ylab("Pases completados") +   # Etiqueta del eje x
    xlab("") +
    coord_flip() # Boxplot horizontal
}  
plot_boxplot(ast_df, "#4271AE", 
             "Centro campistas de La Liga y sus pases completados")
#Análisis por equipos 
ggplot(data = ast_df, aes(x = factor(Squad), y = `PassesCompleted.90`)) +
  stat_boxplot(geom = "errorbar", # Bigotes
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colores
               alpha = 0.9, outlier.colour = "red") +
  # Etiqueta de la variable continua
  scale_y_continuous(name = "Pases completados") +
  scale_x_discrete(name = "Equipos") +  # Etiqueta de los grupos
  # T?tulo del plot
  ggtitle("Análisis de PASES COMPLETADOS de los centrocampistas de cada equipo") +
  theme(axis.line = element_line(colour = "black", # Personalizaci?n del tema
                                 size = 0.25), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##4 . Análisis te componentes principales.

#Análisis descriptivo
data_LaLiga_DF$Competition <- as.factor(data_LaLiga_DF$Competition)
variables <- c("Passes.","Gls","Ast","Sh","Sh.90","SoT.90", 
               "PassesCompleted.90", "LongPasses.", "LongPassesCompleted.90", "ShortPasses.","MediumPasses.","LongPasses.",
               "PassesProgressive.90","PassesAttempted.90","ShortPassesCompleted.90", "MediumPassesCompleted.90","TotDistPasses.90",
               "FinalThirdPasses.90")
colores = c("indianred", "royalblue", "gold", "forestgreen", "grey")[data_LaLiga_DF$Competition]
pairs(data_LaLiga_DF[,variables], col = colores, upper.panel = NULL, pch = 15, cex = 0.6)
legend("topright", bty = "n", pch = 16, 
       col = c("indianred", "royalblue", "gold", "forestgreen", "grey"), 
       legend = levels(data_LaLiga_DF$Competition),
       xpd = TRUE, cex = 1)

##Matriz de correlaciones
library(corrplot)

data_LaLiga_numeric <- data_LaLiga_DF[, variables]

# Asegurarse de que todas las columnas sean numéricas
data_LaLiga_numeric <- data.frame(lapply(data_LaLiga_numeric, function(x) as.numeric(as.character(x))))

# Verificar si hay NA introducidos por la conversión
na_counts <- sapply(data_LaLiga_numeric, function(x) sum(is.na(x)))
print(na_counts)

# Si hay filas con NA, eliminarlas
data_LaLiga_numeric <- data_LaLiga_numeric[complete.cases(data_LaLiga_numeric), ]

# Calcular la matriz de correlación usando solo las filas completas
R <- cor(data_LaLiga_numeric)

corrplot(R, method = "color", tl.cex = 0.45)

# Determinante de la matriz de correlaciones
cat("Det(R):", det(R))

# Autovalores y autovectores
(autoval = eigen(R)$values)
autovec = eigen(R)$vectors # no display

plot(autoval, main = "Gráfico de sedimentación", 
     ylab = "Autovalor", xlab = "Componentes Principales", 
     type = "h", col = "firebrick")

#ACP usando prcomp
library(factoextra)
acp <- prcomp(data_LaLiga_numeric, scale=TRUE)
str(acp)
summary(acp)

## 4.1 Funci?n que obtiene resumen de variabilidad
prop_variance <- function(acp, max_pct){
  resumen <- matrix(NA, nrow = length(acp$sdev), ncol=3)
  resumen[,1] <- acp$sdev^2 # autovalores
  # pct variabilidad por cada CP
  resumen[,2] <- 100*resumen[,1]/sum(resumen[,1])
  resumen[,3] <- cumsum(resumen[,2])/100
  colnames(resumen) <- c("Autovalor", "Porcentaje", 
                         "Porcentaje acumulado")
  n <- length(resumen[,3][resumen[,3] < max_pct])+1
  cat("El n?mero ?ptimo de CPs es:", n)
  return (resumen[1:n,])
}

n_opt_CP <- prop_variance(acp, 0.9)
n_opt_CP

#Visualización
plot_prop_variance <- function(autoval, n_opt_CP, max_pct){
  plot(cumsum(autoval)/sum(autoval), type="s", 
       ylab="Porcenaje acumulado", xlab="Componentes principales", 
       main="Selecci?n de componentes principales")
  abline(h=max_pct, col="indianred", lty=2)
  nopt<-which(100*cumsum(autoval)/sum(autoval)>100*max_pct)[1]
  points(nopt, n_opt_CP[nopt,3], pch=19, col="firebrick")
  text(nopt-2.5, max_pct+0.03, 
       labels = paste0("(", nopt, ", ", round(n_opt_CP[nopt,3], 2), ")"), 
       font = 2, cex = 0.75)
}

plot_prop_variance(autoval, n_opt_CP, 0.9)

##Autovalores superiores a la media
cat("Media de los autovalores:", mean(autoval))
nopt <- length(which(autoval > mean(autoval)))
colors_opt <- rep("royalblue", length(autoval))
colors_opt[nopt] <- "indianred"
plot.new()
plot(autoval, type="h", ylab = "Autovalores", 
     xlab="Componentes principales",
     main="Autovalores superiores a la media", 
     col = colors_opt)
abline(h=mean(autoval),col="firebrick",lty=2)
text(nopt+3, autoval[nopt]+1.5, 
     paste0("(", nopt, ", ", round(autoval[nopt], 3), ")"), 
     cex = 0.75)

## Visualización
fviz_eig(acp)

#Coeficientes y correlaciones de las CP

##Coeficientes
n_opt <- 10
coeficientes <- acp$rotation[,1:n_opt]

### Mostramos los coeficientes de algunas variables sobre las primeras CPs
coeficientes[1:15,1:5]

## 5.2 Correlaciones
correlaciones <- acp$rotation %*% diag(acp$sdev)

### Alguna muestra
correlaciones[1:15,1:5]

corrplot(t(correlaciones[,1:n_opt]), 
         method="color", tl.cex = 0.45)

### Representaci?n gr?fica sobre las 2 CPs
plot(correlaciones[,1:2],
     main="Correlaciones entre variables y componentes principales",
     xlab="CP 1", ylab="CP 2",type="n")
text(correlaciones[,1:2],
     labels=rownames(correlaciones),
     col="firebrick", cex = 0.7)
grid()
abline(h=0,v=0,lty=3)

### Otros gr?ficos
fviz_pca_var(acp,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
plot.new()

# Cálculo de puntaciones
scores <- as.data.frame(acp$x[,1:nopt])
new_data <- cbind(data_LaLiga_DF$Squad, scores)
colnames(new_data) <- c("Equipo", paste0("CP", 1:nopt))
cat("Nuevas dimensiones:", dim(new_data))

head(new_data)

## Representaci?n sobre las 2 CP
plot(new_data[,2:3], type="n", main="Equipos de las 5 grandes ligas",
     xlab=paste0("CP1 (", round(n_opt_CP[1,2], 2), "%)"),
     ylab=paste0("CP2 (", round(n_opt_CP[2,2], 2), "%)"))
text(new_data[,2:3],labels=new_data$Equipo, cex=0.6, col="firebrick")
abline(h=0,v=0,lty=2,col="royalblue")
grid()

## Otros gr?ficos
fviz_pca_ind(acp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## Boxplot
plot.new() # inicializamos gr?fico
boxplot(scores, col = "indianred")
title("Distribuci?n de las puntuaciones \nen las componentes principales")
