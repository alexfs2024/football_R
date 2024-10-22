# Establecer directorio de trabajo

getwd()
#Cambio directorio
setwd("C:/Users/alexf/Desktop/MASTER BIG DATA/PRE MASTER/R/MOD")
# Ver archivos
list.files()
# Punto 1 Lectura del fichero
df<- read.csv(file = "Understat_City_Chelsea.csv",sep = ";", header = TRUE,encoding = 'UTF-8')
df[c(1:10),]


# Punto 2 Descripción y Resumen estadístico
ncol(df)
cat("Numero de filas del dataframe:",nrow(df))
colnames(df) # columnas
cat("Número de columnas/atributos entre ambos equipos:", 
    ncol(df))
str(df)
length(df)

# Conocemos algunas conclusiones: xG vs Goles
df[df$team=="Manchester City",'xG']
df[df$team!="Manchester City",'xG']
cat("xG total del Manchester City:", 
    sum(df[df$team == "Manchester City", "xG"]))
cat("xG total del Atl?tico Madrid:", 
    sum(df[df$team != "Manchester City", "xG"]))
cat("NÚmero de goles:", 
    nrow(df[df$result == 'Goal',]))

#Punto 3 Cálculo del resultado más repetido
N <- 1000 # número de simulaciones
# Para cada simulación...
results_df <- data.frame()
for (j in 1:N){
  set.seed(j) # Semilla aleatoriedad
  result <- c(NA, NA) # Resultado en la simulación
  k <- 1
  # Para cada equipo...
  for (t in unique(df$team)){
    shots_team <- df[df$team == t, ]
    goals_t <- 0 # número goles del equipo (inicializamos en 0)
    # Simulamos cada disparo
    for (i in 1:nrow(shots_team)){
      goals_t <- goals_t + rbinom(n=1, size=1, prob=shots_team$xG[i])
    } # Sumamos los ?xitos = n? goles del equipo en la simulaci?n
    result[k] <- goals_t
    k <- k+1
  }
  # Concatenamos resultados
  results_df <- rbind(results_df, result)
  # Renombramos las columnas
  colnames(results_df) <- unique(df$team)
}
head(results_df)



## Calculamos el resultado de cada partido
results_df$result <- paste(as.character(results_df$`Manchester City`), 
                           "-", as.character(results_df$`Chelsea`))
## Victoria local, visitante o empate
results_df$win <- ifelse(
  results_df$`Manchester City` > results_df$`Chelsea`, "Manchester City", 
  ifelse(results_df$`Manchester City` < results_df$`Chelsea`, "Chelsea", 
         "Empate"))

#  Cuál hubiera sido el resultado más frecuente?
results_table <- sort(table(results_df$result), 
                      decreasing=TRUE)
results_table # número de veces que se repite cada resultado
pos_real_result <- which(names(results_table) == "2 - 0")
colors <- rep("gray", length(results_table))
colors[pos_real_result] <- "firebrick"
barplot(results_table, col = colors,
        main = "Resultado más frecuente tras 1000 simulaciones
        Manchester City 2-0 Chelsea", 
        ylab = "Partidos", xlab = "Resultado", las=2)


#  Porcentaje de victoria local, visitante o empate

win_proportions <- prop.table(table(results_df$win))
win_proportions # porcentaje de cada posible resultado
colors <- c("royalblue", "firebrick", "forestgreen")
teams <- names(win_proportions)
labels <- paste(teams, round(100*win_proportions, 2), "%")
pie(win_proportions, col = colors, labels = labels,
    main = "Proporción de victoria tras 1000 partidos simulados")
legend("bottomleft", names(win_proportions),
       fill = colors, pt.cex = 1, bty = 'n', cex = 0.7)

# Punto 4 Calcular los xPoints asociados a cada equipo.

CHL_xPTS <- as.character(win_proportions["Chelsea"]*3 + 
                           win_proportions["Empate"])
MNC_xPTS <- as.character(win_proportions["Manchester City"]*3 + 
                           win_proportions["Empate"])
cat(paste("xPoints. Manchester City", MNC_xPTS, "-", 
          CHL_xPTS, "Chelsea"))

