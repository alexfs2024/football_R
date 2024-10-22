 library(ggplot2) 
 library(readr) 
 library(gridExtra)
 library(ggforce)
 library(ggExtra)
 library("dplyr")
 library("tidyverse")
 install.packages("ggplot2")
 
 
#----------------------------------------------------
# Punto 2 : Representar un campo de fútbol, donde además se vean las porterias, los arcos correspondientes a los saques de córner
#y los arcos correspondientes al área.

#----------------------------------------------------
 grafico_campo_futbol <- ggplot() +
  
   guides(fill = guide_legend(title = NULL)) +
   
   #Campo y áreas.
   geom_rect(mapping = aes(xmin = 0.0, xmax = 596.97, 
                           ymin = 0.0, ymax = 396),
             color ="#00529f", fill = NA, alpha = 0.1) +
   geom_rect(mapping = aes(xmin = 0.0, xmax = 91.935, 
                           ymin = 78.475, ymax = 317.525), 
             color ="#00529f", fill = NA, alpha = 0.1) +
   geom_rect(mapping = aes(xmin = 505.035, xmax = 596.97, 
                           ymin = 78.475, ymax = 317.525), 
             color ="#00529f", fill = NA, alpha = 0.1) +
   geom_rect(mapping = aes(xmin = 0.0, xmax = 29.858, 
                           ymin = 144.5, ymax = 251,50), 
             color ="#00529f", fill = NA, alpha = 0.1) +
   geom_rect(mapping = aes(xmin = 567.112, xmax = 596.97, 
                           ymin = 144.5, ymax = 251,50), 
             color ="#00529f", fill = NA, alpha = 0.1) +
   # Línea de medio campo
   geom_linerange(aes(x = 298.485, ymin = 0, 
                      ymax = 396), 
                  color = "#00529f") +
   # Puntos de penalti y medio campo. 
   geom_circle(mapping = aes(x0 = 298.485, y0 = 198, r = 52), color = "#00529f") +
   geom_circle(mapping = aes(x0 = 530.64, y0 = 198, r = 1), color = "#00529f") +
   geom_circle(mapping = aes(x0 = 298.485, y0 = 198, r = 1), color = "#00529f") + #No se girarlo
   geom_circle(mapping = aes(x0 = 66.55, y0 = 198, r = 1), color = "#00529f") +
   # Arcos de las áreas
   geom_arc(mapping = aes(x0=66.55, y0=198,r=52,start=pi/6,end=5*pi/6),color = "#00529f")+
   geom_arc(mapping = aes(x0=530.54, y0=198,r=52,start= - pi/6,end= - 5/6*pi),color = "#00529f") + 
 
   # Arcos de los corners
   geom_arc(mapping = aes(x0=0, y0=0, r=52/9.15, start=0,end=pi/2),color= "#00529f") + 
   geom_arc(mapping = aes(x0=596.97, y0=0, r=52/9.15, start=0,end= - pi/2),color= "#00529f") +
   geom_arc(mapping = aes(x0=596.97, y0=396, r=52/9.15, start= -pi/2,end= -pi),color= "#00529f") +
   geom_arc(mapping = aes(x0=0, y0=396, r=52/9.15, start=pi/2,end=pi),color= "#00529f") +  
   # Líneas de portería
   geom_linerange(aes(x= 0,ymin=198-((107/18.32)*(7.32/2)),ymax=198+((107/18.32)*(7.32/2))),color= "#00529f",linewidth=2) +
   geom_linerange(aes(x= 596.97,ymin=198-((107/18.32)*(7.32/2)),ymax=198+((107/18.32)*(7.32/2))),color= "#00529f",linewidth=2) +
 
   coord_fixed() +
   theme_no_axes(base.theme = theme_bw()) +
   theme(legend.position = c(0.5, 0.04),
         legend.box = "horizontal",
         legend.direction = "horizontal",
         legend.box.background = element_rect(fill = "transparent",
                                              colour = "transparent"),
         legend.text = element_text(size = 14),
         panel.border = element_blank(),
         axis.title = element_blank(), 
         axis.text = element_blank(), 
         axis.ticks = element_blank()) +
         #  plot.margin=unit(c(-0.05,-0.05,-0.05,-0.1),"in")) +
   scale_x_continuous(limits = c(0,647.47), expand = c(0,0)) +
   scale_y_continuous(limits = c(0,497), expand = c(0,0)) 
 
grafico_campo_futbol
 
#--------------------------------
 # Identificar todos los pases de Marco Verrati
#--------------------------------
# Qué contiene cada columna.
unique(tarea$Suceso)
unique(tarea$Sección)
unique(tarea$Jugador)
unique(tarea$Equipo)

#Elegir jugador, rival y evento 
jugador_elegido <- c("Verratti")
rival_elegido <- c("Bayern")
evento_elegido <- c("Passes_All")

data_player1 <- subset(tarea,Jugador == jugador_elegido & Sección == evento_elegido )

#---------------------------------
# 3.1 Representar los pases mediante puntos en los orígenes.
#---------------------------------

plot_origenpasses <- grafico_campo_futbol +
  geom_point(data_player1,mapping = aes(x= x1Cor , y= y1Cor , shape= factor(Resultado)), size = 5, color = "red" ) +
  guides(none) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(3 , 6, 10, 1))

plot_origenpasses

#------------------------------
# 3.2 Convex Hull para todos los eventos correspondientes
#------------------------------
list_jugadores <- c("Verratti")
list_variosjugadores <- c("Kimpembe","Nuno Mendes","Hakimi","Ramos","Verratti")	
data_player_full <- tarea %>%
 filter(Equipo %in% "Paris SG", Sección %in%"Passes_All", Jugador %in% list_jugadores)
indices_vertices_poligono <- chull(data_player_full$x1Cor,data_player_full$y1Cor)
vertices_poligono2 <- data_player_full %>%
  group_by(Jugador) %>%
  slice(chull(x1Cor,y1Cor))

vertices_poligono1 <- indices_vertices_poligono

plot_convexhull <- plot_origenpasses +
  geom_polygon(vertices_poligono2, mapping=aes(x=x1Cor,y=y1Cor, color=Jugador, fill=Jugador),alpha=0.2, show.legend = FALSE)+
  theme(legend.box = "vertical",legend.position = "bottom")+
  guides(color=guide_legend(order=1),shape=guide_legend(order=2))

#--------------------------------
# 4.1 Diagrama marginal 
#--------------------------------

ggExtra::ggMarginal(plot_origenpasses,type="violin")


