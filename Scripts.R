###se cargan los paquetes correspondientes al análisis
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(readr)
library(gridExtra)
library(grid)
###Se lee la base de datos y se importa como dataframe
StudentsPerformance <- read_csv("StudentsPerformance.csv")
##Con los siguientes comandos se seleccionarán las columnas de trabajo.

Students<-StudentsPerformance %>% 
  select("gender","reading score","writing score","math score","test preparation course",
         "parental level of education") %>% 
   rename(Genero=gender,Lectura="reading score", Escritura="writing score", 
          Preparacion="test preparation course", Nivel_Padres="parental level of education", 
          Matematicas="math score")
          
          
## plot para matematicas y lenguaje por genero
ggplot(Students, aes(Lectura)) + geom_histogram(binwidth=10, color="black", aes(fill=Genero))+
  geom_vline(aes(xintercept=mean(Lectura)), color="black", linetype="dashed", size=0.5) -> p1
 
                              
ggplot(Students, aes(Escritura)) + geom_histogram(binwidth=10, color="black", aes(fill=Genero))+
  geom_vline(aes(xintercept=mean(Escritura)), color="black", linetype="dashed", size=0.5) -> p2

ggplot(Students, aes(Matematicas)) + geom_histogram(binwidth=10, color="black", aes(fill=Genero))+
  geom_vline(aes(xintercept=mean(Escritura)), color="black", linetype="dashed", size=0.5) ->p3

## uso de grid arrange
grid.arrange(p1, p2, p3, ncol = 3)
