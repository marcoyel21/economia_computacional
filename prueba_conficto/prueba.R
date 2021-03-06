# Script de prueba para probar soluci�n de conflictos en Github


# Carga las librerias
library(dplyr)
library(tidyr)
library(stargazer)
library(kableExtra)


# Carga la base de datos
base<- mtcars

# Guarda las columnas
columnas <- colnames(base) %>% as.data.frame()

# Crea una tabla
kable(columnas, booktabs=T, align = 'c', col.names = c("Columnas"), longtable=T) %>%
  kable_styling(position = "left")

# Algunos datos de interes
library(dagitty)

diagram <-dagitty('dag{
"Tech Frontier in the US" [pos="1,2"]
"Close contact with US" [pos="1,1"]
"Cell phone adoption" [pos="1,0"]
"Tech Frontier in the US"->"Close contact with US"->"Cell phone adoption"
}')

library(ggdag)
ggdag_classic(diagram,
              size=4)+
  theme_dag_gray()


diagrama_rafa <-dagitty('dag{
"Tech Frontera in the US" [pos="1,2"]
"Close contact with America" [pos="1,1"]
"Cell phone adoption" [pos="1,0"]
"Tech Frontier in the US"->"Close contact with US"->"Cell phone adoption"
}')

library(ggdag)
ggdag_classic(diagram,
              size=4)+
  theme_dag_gray()

  # lineas nuevas
  a <- c("uno","dos","tres")
  b <- c(1,2,3)
  df <- data.frame(a,b)
