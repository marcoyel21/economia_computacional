# Script de prueba para probar solución de conflictos en Github


# Carga las librerias
library(dplyr)
library(broom)
library(stargazer)
library(kableExtra)


# Carga la base de datos
base<- mtcars

# Guarda las columnas
columnas <- (as.data.frame(colnames(base)))

# Crea una tabla
kable(columnas, booktabs=T, align = 'c', col.names = c("Columnas"), longtable=T) %>%
  kable_styling(position = "center",latex_options = "repeat_header")

# Algunos datos de interes
sum(as.numeric(duplicated(base)))
summary(base)

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