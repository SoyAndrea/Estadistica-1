library(tidyverse)
library(rstatix)


salarios <- data.frame(
  sala=c(22.5,19.8,20.6,24.7,23.2,19.2,18.7,20.9,21.6,23.5,20.7,21.6,
            21.9,21.6,22.4,24.0,24.1,23.4,21.2,23.9,20.5,24.5,22.3,23.6),
  sexo = gl(2,12,24,labels=c("mujer","varón")))

# test t de comparación de medias independientes
salarios %>% t_test(sala ~ sexo)

# tamaño del efecto
salarios %>% cohens_d(sala ~ sexo)

# test no paramétrico de comparación de medias independientes

salarios %>% wilcox_test(sala ~ sexo)

# tamaño del efecto
salarios %>% wilcox_effsize(sala ~ sexo)


# MUESTRAS APAREADAS
hijos <- data.frame(
  numero=c(3,2,1,0,0,1,2,2,2,0,
         2,3,2,2,0,2,1,3,1,2),
  sexo = gl(2,10,20,labels=c("mujer","varón")))

# test t de muestras apareadas
hijos %>% t_test(numero ~ sexo, paired = T)

# tamaño del efecto para muestras apareadas
hijos %>% cohens_d(numero ~ sexo, paired = T)

# test no paramétrico de muestras apareadas
hijos %>% wilcox_test(numero ~ sexo, paired = T)

# tamaño del efecto
hijos %>% wilcox_effsize(numero ~ sexo, paired = T)
