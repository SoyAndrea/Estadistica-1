library(tidyverse)

# libreria que contiene la función que importa archivos excel
library(readxl)

# importación de la tabla de datos
ejercicio <- read_excel("ejercicio.xlsx")


#para ver el archivo de datos
View(ejercicio)

#para ver los nombres de las variables del archivo
names(ejercicio)

#para tener un primer resumen del contenido del archivo
summary(ejercicio)

# para ver las características de las variables del archivo
str(ejercicio)

# crear un nuevo data set con las variables GUIA, Edad y Sexo
maschico <- ejercicio%>%
  select(GUIA,Edad,Sexo)

# Crear un nuevo data set con los datos de los varones

varones <- ejercicio%>%
  filter(Sexo=="M")

# Crear un nuevo data set con los datos de varones y sólo las variables GUIA y Edad

varones_maschico <- ejercicio%>%
  filter(Sexo=="M")%>%
  select(GUIA, Edad)

# generación de nuevas variables o modificación de las existentes
# generar una variable con la Tam

ejercicio <- ejercicio%>%
  mutate(tam=trunc(TAd+(TAs+TAd)/3))

# generar una variable que identifique a los pacientes mayores de 70 años

ejercicio <- ejercicio%>%
  mutate(mas70=factor(Edad>70, labels = c("No","Si")))

# transformar a la variable tipo de acv en una variable factor
ejercicio <- ejercicio%>%
  mutate(tipo_acv=factor(ACVTIPO, labels = c("Hemorrágico","Isquémico","Nocorresp","TIA")))

# Se puede hacer todo de una sola vez.....
ejercicio <- ejercicio%>%
  mutate(tam=trunc(TAd+(TAs+TAd)/3),
         mas70=factor(Edad>70, labels = c("No","Si")),
         tipo_acv=factor(ACVTIPO, labels = c("Hemorrágico","Isquémico","Nocorresp","TIA")))

