library(tidyverse)

# libreria que contiene la función que importa archivos excel
library(readxl)

# importación de la tabla de datos
ejercicio <- read_excel("ejercicio.xlsx")


#para ver el archivo de datos
View(ejercicio)

# etiquetas y transformación en factores de las cualitativas

ejercicio <- ejercicio%>%
  mutate(Sexo=factor(ejercicio$Sexo,labels=c("F","M")),
         ACV=factor(ejercicio$ACV,labels=c("No","Si")),
         ACVTIPO=factor(ejercicio$ACVTIPO,labels=c("HEMORRAGICO","ISQUEMICO"
                                                   ,"NOCORRESP","TIA")),
         Vivo=factor(ejercicio$Vivo,labels=c("No","Si")))

table(ejercicio$Vivo)

ejercicio <- ejercicio%>%
  mutate(Vivo=recode(Vivo, n="N"))



#############################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE #
#################################################
table(ejercicio$ACVTIPO)

table(ejercicio$Sexo)

table(ejercicio$Vivo)

table(ejercicio$ACV)

##############################################
#TABLA DE FRECUENCIA RELATIVA PARA UNA VARIABLE #
#################################################

prop.table(table(ejercicio$ACVTIPO))*100

##############################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE CON TOTAL#
#################################################

addmargins(table(ejercicio$ACVTIPO))

#####################################################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE INCLUYENDO MISSINGS #
#####################################################################

addmargins(table(ejercicio$ACVTIPO,useNA = "always"))


##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS#
##############################################

table(ejercicio$Sexo, ejercicio$ACVTIPO)

##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS#
##############################################

prop.table(table(ejercicio$Sexo, ejercicio$ACVTIPO))


##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS CON TOTALES#
##############################################
addmargins(table(ejercicio$Sexo, ejercicio$ACVTIPO))


#####################################################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS CON TOTALES Y MISSINGS#
#####################################################################
addmargins(table(ejercicio$Sexo, ejercicio$ACVTIPO,useNA = "always"))



###################################
# FRECUENCIAS CON EL PAQUETE expss#
###################################

############################
# SE CARGA EL PAQUETE expss#
############################
library(expss)


#######################
# TABLA DE FRECUENCIA #
#######################
fre(ejercicio$ACVTIPO)

##########################################
# PARA QUE LAS TABLAS SALGAN EN EL VISOR #
##########################################
expss_output_rnotebook()

fre(ejercicio$ACVTIPO)

#mejora de la tabla

fre(ejercicio$ACVTIPO,
    stat_lab = getOption("expss.fre_stat_lab", c("Casos", "Porcentaje valido", "Porcent",
                                                 "Resp, %", "Resp Acum, %")))



##############################
# ETIQUETA PARA UNA VARIABLE #
##############################

var_lab(ejercicio$ACVTIPO) = "Tipo de Acv"

fre(ejercicio$ACVTIPO,
    stat_lab = getOption("expss.fre_stat_lab", c("Casos", "Porcentaje valido", "Porcent",
                                                 "Resp, %", "Resp Acum, %")))
var_lab(ejercicio$Sexo) = "Sexo"

# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS #
###############################################
cro(ejercicio$Sexo,ejercicio$ACVTIPO)

#######################################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS COLUMNA #
#######################################################

cro_cpct(ejercicio$Sexo,ejercicio$ACVTIPO)

cro_cpct(ejercicio$Sexo,ejercicio$ACVTIPO,
         total_label = "Total",
         total_statistic = "u_cpct")

cro_rpct(ejercicio$Sexo,ejercicio$ACVTIPO,
         total_label = c("Total"),
         total_statistic = c("u_rpct"))


#######################################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS TOTALES #
#######################################################

cro_tpct(ejercicio$Sexo,ejercicio$ACVTIPO)


cro_tpct(ejercicio$Sexo,ejercicio$ACVTIPO,
         total_label = "Total",
         total_statistic = "u_tpct")

############
# GRAFICOS #
############

#####################################################
#GRAFICO DE BARRAS DE LA FRECUENCIA ABSOLUTA DE tipo de acv#
#####################################################
barplot(table(ejercicio$ACVTIPO),
        main="Título distinto",
        xlab="Tipo de Acv",
        ylab="Frec. absolutas",
        border="green",
        col="blue",
        density=100)

#######################################################
# GRAFICO DE BARRAS DE LA FRECUENCIA RELATIVA DE tipo de acv #
#######################################################


barplot(prop.table(table(ejercicio$ACVTIPO))*100,
        main="Tipo de Acv",
        xlab="Tipo",
        ylab="frecuencia porcentual",
        border="red",
        col="green",
        density=80)

################################
# GRAFICO DE TORTAS BASICO 
################################
pie(table(ejercicio$ACVTIPO),
    main="Tipo de Acv",
    col=c("red","orange","yellow","blue"),
    border="brown",
    clockwise=TRUE)

######################################################
# GRAFICO DE BARRAS PARA DOS VARIABLES FREC ABSOLUTAS#
######################################################

barplot(table(ejercicio$Sexo,ejercicio$ACVTIPO),beside=T,
        main = "Tipo de Acv según Sexo",
        xlab = "Tipo",
        ylab = "Frecuencia",
        col = c("red","blue"))

######################################################################

##################################################################
# GRAFICO DE BARRAS PARA DOS VARIABLES FREC RELATIVAS POR COLUMNA#
##################################################################

barplot(prop.table(table(ejercicio$Sexo,ejercicio$ACVTIPO),1),beside=T,
        main = "Tipo de Acv según Sexo",
        xlab = "Tipo",
        ylab = "Frecuencia",
        col = c("red","blue"))
legend("topright",
       c("Fem","Masc"),
       fill = c("red","blue"))
######################################################################


