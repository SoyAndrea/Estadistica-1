library(tidyverse)
library(rstatix)

ejercicio%>%
  get_summary_stats(.,type="common")

ejercicio%>%
  get_summary_stats(Edad,type="common")

ejercicio%>%
  get_summary_stats(Edad, TAs, TAd,type="common")

ejercicio%>%
  get_summary_stats(Edad, TAs, TAd)

ejercicio%>%
  get_summary_stats(Edad, TAs, TAd,type="mean_sd")


ejercicio%>%
  group_by(Sexo)%>%
  get_summary_stats(Edad, TAs, TAd,type="mean_sd")

ejercicio%>%
  group_by(ACVTIPO)%>%
  get_summary_stats(Edad, TAs, TAd,type="mean_sd")

ejercicio%>%
  group_by(ACVTIPO,Sexo)%>%
  get_summary_stats(Edad, TAs, TAd,type="mean_sd")




############################
# SE CARGA EL PAQUETE expss#
############################
library(expss)

##########################################
# PARA QUE LAS TABLAS SALGAN EN EL VISOR #
##########################################
expss_output_rnotebook()

#######################
# TABLA DE FRECUENCIA #
#######################
fre(ejercicio$Edad,
    stat_lab = getOption("expss.fre_stat_lab", c("Casos", "Porcentaje valido", "Percent",
                                                 "Responses, %", "Cumulative responses, %")))


#######################################################
# GRAFICO DE BARRAS DE LA FRECUENCIA RELATIVA DE EDAD #
#######################################################
barplot(prop.table(table(ejercicio$Edad)),
        main="Edad",
        xlab="Edad",
        ylab="frecuencia relativa",
        border="red",
        col="blue",
        density=50)

############################
# PARA HACER EL HISTOGRAMA #
############################
hist(ejercicio$Edad, 
     main="Histograma de Edad", 
     xlab="Edad",
     freq=F,
     border="blue", 
     col="green",
     xlim=c(30,100),
     las=1, 
     breaks=10)

lines(density(ejercicio$Edad, na.rm=T))


hist(ejercicio$TAs, 
     main="Histograma de TAs", 
     xlab="TAs",
     freq=F,
     border="blue", 
     col="green",
     xlim=c(0,300),
     las=1, 
     breaks=10)

lines(density(ejercicio$TAs, na.rm=T))

hist(ejercicio$TAd, 
     main="Histograma de TAd", 
     xlab="TAd",
     freq=F,
     border="blue", 
     col="green",
     xlim=c(50,200),
     las=1, 
     breaks=10)

lines(density(ejercicio$TAs, na.rm=T))



#######################################
# BOXPLOTS
#####################################
boxplot(ejercicio$Edad, data = ejercicio, na.action = NULL,
        xlab = "Edad",
        ylab = "y",main="Boxplot de edad", ann = T, horizontal = TRUE,
        varwidth = F, outline=T, col="red")

boxplot(ejercicio$TAs, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")
boxplot(ejercicio$TAd, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

###############################################
# BOXPLOT PARA DISTINTAS VARIABLES JUNTAS
###############################################
boxplot(ejercicio[,c(5,6)])

boxplot(ejercicio[,c("TAs","TAd")])


boxplot(Edad~Sexo, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = F,
        varwidth = F, outline=T, col="yellow")

boxplot(Edad~ACV, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

boxplot(Edad~ACVTIPO, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = F,
        varwidth = F, outline=T, col="yellow")


boxplot(Edad~Sexo+ACV, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

boxplot(TAs~Sexo+ACV, data = ejercicio, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")


##################################################################
# el paquete table1

#install.packages("table1")
library(table1) 

table1(~  TAs, data=ejercicio)

table1(~  TAs + TAd +Sexo+ Edad+ACV, data=ejercicio)


#tabla con variable de agrupamiento:
table1(~  TAs + TAd | Sexo, data=ejercicio)

table1(~  ACV +ACVTIPO +TAs + TAd | Sexo, data=ejercicio)


#Modificamos títulos:
table1(~  ACV +ACVTIPO +TAs + TAd | Sexo, 
       data=ejercicio, 
       overall = "Total", 
       rowlabelhead = "Variables")


#Diferentes estilos:
table1(~  ACV +ACVTIPO +TAs + TAd | Sexo, 
       data=ejercicio, 
       overall = "Total", 
       rowlabelhead = "Variables", 
       topclass = "Rtable1-zebra")

table1(~  ACV +ACVTIPO +TAs + TAd | Sexo, 
       data=ejercicio, 
       overall = "Total", 
       rowlabelhead = "Variables", 
       topclass = "Rtable1-grid")

table1(~  ACV +ACVTIPO +TAs + TAd | Sexo, 
       data=ejercicio, 
       overall = "Total", 
       rowlabelhead = "Variables", 
       topclass = "Rtable1-grid Rtable1-shade Rtable1-times")

