library(tidyverse)
library(ggpubr)
library(rstatix)


########################
# qqplot
#######################
qqnorm(ejercicio$Edad)
qqline(ejercicio$Edad)

qqnorm(ejercicio$TAs)
qqline(ejercicio$TAs)

qqnorm(ejercicio$TAd)
qqline(ejercicio$TAd)

#################################################
# TEST DE NORMALIDAD - SHAPIRO WILKS
###########################################
shapiro.test(ejercicio$Edad)

#######################################
# test de normalidad
#######################################

ks.test(ejercicio$Edad, pnorm, mean(ejercicio$Edad), sd(ejercicio$Edad))


############################
# test de levene
###########################
library(car)
leveneTest(ejercicio$TAd~ejercicio$ACV)



####################################################
# testeo de normalidad de las variables
ejercicio %>%
  shapiro_test(Edad)


ejercicio %>%
  group_by(Sexo) %>%
  shapiro_test(Edad)


ggqqplot(ejercicio, "Edad")


ggqqplot(ejercicio, "Edad", facet.by = "Sexo")

###################################################



# testeo de homocedasticidad de varianzas

ejercicio %>% levene_test(Edad ~ Sexo)



###########################################################################
# calculo del tamaño muestral
##########################################################

library(pwr)

# Cálculo del tamaño del efecto. d=(mediah0-mediah1)/desvío

# cálculo del tamaño muestral para comparación de dos medias 

pwr.t.test (d = 0.5,
            sig.level = 0.05 ,
            power = 0.9,
            type = "two.sample",
            n= NULL,
            alternative = "greater")


# cálculo del tamaño muestral para comparación de dos medias en muestras apareadas 

pwr.t.test (d = 0.5,
            sig.level = 0.05 ,
            power = 0.8,
            type = "paired",
            n=NULL,
            alternative = "two.sided")

# cálculo del tamaño muestral para comparación de dos proporciones

pwr.2p.test(h=0.2,sig.level =0.05, power=.80, alternative="two.sided")


# cálculo del tamaño muestral para test chi cuadrado

pwr.chisq.test (w=0.3, df=3, sig.level =0.05, power=0.8)

# cálculo del tamaño muestral para anova de un factor

pwr.anova.test (k =6 , f =0.1 , sig.level =0.05 , power =0.80)


############################################

