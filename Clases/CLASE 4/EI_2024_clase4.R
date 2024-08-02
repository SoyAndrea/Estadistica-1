library(tidyverse)
library(ggpubr)

########################
# qqplot
#######################
qqnorm(ejercicio$Edad,
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(ejercicio$Edad)



# qqplot versión 2

ggqqplot(ejercicio, "Edad")

ejercicio%>%
  ggqqplot("Edad")



# qqplot en cada grupo
ggqqplot(ejercicio, "Edad", facet.by = "Sexo")

