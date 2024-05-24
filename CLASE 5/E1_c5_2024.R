# para hacer analisis descriptivo 

library(psych)
describe(LOWBWT[,c("AGE","LWT","BWT")], na.rm=TRUE, skew = F, ranges = F,
         trim=.1, quant = NULL, IQR = F)


describe(LOWBWT$AGE, na.rm=TRUE, skew = F, ranges = F,
         trim=.1, quant = NULL, IQR = F)


# test de una muestra
t.test(LOWBWT$AGE, mu=23, alternative = c("two.sided"))


# test de dos muestras
t.test(LOWBWT$AGE~LOWBWT$LOW)

describeBy(LOWBWT$AGE,LOWBWT$LOW, na.rm=TRUE, skew = F, ranges = F,
         trim=.1, quant = NULL, IQR = F)


#  comandos que se utilizan en la resolución del ejercicio

t.test(LOWBWT$AGE, mu=23, alternative = c("less"))
t.test(LOWBWT$AGE, mu=23, alternative = c("two.sided"))

t.test(LOWBWT$LWT, mu=140, alternative = c("greater"))
t.test(LOWBWT$LWT, mu=140, alternative = c("less"))
t.test(LOWBWT$LWT, mu=140, alternative = c("two.sided"))

t.test(LOWBWT$BWT, mu=2500, alternative = c("greater"))
t.test(LOWBWT$BWT, mu=2500, alternative = c("less"))
t.test(LOWBWT$BWT, mu=2500, alternative = c("two.sided"))
