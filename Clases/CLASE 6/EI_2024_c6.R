library(tidyverse)
library(rstatix)
library(haven)

LOWBWT <- read_sav("LOWBWT.sav")
View(LOWBWT)

x <- chisq.test(LOWBWT$LOW,LOWBWT$SMOKE)

xx <- fisher.test(LOWBWT$LOW,LOWBWT$SMOKE)

x$observed   # observed counts (same as M)
x$expected   # expected counts under the null
x$residuals  # Pearson residuals
x$stdres   

prop_test(table(LOWBWT$LOW,LOWBWT$SMOKE))

fisher_test(table(LOWBWT$LOW,LOWBWT$SMOKE))

cramer_v(table(LOWBWT$LOW,LOWBWT$SMOKE))


