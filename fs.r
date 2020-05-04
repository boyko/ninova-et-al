#Import data and initial model ----
##
## install.packages("leaps")
library(leaps)
source("./pre.R")

fit = lm(GDP ~ K + L, data = dd)
summary(fit)
coef(fit)

#Graduates of Tertiary education test ----

plot(dd$Year, dd$Vishisti)
cor.test(dd$GDP, dd$Vishisti)
dd$Vishisti = log(dd$Vishisti)
dd1 = dd[-c(1, 2), ]
fit1 = lm(GDP ~ K + L + Vishisti, data = dd1)
summary(fit1)


cor.test(dd$GDP, dd$R.DExpends) #0.87 with p-value=0
eq = lm(GDP ~ K + L + R.DExpends, data = dd) #There is a strong positive correlatiob between the R&D and the GDP, but
#It isn't statistically significant in the model - sa it cannot be included
summary(eq)
#Graduated - osnovno;sredno - test ----
#Osnovno

cor.test(dd$GDP, dd$Osnovno) #there is negative correlation =-0.52 and it is significant at 0.05, but not at 0.01
eq1 = lm(GDP ~ K + L + Osnovno, data = dd)
summary(eq1)
eq2 = lm(GDP ~ K + L + Osnovno + Vishisti, data = dd)
summary(eq2) #we see that zavyrshili osnovno obrazovanie ne e statisticheski znachima za tozi model

cor.test(dd$GDP, dd$Sredno) #negative correlation =-0.68 - statistically significant at 5% and 1%
eq3 = lm(GDP ~ K + L + Sredno, data = dd)
summary(eq3) #it is statistically significant at the model at 5% level
eq4 = lm(GDP ~ K + L + Sredno + Vishisti, data = dd)
summary(eq4) #but included in this model - it is not statistically significant - p-value=0.1443

cor.test(dd$GDP, dd$EduExpends) #high correlation - 0.94 - statistically significant at all levels
eq5 = lm(GDP ~ K + L + EduExpends + Vishisti, data = dd)
summary(eq5) #this variable is not significant included in the model

#Synergies: S1=grad.osnovno*grad.sredno; S2=grad.osn*grad.sredno*vishisti; S3=grad.sredno*vishisti
dd$S1 = dd$Osnovno * dd$Sredno
dd$S2 = dd$Sredno * dd$Vishisti
dd$S3 = dd$Osnovno * dd$Sredno * dd$Vishisti
dd$S4 = dd$Osnovno * dd$Sredno * dd$Vishisti * dd$EduExpends

#Regsubsets  and final model----
dd = dd[-c(1, 2, 19), ]
fs = regsubsets(GDP ~ ., data = dd, nvmax = 11)
summary(fs)$which
summary(fs)$adjr2
#Highest Adj.R.2 = 0.9922370 for the model with 5 variables - K,L, EduExpends, S1 and S2
eq6 = lm(GDP ~ K + L + EduExpends + S1 + S2, data = dd)
summary(eq6)
