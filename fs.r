#Import data and initial model ----
setwd("C:\\Users\\vladi\\Documents\\FEBA\\Macro II\\Project\\kur")
dd=read.csv("ImportR.csv", stringsAsFactors = F, na.strings=c(""," ", "?"))
dd$GDP=log(dd$GDP)
dd$K=log(dd$K)
dd$L=log(dd$L)
fit=lm(GDP~K+L, data=dd)
summary(fit)
coef(fit)
#Graduates of Tertiary education test ----
dd$Vishisti=c(NA, NA,50214,46876,45565,45510,44770,48544,54309,57167,59927,63405,58961,65221,62010,61276,58919,55428,53115)
plot(dd$Year, dd$Vishisti)
cor.test(dd$GDP, dd$Vishisti)
dd$Vishisti=log(dd$Vishisti)
dd1=dd[-c(1,2),]
fit1=lm(GDP~K+L+Vishisti, data=dd1)
summary(fit1)
#R.D - delfated - test
dd$R.DExpends=c(257508.706, 238610.0785, 257711.1621, 275148.8324,290577.5714, 294089.7209, 311793.7475,333879.9217, 355918.2262, 384870.6652,436161.6249,  429834.6467, 484894.1299, 507863.8705,  657747.2546,850471.1745,  744127.4882, 761388.773, 808877.5402)
dd$R.DExpends=log(dd$R.DExpends)
cor.test(dd$GDP, dd$R.DExpends) #0.87 with p-value=0
eq=lm(GDP~K+L+R.DExpends, data=dd) #There is a strong positive correlatiob between the R&D and the GDP, but
#It isn't statistically significant in the model - sa it cannot be included 
summary(eq)
#Graduated - osnovno;sredno - test ----
#Osnovno
dd$Osnovno=c(85835,87716,	87823,	90844,	86455,	85425,	77977,	71280,	67110,	63557,	61014,	56753,	53929,	54116,	57798,	58235,	58654,	113250,	71880)
dd$Osnovno=log(dd$Osnovno)
cor.test(dd$GDP, dd$Osnovno) #there is negative correlation =-0.52 and it is significant at 0.05, but not at 0.01
eq1=lm(GDP~K+L+Osnovno, data=dd)
summary(eq1)
eq2=lm(GDP~K+L+Osnovno+Vishisti, data=dd)
summary(eq2) #we see that zavyrshili osnovno obrazovanie ne e statisticheski znachima za tozi model 
#Sredno 
dd$Sredno=c(79470,	75654,	55141,	77737	,75354,	77845	,76361	,77057	,69649,	69302,	63043	,62500	,59741,	56316	,53487	,50326	,44975	,48281,	49504)
dd$Sredno=log(dd$Sredno)
cor.test(dd$GDP, dd$Sredno) #negative correlation =-0.68 - statistically significant at 5% and 1%
eq3=lm(GDP~K+L+Sredno, data=dd)
summary(eq3) #it is statistically significant at the model at 5% level 
eq4=lm(GDP~K+L+Sredno+Vishisti, data=dd)
summary(eq4) #but included in this model - it is not statistically significant - p-value=0.1443

#Education - expenditures - test ----
dd$EduExpends=c(2426472.927,	2403672.011,	2635946.991,	2759993.639,	2852234.624,	2975549.276,	3092401.894,	3190886.525,	3634038.083,	3613533.258,	3211001.664,	3208270.169,	3128063.392,	3380840.952,	3719554.793,	3704198.737,	3890301.07,	4288573.336, NA)
dd$EduExpends=log(dd$EduExpends)
cor.test(dd$GDP, dd$EduExpends) #high correlation - 0.94 - statistically significant at all levels
eq5=lm(GDP~K+L+EduExpends+Vishisti, data=dd)
summary(eq5) #this variable is not significant included in the model 

#Synergies: S1=grad.osnovno*grad.sredno; S2=grad.osn*grad.sredno*vishisti; S3=grad.sredno*vishisti
dd$S1=dd$Osnovno*dd$Sredno
dd$S2=dd$Sredno*dd$Vishisti
dd$S3=dd$Osnovno*dd$Sredno*dd$Vishisti
dd$S4=dd$Osnovno*dd$Sredno*dd$Vishisti*dd$EduExpends

#Regsubsets  and final model----
dd=dd[-c(1,2,19),]
library(leaps)
fs=regsubsets(GDP~.,data=dd,nvmax=11)
summary(fs)$which
summary(fs)$adjr2
#Highest Adj.R.2 = 0.9922370 for the model with 5 variables - K,L, EduExpends, S1 and S2
eq6=lm(GDP~K+L+EduExpends+S1+S2, data=dd)
summary(eq6)
