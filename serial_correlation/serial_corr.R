install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


df = readxl::read_excel('/home/rioba/Downloads/DSPIC96.xlsx')

#head(df)
#tail(df)

str(df)

#Question 1:descriptive statistics
stargazer(df)

#Question II(3)

df$RM1 = df$M1SL / (df$CPIAUCSL/100)

##Question 2~Regression analysis
# first regression real disposable personal income ~ PCEC96 + GS5 + CPI + M1SL
# SECOND REGRESSION real disposable personal income ~ PCEC96 + GS5 + CPI + M1SL + RM1

first_reg = lm(PCEC96 ~ DSPIC96 + GS5 + CPIAUCSL + M1SL, data= df)
summary(first_reg)
tab_model(first_reg)


second_reg = lm( PCEC96 ~ DSPIC96 + GS5 + RM1, data= df)
summary(second_reg)
tab_model(second_reg)


## Question 4)
df$logRM1 = log(df$RM1)
df$logDSPIC96 = log(df$DSPIC96)
df$logGS5 = log(df$GS5)

#Question 3
model_3 = lm(logRM1 ~ logDSPIC96 + logGS5, data=df)
summary(model_3)
tab_model(model_3)

##III. Serial correlation
### Detect the serial correlation 
## Plot the residuals over time
par(mar=c(1,1,1,1))
plot(df$DATE, model_3$residuals, type="l", main="Static Model: Residuals Over Time", xlab="Year", ylab="Residuals")

##Q4, there is visual evidence to suggest serial correlation because of the trend 
## in the graph for the years with the residuals of the model_3. 

#capture residuals.
RHAT = model_3$residuals
## Durbin-watson test and residuals 
library(car)
library(dplyr)
durbinWatsonTest(model_3, alternative="two.sided")
## we reject the null hypothesis because the p-value < 0.05
##we conclude that the residuals in the regression model are autocorrelated. 
##Breauch-Godfrey test 
library(lmtest)
library(zoo)
bgtest(model_3, order=1)
## From the output we see that the test statistic is 91 with df=3 and p<0.05.
## We reject the null hypothesis and conclude that autocorrelation exists among the residuals at order 1 or less. 

#Question 5 perform positive serial correlation. 
df$RHAT = residuals(model_3)

df = mutate(df, RHAT_1 = lag(RHAT, 1))
head(df)

model_rho = lm(RHAT~RHAT_1, data=df)
summary(model_rho)

#Question 6, test for coefficeint = 0
rho = coef(summary(model_rho))[2,4]
#test
ttest = function(model_rho, coefnum, val){
  co = coef(summary(model_rho))
  tstat = (co[coefnum, 1] - val)/co[coefnum,2]
  2*pt(abs(tstat), model_4$df.residual, lower.tail = FALSE )
}
ttest(model_rho, 2, 0.1)

## durbin-watson test
durbinWatsonTest(model_rho, alternative="two.sided")


#Step I:correcting the model
deltaRM1 = df$logRM1 - rho*lag(df$logRM1,1)

#regression corrected
df$deltaRM1 = deltaRM1
str(df)


model_4 = lm(deltaRM1 ~ logDSPIC96 + logGS5, data=df)
summary(model_4)

#question 7
## durbin-watson test
durbinWatsonTest(model_4, alternative="two.sided")

#estimate with built-in package
library(prais)
pw = prais_winsten(logRM1 ~ logDSPIC96 + logGS5, data=df, index = "logGS5")
summary(pw)