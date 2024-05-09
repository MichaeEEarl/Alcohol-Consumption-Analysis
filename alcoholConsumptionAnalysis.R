library(readxl)
library(AER)
library(vtable)
library(dplyr)

#import the data
AlcData <- read_excel("alcohol.xlsx")

#Create sex Bernoulli random variable with 0 for men and 1 for female
AlcData$sexBRV <- AlcData$sex - 1

#Done in order to see split between men and women in data
AlcData$male <- AlcData$sex == 1

#Done in order to see split between rural and urban areas in data
AlcData$urban <- AlcData$urbrur == 1

#add 1 to wgsal for log
AlcData$wgsal2 <- AlcData$wgsal+1

#View(AlcData)

#Summary Tables
#total
st(data = AlcData,
   vars = c("wgsal", "days", "afqtrev","weight",
            "urbrur", "urate", "sexBRV"),
   summ = c('mean(x)','sd(x)',
            "median(x)",'IQR(x)',
            'min(x)', 'max(x)','notNA(x)'))

#by sex
st(data = AlcData, group = "male", group.long = TRUE,
   vars = c("wgsal", "days", "afqtrev","weight",
            "urbrur", "urate"),
   summ = c('mean(x)','sd(x)',
            "median(x)",'IQR(x)',
            'min(x)', 'max(x)'))

#by urbrur
st(data = AlcData, group = "urban", group.long = TRUE,
   vars = c("wgsal", "days", "afqtrev","weight",
            "urate", 'sexBRV'),
   summ = c('mean(x)','sd(x)',
            "median(x)",'IQR(x)',
            'min(x)', 'max(x)'))

#regression model
mod1 <- lm(log(wgsal2) ~ days + sexBRV + weight
           + afqtrev + urbrur + urate, data = AlcData)
coeftest(mod1, vcov = vcovHC, type="HC1")
summary(mod1)$adj.r.squared

#plot
mod2 <- lm(log(wgsal2) ~ days, data = AlcData)
plot(log(AlcData$wgsal2) ~ AlcData$days,
     ylab = "Log(wgsal)", xlab = "Days", main = "Log(wgsal) vs Days")
orderid <- order(AlcData$days)
lines(AlcData$days[orderid], fitted(mod2)[orderid], col="red")

