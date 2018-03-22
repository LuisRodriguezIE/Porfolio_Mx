rm(list=ls())
library(xts)
library(zoo)
library(quantmod)
library(tidyr)
library(PerformanceAnalytics)
library(reshape2)
library(ggplot2)


MexVal<-getSymbols("BOLSAA.MX",src="yahoo",auto.assign = F)
View(MexVal)
head(MexVal)
tail(MexVal)

# To calculate your daily return: 
#subtract the opening price from the closing price. 
#Then, divide the result by the opening price. 

##Simple mathematical average of a series of returns generated over a period of time.
MexVal.rets<-dailyReturn(MexVal) 

#VaR: The predicted maximum loss of a portafolio with a specific 
#probability level over a certain period of time.
#CVar: The expected value of the loss given that the loss exceeds VaR. 

# Value at risk (VaR) is a measure of the risk of loss for investments. 
# It estimates how much a set of investments might lose, given normal 
# market conditions, in a set time period such as a day.
MexValVar<-VaR(MexVal.rets,p=0.95,method = "historical")
MexValVar
MexValVar<-VaR(MexVal.rets,p=0.99,method = "historical")
MexValVar

#Conditional Value Risk
MexVC<-CVaR(MexVal.rets,p=0.99, method="historical")
MexVC

#Creacion de portafolio

tickComp<-c("ELEKTRA.MX","KOF","BIMBOA.MX")
weights<-c(0.3,0.4,0.3)
#Portafolio<-getSymbols(tickComp,src="yahoo",auto.assign = T,from=maxDate)
#View(Portafolio)

getSymbols(tickComp,src="yahoo",auto.assign = T,from=maxDate)

Port.prices<-na.omit(merge(Ad(ELEKTRA.MX),Ad(KOF),Ad(BIMBOA.MX))) #NA OMITION
View(Port.prices)
summary(Port.prices)
Port.return<-ROC(Port.prices,type = "discrete")[-1,] #Eliminate the first row
head(Port.return)
colnames(Port.return)<-tickComp #Chage the column names.
head(Port.return)

VaR(Port.return,p=0.99,weights = weights,portfolio_method = "component",method = "gaussian")
VaR(Port.return,p=0.99,weights = weights,portfolio_method = "component",method = "modified")

CVaR(Port.return,p=0.99,weights = weights,portfolio_method = "component",method = "modified")

#Expected Shortfall(ES) (or Conditional Value-at-Risk(CVaR)
ETL(Port.return,p=0.99,weights = weights,portfolio_method = "component",method = "modified")

VAR.Hist<-VaR(Port.return,p=0.95,weights = NULL,portfolio_method = "single",method = "historical")
VAR.Gauss<-VaR(Port.return,p=0.95,weights = NULL,portfolio_method = "single",method = "gaussian")
VAR.Mod<-VaR(Port.return,p=0.95,weights = NULL,portfolio_method = "single",method = "modified")

All.VARm<-data.frame(rbind(VAR.Hist,VAR.Gauss,VAR.Mod))
row.names(All.VARm)<-c("Histo","Gauss","Modif")
All.VARm

PortVAR.Hist<-VaR(Port.return,p=0.95,weights = weights,portfolio_method = "component",method = "historical")$hVaR
PortVAR.Gauss<-VaR(Port.return,p=0.95,weights = weights,portfolio_method = "component",method = "gaussian")$VaR
PortVAR.Mod<-VaR(Port.return,p=0.95,weights = weights,portfolio_method = "component",method = "modified")$MVaR



All.VARm$Portfolio<-0
All.VARm$Portfolio<-c(PortVAR.Hist,PortVAR.Gauss,PortVAR.Mod)
All.VARm<-abs(All.VARm)
All.VARm$Type<-c("Histo","Gauss","Modif")
All.VARm

plotVar<-melt(All.VARm,variable.name = "Ticker",value.name = "VaR")
plotVar
ggplot(plotVar,aes(x=Type,y=VaR,fill=Ticker))+geom_bar(stat = "identity",position="dodge")

