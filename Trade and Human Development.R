#
library(tidyverse)
library(foreign)
# Load GDP
gdp<-read_csv("/Users/ayushkumar/Downloads/Internation trade data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_1495053/WoGDP.csv")
# gather GDP 
gdp%>%
  gather(year,gdp,-Country)%>%->gdp
#Filter for 2018
gdp$year<-as.integer(gdp$year)
gdp%>%
  filter(year==2018)->gdp
#ukgdp select and create a coulmn
gdp%>%
  mutate(uk_gdp=2860000000000)->gdp
# Load uk trade data
trade<-read_csv("/Users/ayushkumar/Downloads/Internation trade data/trade.csv")
#change trade col names
colnames(trade)<-c("Country","year","export","import")
#full join gdp and trade
gdp%>%
  full_join(trade)%>%
  mutate(trade_vol=export*import)%>%
  select(Country,year,gdp,uk_gdp,trade_vol)->fdsheet
#Read HDI
hdi<-read_csv("/Users/ayushkumar/Downloads/Internation trade data/HDI.csv")
#Gather HDI
hdi%>%
  gather(year,hdi,-Country)->hdi
#Clean HDI
as.integer(hdi$year)->hdi$year
as.double(hdi$hdi)->hdi$hdi
hdi%>%
  filter(year==2018)->hdi
hdi%>%
  mutate(hdi_ij=hdi/0.92)->hdi
hdi%>%
  select(Country,year,hdi_ij)->hdi
#fulljoin hdi
fdsheet%>%
  full_join(hdi)
#load openness
open<-read_csv("/Users/ayushkumar/Downloads/API_NE.TRD.GNFS.ZS_DS2_en_csv_v2_1566329/openness.csv")
#clean openness
open%>%
  gather(year,open,-`Country Name`)->open
colnames(open)<-c("Country","year","open")
as.integer(open$year)->open$year
open%>%
  filter(year==2018)->open
open%>%
  mutate(open_ij=open/62.6)%>%
  select(Country,year,open_ij)->open
#fulljoin with fdsheet
fdsheet%>%
  full_join(open)->fdsheet
#load and clean ddi
ddi<-read.dta("/Users/ayushkumar/Downloads/Internation trade data/gravdata_cepii/gravdata.dta")
ddi%>%
  filter(iso3_o=="GBR",year==2015)%>%
  select(iso2_d,iso3_d,distw,col_to)->ddi
ddi%>%as_tibble()->ddi
#load iso3
iso3<-read_csv("/Users/ayushkumar/Downloads/iso3.csv")
#merge iso3 and ddi
ddi%>%
  full_join(iso3)->ddi
#merge fdi
fdsheet%>%
  full_join(ddi)->fdsheet
# Get Rid of NA's
na.omit(fdsheet)->use_fdsheet
#Work on fdsheet
use_fdsheet%>%
  mutate(Country,year,ln_gdp=log(gdp),ln_uk_gdp=log(uk_gdp),ln_trade_vol=log(trade_vol),open_ij,ln_dist=log(distw),col_to)%>%
  select(Country,year,ln_gdp,ln_uk_gdp,ln_trade_vol,open_ij,ln_dist,col_to)->use_fdsheet_2
#fit lin model
model<-lm(ln_trade_vol~ln_gdp+open_ij+ln_dist+col_to,data=use_fdsheet_2)
#residual diag
plot(model$residuals)
library(lmtest)
library("sandwich")
summary(model)
coeftest(model,vcov = vcovHC(model, type = "HC1"))
#Ramsey Test
resettest(model)
#
write.csv(use_fdsheet_2,"/Users/ayushkumar/Downloads/use")
#
install.packages("mlr")
#
library(dplyr)
#
use_fdsheet_2%>%
  mutate(v=1,ctr=Country)%>%
  spread(ctr,v,fill = 0)->use_fdsheet_3
use_fdsheet_2

use_fdsheet_3[,c(-1,-2,-4,c(-26,-42,-47,-53,-57,-105))]->use_fdsheet_4
use_fdsheet_3[,c("Angola","Barbados","Botswana","Ecuador","Kiribati","Lebanon")]
which(colnames(use_fdsheet_3) %in% c("Russia","Djibouti","Estonia","Nepal","Costa Rica","Bhutan","Haiti"))
'%ni%' <- Negate('%in%')
#
use_fdsheet_3[,150]
#
model_2<-lm(ln_trade_vol~.,data = use_fdsheet_4)
#
library(broom)
#
library(car)
#
summary(model_2)
plot(model_2)

coeftest(model_2, vcov = vcovHC(model_2, type="HC1"))

stargazer(model,model_2)

augment(model)%>%
  full_join(use_fdsheet_2)%>%
  arrange(.cooksd)%>%
  print(n=140)
#
use_fdsheet_2
#
ggplot(use_fdsheet_2,aes(x=ln_gdp,y=ln_trade_vol,color=as.factor(col_to)))+
  geom_point()+
  geom_smooth(method = "lm",se=0)+
  labs(x="Natural Log of GDP",y="Natural Log of Trade Volume")+
  theme_classic()+
  scale_fill_discrete(labels = c("Not A Former Colony", "A Former Colony"))+
  theme(plot.title = element_blank(),legend.position = "bottom",axis.text.x = element_text(angle=0),legend.title=element_blank())
#
ggplot(use_fdsheet_2,aes(x=hdi_ij,y=ln_trade_vol,color=as.factor(col_to)))+
  geom_point()+
  geom_smooth(method = "lm",se=0)+
  labs(x="HDI of nations i and j",y="Natural Log of Trade Volume")+
  theme_classic()+
  scale_fill_discrete(labels = c("Not A Former Colony", "A Former Colony"))+
  theme(plot.title = element_blank(),legend.position = "bottom",axis.text.x = element_text(angle=0),legend.title=element_blank())
#
ggplot(use_fdsheet_2,aes(x=log(open_ij),y=ln_trade_vol,color=as.factor(col_to)))+
  geom_point()+
  geom_smooth(method = "lm",se=0)+
  labs(x="HDI of nations i and j",y="Natural Log of Trade Volume")+
  theme_classic()+
  scale_fill_discrete(labels = c("Not A Former Colony", "A Former Colony"))+
  theme(plot.title = element_blank(),legend.position = "bottom",axis.text.x = element_text(angle=0),legend.title=element_blank())
#
ggplot(use_fdsheet_2,aes(x=log(open_ij),y=log(hdi_ij),color=as.factor(col_to)))+
  geom_point()+
  geom_smooth(method = "lm",se=0)+
  labs(x="Natural Log of Openness of Nations i and j",y="Natural Log of HDI of Nations i and j")+
  theme_classic()+
  scale_fill_discrete(labels = c("Not A Former Colony", "A Former Colony"))+
  theme(plot.title = element_blank(),legend.position = "bottom",axis.text.x = element_text(angle=0),legend.title=element_blank())

                 
