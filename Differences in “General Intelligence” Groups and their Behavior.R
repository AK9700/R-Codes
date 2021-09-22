#Load libraries
library(tidyverse)
library(lmtest)
library(sandwich)

#Load files, divided by each experiment's response
crt<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/CRT.csv")
ef<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/EF.csv")
ma<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/MA.csv")
re<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/RE.csv")
scf<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/SCF.csv")
tp<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/TP.csv")
tu<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/TU.csv")

#Full-join these files to create a data file
crt%>%
  full_join(ef)%>%
  full_join(ma)%>%
  full_join(re)%>%
  full_join(scf)%>%
  full_join(tp)%>%
  full_join(tu)->data

#Create a back-up file
data->data_2

#CRT2 has units minutes so perform some textual cleaning
data$CRT2%>%
  str_remove(" minutes")->data_2$CRT2
data$CRT2%>%
  str_remove(" minute")->data_2$CRT2

#Remove $ from EE Questions so perform some textual cleaning
data$`EE Questions`%>%
  str_remove('[$]')->data_2$`EE Questions`

#Code a column into base group and experiment group
data%>%
  mutate(surveycode=case_when(Survey=="Blue"~1,Survey=="Green"~0))->data_2

# CRT1 has a unit of cents so perform some textual cleaning
data$CRT1%>%
  str_remove(' cents')->data_2$CRT1

#Summarise the findings of Endowment Effect experiment 
data_2%>%
  group_by(Survey,EE=as.factor(`EE Questions`))%>%
  summarise(n=n())%>%
  group_by(Survey)%>%
  summarise(EE,perc=n/sum(n)*100)
#Notice that the plurality wanted to sell it for 10 but wanted to receive it for 5

#Summarise the findings of Transaction Utility experiment 
data_2%>%
  group_by(Survey,TU=as.factor(`TU Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(TU,perc=n/sum(n)*100)
#Notice that some people chose Printed version with one semester access to the electronic version of the textbook: $89.00

#Summarise the findings of Reflection Effect experiment 
data_2%>%
  group_by(Survey,RE=as.factor(`RE Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(RE,perc=n/sum(n)*100)
# Observe more people are willing to take the gamble for loss

#Summarise the findings of Time Preference experiment
data_2%>%
  group_by(Survey,TP=as.factor(`TP Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(TP,perc=n/sum(n)*100)%>%
  ungroup()%>%
  arrange(desc(perc))
#No clear pattern could be observed.

#Summarise the findings of Sunk Cost Fallacy experiment
data_2%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(SCF,perc=n)
#More people are going to go when they purchased the ticket

#Summarise the findings of Summarise Mental Accounting experiment
data_2%>%
  group_by(Survey,MA=as.factor(`MA Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(MA,perc_MA=n/sum(n)*100)
#People experience mental accounting

#Encoding CRT variables for econometrics we set response 5 min to 1, else to 0
data_2%>%
  mutate(crt1_u=ifelse(CRT1=="5" & CRT2 !="5 minutes", 1,0),crt2_u=ifelse(CRT2=="5 minutes" & CRT1 !="5",1,0),crtb_use=ifelse(CRT1=="5" & CRT2=="5 minutes",1,0))->data_2

#Encoding mental accounting variables for econometrics we set response 5 min to 1, else to 0
data_2%>%
  mutate(ticket=case_when(`MA Questions`=="I would buy a ticket and go to the movie." | `MA Questions`=="I would buy another ticket and go to the movie."~"Buy ticket",`MA Questions`!="I would buy a ticket and go to the movie." & `MA Questions`!="I would buy another ticket and go to the movie"~"Will not Buy ticket"))%>%
  group_by(Survey,MA=as.factor(ticket),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,MA,perc_MA=n)->t_MA

#Rearranging the file with the intent of gauging the impact of CRT response on mental accounting
t_MA%>%
  spread(MA,perc_MA)%>%
  arrange(crt1_u,crt2_u,crtb_use)

#generating the output in Tex
print(xtable(t, type = "latex"))

#Fitting the linear model
glm(ma_buy~treatment+crtb_use+crt1_u+crt2_u, family = binomial(link = "logit"),data=data_2)->glm_MA

#Observing the summary
summary(glm_MA)

#General tests, standard error estimations
confint(glm_MA)

bptest(model_MA)


coeftest(glm_MA, vcov. = vcovHC, type = "HC1")

predict(glm_MA, 
        newdata = data.frame(treatment=1,crtb_use=0,crt1_u=0,crt2_u=1),
        type = "response")
#It appears that only the constant and the first crt response has any effect on mental accounting 


#Rearranging the file with the intent of gauging the impact of CRT response on Reflection effects
data_2%>%
  group_by(Survey,RE=as.factor(`RE Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,RE,n_RE=n)%>%ungroup()->t_n_RE


data_2%>%
  group_by(Survey,RE=as.factor(`RE Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,RE,n)%>%
  ungroup()%>%
  spread(RE,n,0)->t_RE

t_RE%>%
  mutate(Risky=`50% chance of losing $1000 and 50% chance of losing $0`+`50% chance of winning $1000 and 50% chance of winning $0`,Safe=`Win $500 for sure`+`Lose $500 for sure`)%>%
  select(Survey,crt1_u,crt2_u,crtb_use,Risky,Safe)%>%
  arrange(crt1_u,crt2_u,crtb_use)


data_2%>%
  mutate(risks=ifelse(`RE Questions`=="50% chance of winning $1000 and 50% chance of winning $0" | `RE Questions`=="50% chance of losing $1000 and 50% chance of losing $0",1,0),
    treatment=ifelse(Survey=="Blue",0,1))->data_2

#Building the model
glm(risks~treatment+crtb_use+crt1_u+crt2_u,family = binomial(link = "logit"), data = data_2)->glm_RE

#Some general tests and coef estimations
ptest(model_RE)

coeftest(glm_RE, vcov = vcovHC(glm_RE, type="HC1"))

predict(glm_MA, 
        newdata = data.frame(treatment=0,crtb_use=0,crt1_u=0,crt2_u=0),
        type = "response")

#Making adjustments to enable estimating CRT's impact on sunk cost fallacy
data_2%>%
  group_by(Survey,SCF=as.factor(`SCF Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,SCF,perc_SCF=n/sum(n)*100)->t_SCF

#Building the model 
glm(scf_yes~treatment+crtb_use+crt1_u+crt2_u,family = binomial(link = "logit"),data=t_SCF)->glm_SCF

#General tests and coef estimation
summary(model_SCF)

bptest(model_SCF)

reset(model_SCF)

coeftest(glm_SCF, vcov = vcovHC(glm_SCF, type="HC1"))

#Developing some tex tables and graphs
stargazer(glm_RE,glm_SCF,glm_MA)

plot(glm_MA)

ggplot(aes(x=as.factor(treatment),y=as.factor(ma_buy)),data = data_2)+
  geom_jitter()+
  geom_smooth()

glm(scf_yes~as.factor(treatment)+as.factor(crtb_use)+as.factor(crt1_u)+as.factor(crt2_u),family = binomial(link = "logit"),data=data_2)->t

coeftest(t, vcov = vcovHC(t, type="HC1"))

#Estimating the overall significance of crt variables
wald.test(b = coef(glm_RE), Sigma = vcov(glm_RE), Terms = 3:5)

anova(glm_MA, test="Chisq")

anova(glm_SCF, test="Chisq")

anova(glm_RE, test="Chisq")

pR2(glm_MA)

pR2(glm_SCF)

pR2(glm_RE) 


