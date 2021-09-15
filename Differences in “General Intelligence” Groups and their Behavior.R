#
library(tidyverse)
#
crt<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/CRT.csv")
ef<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/EF.csv")
ma<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/MA.csv")
re<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/RE.csv")
scf<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/SCF.csv")
tp<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/TP.csv")
tu<-read_csv("/Users/ayushkumar/Downloads/Behav Paper/TU.csv")
#
crt%>%
  full_join(ef)%>%
  full_join(ma)%>%
  full_join(re)%>%
  full_join(scf)%>%
  full_join(tp)%>%
  full_join(tu)->data
#
data->data_2
# CRT1 has a unit of cents
data$CRT1%>%
  str_remove(" ")->data_2$CRT1

#CRT2 has units minutes
data$CRT2%>%
  str_remove(" minutes")->data_2$CRT2
data$CRT2%>%
  str_remove(" minute")->data_2$CRT2

#Remove $ from EE Questions
data$`EE Questions`%>%
  str_remove('[$]')->data_2$`EE Questions`

#Survey Code
data%>%
  mutate(surveycode=case_when(Survey=="Blue"~1,Survey=="Green"~0))->data_2

#Remove Cents from CRT1
data$CRT1%>%
  str_remove(' cents')->data_2$CRT1

#Summary
data_2%>%
  summary()

#Summarise Endowment Effect
data_2%>%
  group_by(Survey,EE=as.factor(`EE Questions`))%>%
  summarise(n=n())%>%
  group_by(Survey)%>%
  summarise(EE,perc=n/sum(n)*100)
#Notice that the prularity wanted to sell it for 10 but wanted to recieve it for 5

#Summarise Transaction Utility
data_2%>%
  group_by(Survey,TU=as.factor(`TU Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(TU,perc=n/sum(n)*100)
#Notice that some people chose Printed version with one semester access to the electronic version of the textbook: $89.00

#Summarise Transaction Utility
data_2%>%
  group_by(Survey,RE=as.factor(`RE Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(RE,perc=n/sum(n)*100)
# More people are willing to take the gambel for loss

#Summarise Time Prefference
data_2%>%
  group_by(Survey,TP=as.factor(`TP Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(TP,perc=n/sum(n)*100)%>%
  ungroup()%>%
  arrange(desc(perc))
#No clear pattern could be observed.

#Summarise Sunk Cost Fallasy
data_2%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(SCF,perc=n)
#More people are going to go when they purchased the ticket

#Summarise Mental Accounting (MA)
data_2%>%
  group_by(Survey,MA=as.factor(`MA Questions`))%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey)%>%
  summarise(MA,perc_MA=n/sum(n)*100)
#People experience mental accounting

#Some work
data_2%>%
  mutate(crt1_u=ifelse(CRT1=="5" & CRT2 !="5 minutes", 1,0),crt2_u=ifelse(CRT2=="5 minutes" & CRT1 !="5",1,0),crtb_use=ifelse(CRT1=="5" & CRT2=="5 minutes",1,0))->data_2

#CRT and Mental Accounting
data_2%>%
  mutate(ticket=case_when(`MA Questions`=="I would buy a ticket and go to the movie." | `MA Questions`=="I would buy another ticket and go to the movie."~"Buy ticket",`MA Questions`!="I would buy a ticket and go to the movie." & `MA Questions`!="I would buy another ticket and go to the movie"~"Will not Buy ticket"))%>%
  group_by(Survey,MA=as.factor(ticket),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,MA,perc_MA=n)->t_MA

t_MA%>%
  spread(MA,perc_MA)%>%
  arrange(crt1_u,crt2_u,crtb_use)

2print(xtable(t, type = "latex"))

data_2%>%
  mutate(ma_buy=ifelse(`MA Questions`=="I would buy a ticket and go to the movie.",1,0))->data_2

glm(ma_buy~treatment+crtb_use+crt1_u+crt2_u, family = binomial(link = "logit"),data=data_2)->glm_MA

summary(glm_MA)
confint(glm_MA)

bptest(model_RE)


coeftest(glm_MA, vcov. = vcovHC, type = "HC1")

predict(glm_MA, 
        newdata = data.frame(treatment=1,crtb_use=0,crt1_u=0,crt2_u=1),
        type = "response")

#CRT and Transaction Utility
data_2%>%
  group_by(Survey,TU=as.factor(`TU Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,TU,perc=n/sum(n)*100)%>%
  ungroup()%>%
  spread(TU,perc)

data_2%>%
  count(Survey=="Blue")

#CRT and RE
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
  
#modify for lm

data_2%>%
  mutate(risks=ifelse(`RE Questions`=="50% chance of winning $1000 and 50% chance of winning $0" | `RE Questions`=="50% chance of losing $1000 and 50% chance of losing $0",1,0),
         treatment=ifelse(Survey=="Blue",0,1))->data_2

ggplot(aes(y=risks,group=crtb_use),data = data_2)+
  geom_boxplot()

glm(risks~treatment+crtb_use+crt1_u+crt2_u,family = binomial(link = "logit"), data = data_2)->glm_RE

library(lmtest)

bptest(model_RE)

library(sandwich)

coeftest(glm_RE, vcov = vcovHC(glm_RE, type="HC1"))

predict(glm_MA, 
        newdata = data.frame(treatment=0,crtb_use=0,crt1_u=0,crt2_u=0),
        type = "response")

#
data_2%>%
  group_by(treatment,risks,crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  group_by(treatment,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,risks,perc=n/sum(n)*100)%>%
  ungroup()%>%
  spread(risks,perc)->t_RE
colnames(t_RE)<-c("treatment","crt1_u","crt2_u","crtb_use","Safe Option","Risky Option")


# CRT and TP
data_2%>%
  group_by(Survey,TP=as.factor(`TP Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,TP,perc=n/sum(n)*100)%>%
  ungroup()%>%
  spread(TP,perc)

#CRT and EE
data_2%>%
  group_by(Survey,EE=as.factor(`EE Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,EE,perc=n/sum(n)*100)%>%
  ungroup()%>%
  spread(EE,perc)

#CRT and SCF
data_2%>%
  group_by(Survey,SCF=as.factor(`SCF Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,SCF,perc_SCF=n/sum(n)*100)->t_SCF

data_2%>%
  group_by(Survey,SCF=as.factor(`SCF Questions`),crtb_use,crt1_u,crt2_u)%>%
  summarise(n=n())%>%
  ungroup()%>%
  group_by(Survey,crt1_u,crt2_u,crtb_use)%>%
  summarise(crtb_use,crt1_u,crt2_u,SCF,perc_SCF=n)%>%
  spread(SCF,perc_SCF)%>%
  arrange(crt1_u,crt2_u,crtb_use)%>%
  select(`Yes, I would go to the concert.`,`No, I would not go to the concert.`)

3data_2%>%
  mutate(scf_yes=ifelse(`SCF Questions`=="Yes, I would go to the concert.",1,0))->data_2

glm(scf_yes~treatment+crtb_use+crt1_u+crt2_u,family = binomial(link = "logit"),data=data_2)->glm_SCF

summary(model_SCF)

bptest(model_SCF)

reset(model_SCF)

coeftest(glm_SCF, vcov = vcovHC(glm_SCF, type="HC1"))

stargazer(glm_RE,glm_SCF,glm_MA)

plot(glm_MA)

ggplot(aes(x=as.factor(treatment),y=as.factor(ma_buy)),data = data_2)+
  geom_jitter()+
  geom_smooth()

glm(scf_yes~as.factor(treatment)+as.factor(crtb_use)+as.factor(crt1_u)+as.factor(crt2_u),family = binomial(link = "logit"),data=data_2)->t

coeftest(t, vcov = vcovHC(t, type="HC1"))

t_RE%>%
  full_join(t_SCF, by=c("Survey","crt1_u","crt2_u","crtb_use"))%>%
  full_join(t_MA, by=c("Survey","crt1_u","crt2_u","crtb_use"))->t_full


t_RE%>%
  spread(RE,perc_RE,fill = 0)%>%
  mutate(Risky_Alternative=`50% chance of winning $1000 and 50% chance of winning $0`+`50% chance of losing $1000 and 50% chance of losing $0`,
         Safe_Alternative=`Win $500 for sure`+`Lose $500 for sure`)%>%
  select(Survey,crt1_u,crt2_u,crtb_use,Risky_Alternative,Safe_Alternative)->t_RE

print(xtable(t_RE))

count(data_2,Survey=="Blue")

count(data_2,treatment==1)

t_SCF%>%
  spread(SCF,perc_SCF)->t_SCF

t_MA%>%
  spread(MA,perc_MA)%>%
  full_join(t_SCF)%>%
  full_join(t_RE)->t

t%>%
  ungroup()%>%
  mutate(survey_1=ifelse(Survey=="Blue","Control","Treatment"),
         CRT_Q_1=ifelse(crt1_u==0,"Incorrect","Correct"),
         CRT_Q_2=ifelse(crt2_u==0,"Incorrect","Correct"),
         CRT_Q_both=ifelse(crtb_use==0,"Incorrect","Correct"))%>%
  select(survey_1,CRT_Q_1,CRT_Q_2,CRT_Q_both,"Buy_Ticket"=`Buy ticket`,"Would_Not_Buy_Ticket"=`Will not Buy ticket`,"Not_Go_To_Concert"=`No, I would not go to the concert.`,"Yes_Go_To_Concert"=`Yes, I would go to the concert.`,Risky_Alternative,Safe_Alternative)->t
  
t%>%
  ungroup()%>%
  select(Survey=survey_1,CRT_Q_1,CRT_Q_2,CRT_Q_both,Buy_Ticket,Would_Not_Buy_Ticket,Not_Go_To_Concert,Yes_Go_To_Concert,Risky_Alternative,Safe_Alternative)->t

t%>%
  arrange(Survey,desc(CRT_Q_both),desc(CRT_Q_2),desc(CRT_Q_1))->t

t


print(xtable(t%>%select(Survey,CRT_Q_1,CRT_Q_2,CRT_Q_both,Risky_Alternative,Safe_Alternative)%>%arrange(desc(CRT_Q_1),desc(CRT_Q_2),desc(CRT_Q_both),desc(Survey)),type="latex"))
  

wald.test(b = coef(glm_RE), Sigma = vcov(glm_RE), Terms = 3:5)

anova(glm_MA, test="Chisq")

anova(glm_SCF, test="Chisq")

anova(glm_RE, test="Chisq")

pR2(glm_MA)

pR2(glm_SCF)

pR2(glm_RE) 

summary(data_2)

coeftest(glm_RE, vcov = vcovHC(glm_RE, type="HC1"))

t


