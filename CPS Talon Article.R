#Load Files
cps_ddi <- read_ipums_ddi("/Users/ayushkumar/Downloads/cps_00025.xml")
cps_data<-read_ipums_micro(cps_ddi, verbose = FALSE)
#
cps<-cps_data
#number of people in college
cps%>%
  filter(SCHLCOLL %in% c(3,4),EMPSTAT %in% c(21,22))%>%
  group_by(YEAR,MONTH,SCHLCOLL)%>%
  summarise(n=sum(WTFINL))%>%
  unite(YEAR,MONTH,YEAR:MONTH,sep = "-")%>%
  mutate(category=case_when(SCHLCOLL==3~"College or university full time",SCHLCOLL==4~"College or university part time"))%>%
  select(date=YEAR,category,unemployed=n)->t
t$date<-mdy(t$date)
unemployed<-t
#mutate race
cps%>%
  mutate(race=case_when(RACE==100 & HISPAN==0~"Non-Hispanic White Students",
                        RACE==200 & HISPAN==0~"Black Students",
                        HISPAN!=0~"Hispanic Students"))->cps
#number of people in labor force
cps%>%
  filter(SCHLCOLL==3,EMPSTAT %in% c(21,22))%>%
  group_by(YEAR,MONTH,SCHLCOLL,race)%>%
  summarise(n=sum(WTFINL))%>%
  unite(YEAR,MONTH,YEAR:MONTH,sep = "-")%>%
  mutate(category=case_when(SCHLCOLL==3~"College or university full time",SCHLCOLL==4~"College or university part time"))%>%
  select(date=YEAR,race,unemp_coll=n)%>%ungroup()->t
#
cps%>%
  filter(SCHLCOLL==3,EMPSTAT %in% c(10:22))%>%
  group_by(YEAR,MONTH,SCHLCOLL,race)%>%
  summarise(n=sum(WTFINL))%>%
  unite(YEAR,MONTH,YEAR:MONTH,sep = "-")%>%
  mutate(category=case_when(SCHLCOLL==3~"College or university full time",SCHLCOLL==4~"College or university part time"))%>%
  select(date=YEAR,race,labor_coll=n)%>%ungroup()->t_2
#
t%>%full_join(t_2)->t_3
t_3%>%
  mutate(unemp_coll_rate=(unemp_coll/labor_coll)*100)->t_3
#
t_3$date<-mdy(t_3$date)
#
t_3$date_2<-rep(seq(as.Date("2019/01/01"), as.Date("2020/06/01"), "months"),4)%>%sort()
#
ggplot(t_3%>%filter(race %in% c("Black Students","Hispanic Students","Non-Hispanic White Students")),aes(x=date_2,y=unemp_coll_rate,color=race))+
  geom_line(lty=2)+
  geom_line(x_3,mapping = aes(x=date_2,y=unemp_rate,color=race))+
  geom_vline(xintercept = as.Date("2020-03-01"),lty=2,color="black")+
  scale_x_date(date_labels = "%b-%y",date_breaks  ="1 month")+
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks=c(5,10,15,20,25,30,35,40))+
  labs(x="Month of Observation",y="Unemployment Rate",caption="Source: Current Population Survery, The Bureau of Census",subtitle = "All Figures are Seasonally Unadjusted",title = "Unemployment Rate Among Full-Time College Students in U.S.")+
  theme_classic()+
  theme(legend.position = "bottom",legend.title=element_blank())+
  theme(text=element_text(family="Palatino"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggsave("talon.png", width=8, height=5, dpi=900)
#
cps%>%
  filter(EMPSTAT %in% c(21,22))%>%
  group_by(YEAR,MONTH)%>%
  summarise(unemployed=sum(WTFINL))%>%
  ungroup()%>%
  unite(YEAR,MONTH,YEAR:MONTH,sep = "-")->x
#
cps%>%
  filter(EMPSTAT %in% c(10:22))%>%
  group_by(YEAR,MONTH)%>%
  summarise(lab_force=sum(WTFINL))%>%
  ungroup()%>%
  unite(YEAR,MONTH,YEAR:MONTH,sep = "-")->x_2
#
x%>%
  full_join(x_2)->x_3
#
x_3$date_2<-seq(as.Date("2019/01/01"), as.Date("2020/06/01"), "months")
#
x_3%>%
  mutate(unemp_rate=(unemployed/lab_force)*100,race="National Unemployment Rate")->x_3
