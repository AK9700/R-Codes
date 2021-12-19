
#This file is written in R
#Overview: I was asked by an academic center to collapse grid-level dataset from India to 
#a district level dataset by estimating total rainfall and mean rainfall/temperature. The other tasks included graphing daily rainfall, and fitting a fixed effects model by regressing rainfall and total #rainfall on #temperature.

#install packages in not already present 
#install.packages("tidyverse")
#install.packages("geodist")
#install.packages("stats")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("xtable")
install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("stargazer")


#Load library
library(tidyverse)
library(geodist)
library(stats)
library(stringr)
library(lubridate)
library(xtable)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

#The central task of the first section is to collapse the grid-level dataset into a district-level #dataset. The algorithm to match each grid point to a district is as follows: take a weighted average #of daily mean temperature, daily mean rainfall, and daily total rainfall for all grid points within #100 km of each district’s geographic center. The weights are the inverse of the squared distance #from the district center. 

#Response

#Load datasets
geo<-read.csv("/Users/ayushkumar/Downloads/Files For Applicant/Geo/district_crosswalk_small.csv")

for (i in 2009:2013){
  assign(paste0("rainfall_",i),
         read.csv(paste0("/Users/ayushkumar/Downloads/Files For Applicant/Rainfall/rainfall_",
                         i,".csv"))
  )
}

for (i in 2009:2013){
  assign(paste0("temperature_",i),
         read.csv(paste0("/Users/ayushkumar/Downloads/Files For Applicant/Temperature/temperature_",
                         i,".csv"))
  )
}

#Calculating distance for rainfall_i datasets by recursively adding column 
#measuring geodist from each centroid
for( i in 2009:2013){
  for (j in 1:nrow(geo)){
    assign(paste0("rainfall_",i),
           get(paste0("rainfall_",i))%>%
             add_column(geodist_vec(get(paste0("rainfall_",i))$longitude,
                                    get(paste0("rainfall_",i))$latitude,
                                    rep(geo[j,5],nrow(get(paste0("rainfall_",i)))),
                                    rep(geo[j,6],nrow(get(paste0("rainfall_",i)))),
                                    paired=T,measure = "geodesic")/1000))
  }
}

#Changing column names for rainfall_i variables
l_1<-c("date","latitude","longitude","rainfall","year","month","day")

for (j in 1:nrow(geo)){
  append(l_1,paste0("dist_",geo[j,7]))->l_1
}

colnames(rainfall_2009)<-l_1
colnames(rainfall_2010)<-l_1
colnames(rainfall_2011)<-l_1
colnames(rainfall_2012)<-l_1
colnames(rainfall_2013)<-l_1

#Calculating distance for temperature_i datasets by recursively adding column 
#measuring geodist from each centroid
for( i in 2009:2013){
  for (j in 1:nrow(geo)){
    assign(paste0("temperature_",i),
           get(paste0("temperature_",i))%>%
             add_column(geodist_vec(get(paste0("temperature_",i))$longitude,
                                    get(paste0("temperature_",i))$latitude,
                                    rep(geo[j,5],nrow(get(paste0("temperature_",i)))),
                                    rep(geo[j,6],nrow(get(paste0("temperature_",i)))),
                                    paired=T,measure = "geodesic")/1000))
  }
}

#Changing column names
l_2<-c("date","latitude","longitude","temperature","day","month","year")

for (j in 1:nrow(geo)){
  append(l_2,paste0("dist_",geo[j,7]))->l_2
}

colnames(temperature_2009)<-l_2
colnames(temperature_2010)<-l_2
colnames(temperature_2011)<-l_2
colnames(temperature_2012)<-l_2
colnames(temperature_2013)<-l_2


# Converting grid-level data to district level data for rainfall data sets

#For each year i and centroid j we filter the grids within 100 km distance to create 
#Variables total rainfall and weighted mean rainfall in file rf_i_j
for (i in 2009:2013){
  for (j in 1:nrow(geo)){
    assign(paste0("rf_",i,"_",geo[j,7]),
           get(paste0("rainfall_",i))%>%
             filter(get(paste0("dist_",geo[j,7]))<=100)%>%
             group_by(date)%>%
             summarise(
               dm_rain=weighted.mean(rainfall,1/(get(paste0("dist_",geo[j,7])))^2,na.rm=T),
               total_rain=sum(rainfall,na.rm = T))%>%
             mutate(unique_dist_id=geo[j,7])%>%
             left_join(geo))
  }
}

rf_2009<-rf_2009_1711
rf_2010<-rf_2010_1711
rf_2011<-rf_2011_1711
rf_2012<-rf_2012_1711
rf_2013<-rf_2013_1711

#Now we merge all rf_i_j to rf_i
for (i in 2009:2013){
  for (j in 2:nrow(geo)){
    assign(paste0("rf_",i),
           full_join(get(paste0("rf_",i)),
                     get(paste0("rf_",i,"_",geo[j,7])))) 
  }
}

# Converting grid-level data to district level data for temperature data sets

#For each year i and centroid j we filter the grids within 100 km distance to create 
#Variables weighted mean temp in file temp_i_j
for (i in 2009:2013){
  for (j in 1:nrow(geo)){
    assign(paste0("temp_",i,"_",geo[j,7]),
           get(paste0("temperature_",i))%>%
             filter(get(paste0("dist_",geo[j,7]))<100)%>%
             group_by(date)%>%
             summarise(
               dm_temp=weighted.mean(temperature,1/(get(paste0("dist_",geo[j,7])))^2,na.rm=T))%>%
             mutate(unique_dist_id=geo[j,7])%>%
             left_join(geo))
  }
}


temp_2009<-temp_2009_1711
temp_2010<-temp_2010_1711
temp_2011<-temp_2011_1711
temp_2012<-temp_2012_1711
temp_2013<-temp_2013_1711

#Now we merge all temp_i_j to temp_i

for (i in 2009:2013){
  for (j in 2:nrow(geo)){
    assign(paste0("temp_",i),
           full_join(get(paste0("temp_",i)),
                     get(paste0("temp_",i,"_",geo[j,7])))) 
  }
}

#Merging rf_i's to create data and temp_i's to create data_2 and creating the final data file
data<-rf_2009
data_2<-temp_2009

for(i in 2010:2013){
  data<-full_join(data,get(paste0("rf_",i)))
}

for(i in 2010:2013){
  data_2<-full_join(data_2,get(paste0("temp_",i)))
}

#viewing data files
view(data)
view(data_2)

#Notice while data's date is arranged ymd data_2's date is not so before we merge them
#Some modifications are essential, we will change data_2's date to ymd format

#Fist convert data_2's date from numeric to character format
data_2$date<-as.character(data_2$date)

#Split the variable into a list
strsplit(data_2$date,split = " ")->l

#Create 5 empty lists
vector(mode = "list", length = length(l))->y

vector(mode = "list", length = length(l))->m

vector(mode = "list", length = length(l))->d

vector(mode = "list", length = length(l))->date

vector(mode = "list", length = length(l))->date_2

#Extract year from the string by subsetting last 4 characters and entering them in list y
for (i in 1:length(l)){
  str_sub(l[[i]],-4,-1)->y[[i]]
}

#Extract month from the string by subsetting 6,7 character from left characters and entering them in list m
for (i in 1:length(l)){
  str_sub(l[[i]],-6,-5)->m[[i]]
}

#Extract day from the string by subsetting digits to the right of 7th character
#from left characters and entering them in list d

for (i in 1:length(l)){
  str_sub(l[[i]],end=-7)->d[[i]]
}

#Now combine ymd as date
for (i in 1:length(l)){
  paste0(y[[i]],m[[i]],d[[i]])->date[[i]]
}

#Notice as day 1 is recorded as 1 in temperature data set and not 01 as in 
#rainfall datasets we might want to make that modification

#Computing the length of each element in ymd dataset
for (i in 1:length(l)){
  floor(log10(as.numeric(paste0(y[[i]],m[[i]],d[[i]]))))+1->date_2[[i]]
}

#As we need to change 1 to 01 we will insert 0 in the strings of list date
#To do so we will use the following function
fun_insert <- function(x, pos, insert) {       
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

#Now we for every element in list date of length 7 we insert 0 after 6th element 
for (i in 1:length(l)){
  if (date_2[[i]]==7){
    fun_insert(paste0(strsplit(date[[i]],split = "")[[1]],collapse = ""), pos=6,"0")->date[[i]]
  }
}

#Now we unlist the date convert it to numeric and substituting them as date for data_2
as.numeric(unlist(date))->data_2$date

#Now as the format for dates match we can merge data and data_2
full_join(data_2,data)->data

#We remove the columns we don't need and save the file as fdata
data%>%
  dplyr::select(date,stname_iaa,distname_iaa,
                rainfall=dm_rain,temperature=dm_temp,
                total_rainfall=total_rain,unique_dist_id)->fdata

#We now convert the date column of fdata from a numeric to date datatype the strategy 
#Is very similar to the one employed in changing format of date in data_2 file 

#splitting date column of fdata as character and creating empty lists 
strsplit(as.character(fdata$date),split = " ")->l

vector(mode = "list", length = length(l))->y

vector(mode = "list", length = length(l))->m

vector(mode = "list", length = length(l))->d

vector(mode = "list", length = length(l))->date

#Extracting year, month, and date information from fdata's date and entering them in lists y,m, and , d
#respectively
for (i in 1:length(l)){
  str_sub(l[[i]],1,4)->y[[i]]
}

for (i in 1:length(l)){
  str_sub(l[[i]],5,6)->m[[i]]
}

for (i in 1:length(l)){
  str_sub(l[[i]],7,8)->d[[i]]
}

#Finally creating date_2 a column in fdata which is the representation of date column as date data type
fdata%>%
  mutate(year=as.numeric(unlist(y)),
         month=as.numeric(unlist(m)),
         day=as.numeric(unlist(d)),
         date_2=ymd(paste0(y,"-",m,"-",d)))->fdata

#Now we remove residual columns make changes form column name and export the file
fdata%>%
  dplyr::select(Date=date_2,State=stname_iaa,
                District=distname_iaa,
                Rainfall=rainfall,Temperature=temperature,
                `Total Rainfall`=total_rainfall)->fdata_exp

fdata_exp$State<-str_to_title(fdata_exp$State)

fdata_exp$District<-str_to_title(fdata_exp$District)

#write.csv(fdata_exp,"/Users/ayushkumar/Desktop/E & E Lab Responses/fdata.csv")

#2. Data Exploration In this section I will also mention the question as it would give context
#for what I am doing

#1. For the Jaipur district in Rajasthan, 
#create a time series dataset of daily rainfall, averaged over the five years. 

#Making the time-series data and storing it as ts
fdata%>%
  group_by(day,month)%>%
  filter(distname_iaa=="jaipur",stname_iaa=="rajasthan")%>%
  summarise(rain_avg=mean(rainfall))%>%ungroup()%>%
  mutate(date=ymd(paste0("2012","-",as.character(month),"-",as.character(day))))->ts

# 2.	Using this time series, create a scatterplot of rainfall by day. 
#On what day does the monsoon season start in Jaipur? When does it end? 
#Indicate your answer on the graph. 

#Graph
ggplot(ts)+
  geom_point(aes(x=date,y=rain_avg,color="pink"))+
  geom_rect(aes(xmin=ymd("2012-06-01"), xmax=ymd("2012-09-30"), ymin=-Inf, ymax=Inf),fill="gray92")+
  geom_point(aes(x=date,y=log(rain_avg),color="pink"))+
  scale_x_date(breaks = seq(ymd("2012-01-01"),ymd("2012-12-01"),by = '1 month'), date_labels =  "%b",
               limits = as.Date(c('2012-01-01','2012-12-31'), format="%Y/%m/%d"))+
  labs(x="Month of Observation",y="Daily Rainfall",
       title="Daily Rainfall, Averaged from 2009 to 2013, for Jaipur, Rajasthan",
       caption ="Note: Shaded region depicts the period of observed monsoon season in Jaipur, Rajasthan.")+
  theme_classic()+
  theme(legend.position = "")

#ggsave("/Users/ayushkumar/Desktop/E & E Lab Responses/img_1.png")

#Clearly, the variability in average rain is too high hence, it makes sense that we use log values
#for clarity of image. 

#Graph with log values
ggplot(ts)+
  geom_point(aes(x=date,y=log(rain_avg),color="pink"))+
  geom_rect(aes(xmin=ymd("2012-06-01"), xmax=ymd("2012-09-30"), ymin=-Inf, ymax=Inf),fill="gray92")+
  geom_point(aes(x=date,y=log(rain_avg),color="pink"))+
  scale_x_date(breaks = seq(ymd("2012-01-01"),ymd("2012-12-01"),by = '1 month'), date_labels =  "%b",
               limits = as.Date(c('2012-01-01','2012-12-31'), format="%Y/%m/%d"))+
  labs(x="Month of Observation",y="Log Value of Daily Rainfall",
       title="Log Value of Daily Rainfall, Averaged from 2009 to 2013, for Jaipur, Rajasthan",
       caption ="Note: Shaded region depicts the period of observed monsoon season in Jaipur, Rajasthan. Moreover, the points
       clustered very close to the x-axis are the days when Jaipur, Rajasthan experienced no rain.")+
  theme_classic()+
  theme(legend.position = "")

#ggsave("/Users/ayushkumar/Desktop/E & E Lab Responses/img_2.png")

#3.	Format your graphs so that they are publication-quality and save them as .pdf files. 
#Create a publication-quality table of annual average temperature by state and year.

#Filtering, finding average and getting the latex code for the table
print(xtable(fdata%>%
               group_by(year,stname_iaa)%>%
               summarise(avg_temp=mean(temperature))%>%
               spread(stname_iaa,avg_temp)%>% mutate_if(is.numeric, ~round(., 1)), type = "latex"))



#3. Fit a fixed effects model and a OLS model, and check if fixed effects is needed
# finally export the better model in form of a publication quality table 


#Creating levels
fdata%>%
  mutate(id=paste0(State,"-",District))%>%
  select(Date,id,State,District,Rainfall,Total.Rainfall,Temperature)->fdata

#Declare panel data
f_pd<-pdata.frame(fdata,index=c("id","Date"), drop.index=TRUE, row.names=TRUE)


#Fitting model

mod<-plm(Temperature~Rainfall+Total.Rainfall,data = f_pd, effect = "twoways")

ols<-lm(Temperature~Rainfall+Total.Rainfall,data=fdata)

summary(mod)

#getting robust standard errors
coeftest(mod, vcov = vcovHC, type = "HC1")

#Lets see if the fixed effects model is better
pFtest(mod, ols)
#Turns out it is


#Getting a publication quality table
stargazer(mod)





