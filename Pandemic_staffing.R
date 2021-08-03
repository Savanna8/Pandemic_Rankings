# set up R work space

options(stringsAsFactors = FALSE, scripen = 999)

# set working directory

save.image("OhioNH.RData")
load("OhioNH.RData")

# call packages

library(tidyverse) 
library(dplyr)
library(DescTools)
library(psych)
library(fastDummies)

# import data

"Total"=read.csv("NH6.27.21.csv")

#Separate for only NH that submitted data
DataUS = Total[Total$Submitted.Data == "Y",]

#Separate for only NH that submitted data in Ohio
OhioData = DataUS[DataUS$Provider.State == "OH",]

###Look at shortages for key weeks (first, peak, latest)

#Find shortage for the first week, 05/24/20, for Ohio
OH52420=OhioData[OhioData$Week.Ending == "05/24/20",]
length(OH52420$Shortage.of.Aides[OH52420$Shortage.of.Aides == "Y"])
207/874

#Find shortage for the first week, 05/24/20, for the US
US52420 = DataUS[DataUS$Week.Ending == "05/24/20",]
length(US52420$Shortage.of.Aides[US52420$Shortage.of.Aides =="Y"])
2587/14563

#Find shortage for 12/13/20 (Ohio's covid cases peak) for Ohio
OH121320 = OhioData[OhioData$Week.Ending =="12/13/20",]
length(OH121320$Shortage.of.Aides[OH121320$Shortage.of.Aides == "Y"])
282/948

#Look at US 12/13/20
US121320= DataUS[DataUS$Week.Ending == "12/13/20",]
length(US121320$Shortage.of.Aides[US121320$Shortage.of.Aides == "Y"])
3053/15231

#Look at Ohio for 6/6/21 (give the NHs/data a two-ish week window to get caught up-Applebaum)
#Update with most current numbers (but 2-3 weeks delayed) pre-publication
OH060621 = OhioData[OhioData$Week.Ending =="06/06/21",]
length(OH060621$Shortage.of.Aides[OH060621$Shortage.of.Aides == "Y"])
256/948

#Look at US for 6/6/21
US060621= DataUS[DataUS$Week.Ending == "06/06/21",]
length(US060621$Shortage.of.Aides[US060621$Shortage.of.Aides == "Y"])
2671/15206

### Rank states by shortages

#Remove unnecessary columns, focusing on staffing shortages
USsmall = DataUS[-7:-65]
USsmall = USsmall[-10:-220]

#Separate files by year by separating out the month, day, year

USsmall = transform(USsmall,Date = as.character(Week.Ending))
USsplit = strsplit(USsmall$Week.Ending, split = "/")
USsmall = transform(USsmall, 
                    Date1=sapply(USsplit,"[[",1), 
                    Date2=sapply(USsplit, "[[",2),
                    Date3=sapply(USsplit, "[[",3))

#Make vectors for necessary info for ranking, based on rows from 2020
aides20 = USsmall$Shortage.of.Aides[grep("20",USsmall$Date3)]
nurse20 = USsmall$Shortage.of.Nursing.Staff[grep("20",USsmall$Date3)]
month20= USsmall$Date1[grep("20",USsmall$Date3)]
state20= USsmall$Provider.State[grep("20",USsmall$Date3)]
name20 = USsmall$Provider.Name[grep("20",USsmall$Date3)]

#Create teh 2020 data frame
Y20=data.frame(Provider.Name=name20,
               Provider.State=state20,
               Month=month20,
               Shortage.of.Aides=aides20,
               Shortage.of.Nurses=nurse20)

#Make dummy columns so that the binary (y/n) is numeric and separated

dumY20 = dummy_cols(Y20, select_columns=c("Shortage.of.Aides"))

#Group by staff and find the percentage of shortage

FinalY20 = dumY20 %>% group_by(Provider.State) %>%
  summarise(Count_shortages=sum(Shortage.of.Aides_Y),
            Percentage_shortages=
              Count_shortages/
              sum(Shortage.of.Aides_Y,Shortage.of.Aides_N)*100)

FinalY20=FinalY20[order(FinalY20$Percentage_shortages, decreasing=TRUE),]

write.csv(FinalY20, "FinalY20.csv", fileEncoding = "UTF-8")

#Do the same for 2021

aides21 = USsmall$Shortage.of.Aides[grep("21",USsmall$Date3)]
nurse21 = USsmall$Shortage.of.Nursing.Staff[grep("21",USsmall$Date3)]
month21= USsmall$Date1[grep("21",USsmall$Date3)]
state21= USsmall$Provider.State[grep("21",USsmall$Date3)]
name21 = USsmall$Provider.Name[grep("21",USsmall$Date3)]

Y21=data.frame(Provider.Name=name21,
               Provider.State=state21,
               Month=month21,
               Shortage.of.Aides=aides21,
               Shortage.of.Nurses=nurse21)

dumY21 = dummy_cols(Y21, select_columns=c("Shortage.of.Aides"))


FinalY21 = dumY21 %>% group_by(Provider.State) %>%
  summarise(Count_shortages=sum(Shortage.of.Aides_Y),
            Percentage_shortages=
              Count_shortages/
              sum(Shortage.of.Aides_Y,Shortage.of.Aides_N)*100)

FinalY21=FinalY21[order(FinalY21$Percentage_shortages, decreasing=TRUE),]

write.csv(FinalY21, "FinalY21.csv", fileEncoding = "UTF-8")