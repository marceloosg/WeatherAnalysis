---
title: "Weather Impact Analisys"
author: "Marcelo Guimaraes"
date: "September 10, 2016"
---

#Summary
We report the weather events impact on economic and population health in United States. The data collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database were analysed for this report.  According to this analysis the greatest whether event affecting the population health is due to excessive **heat** which had a mortality and morbidity rating  .
The **flood** event caused the most economict impact where over **5*10^11 US$** in damages were estimated.

#Intro
There is great interest in the research of weather events. Since the resources provided to avert critical events are limited, the information of the events with the most impact on the population must be known in order to prioritize available resources to the most critical scenarios first, and to invest in the forecast and warning of those events. In this report data collected from the 
National Weather Service (NSW). The specification of the columns of the datasets are specified at this [link](http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx). Due to time constraints many of the sources used to produce these data were unverified and we quote 
"Accordingly, the NWS does not guarantee the accuracy
or validity of the information", read this report with that information in mind.

#Data Processing


##Gathering and aggregating Data

In order to investigate the economic and health impact of the weather events the following columns were selected: 

1. PROPDMG  - Property Damage
2. PROPDMGEXP - Scale (Exponent)
3. CROPDMG - Crop Damage 
4. CROPDMGEXP - Scale (Exponent)
5. FATALITIES - Number of deaths
6. INJURIES - Number of injuries 

The weather events registred by the EVTYPE columns are redundant. For example, "HEAT", "EXCESSIVE HEAT", "HIGH TEMPERATURE", all represents the same event. It is necessary to agragate these events into the same category before begining the analysis. The criteria used can be summarized in the following table:
```{r load-data,cache=T}
oldw <- getOption("warn")

source("scale.R")
library(data.table)
library(ggplot2)
storm.complete=data.table(read.csv("resources/repdata_data_StormData.csv"))
mapset=getEvents(storm.complete)
storm=applyMapset(storm.complete,mapset)
reduce.criteria=data.table(NewCategory=mapset[["keys"]], Criteria=mapset[["labels"]])
reduce.criteria
```
The column "NewCategory" is the new Event that will aggregate others. The column "Criteria" is the key word that is matched against the old events, the category "OTHER" collects the events not matched by the preceeding criteria. We apply each matched content in order and replace the old naming by the new one. It is possible to different Criterias to match the same Event, in that case the last Criteria is the effective one.

## Scaling
It can be seen that some exponents were not acurately specified ("?","+","-"). There are considered to be 0.
```{r,cache=T}
levels(storm$CROPDMGEXP)
levels(storm$PROPDMGEXP)
```

These scales are applied to PROPDMG and CROPDMG columns in order to obtain the TOTAL PROPERTY DAMAGE defined as the sum of CROP DAMAGE and PROPERTY DAMAGE.
The number of AFFECTED PEOPLE is defined as the sum of the number of FATALITIES and the number of people with INJURIES. Each quantity is grouped by each event:
```{r Apply-Scale,cache=T}
library(data.table)
storm$CROPDMG=ApplyScale(storm$CROPDMGEXP,storm$CROPDMG)
storm$PROPDMG=ApplyScale(storm$PROPDMGEXP,storm$PROPDMG)
TotalStorm=storm[,.(FATALITIES=sum(FATALITIES),INJURIES=sum(INJURIES),CROP_DAMAGE=sum(CROPDMG),PROPERTY_DAMAGE=sum(PROPDMG)),by=EVTYPE]
total=getTop("TOTAL",TotalStorm,T,5)
totalv=getTopValues("TOTAL",TotalStorm,5)
total
totalv
```

At first glance it seems that the FLOOD is the main cause of property damage and TORNADO has the main effect on peoples health.However, the Flood damages estimates are not as accurate as other events, and the conclusion may not be reliable, we quote the NWS directives: 
"The Storm Data preparer must enter monetary damage amounts for flood events, even if it is a 
**'guesstimate.'**  The U.S. Army Corps of Engineers requires the NWS to provide monetary 
damage amounts (property and/or crop) resulting from any flood event. 
". Therefore HURRICANE/TYPHOON may very well be the main source of weather related damages in the scenario which the flood damages are overestimated. 

To make sure that this analysis is realiable we visually explore the distribuition of those events on each year:
```{r,cache=T}
library(cowplot)
source("scale.R")
storm$Year=year(as.Date(gsub(" .*","",as.character(storm$BGN_DATE)),format="%m/%d/%Y"))
TotalStormYear=storm[,.(FATALITIES=sum(FATALITIES),INJURIES=sum(INJURIES),CROP_DAMAGE=sum(CROPDMG),PROPERTY_DAMAGE=sum(PROPDMG)),by=.(EVTYPE,Year)]
TotalStormYear[,TOTAL_DAMAGE:=CROP_DAMAGE+PROPERTY_DAMAGE]
TotalStormYear[,TOTAL_HEALTH_DAMAGE:= FATALITIES+INJURIES]

events=unique(as.array(as.matrix(total[,-1,with=FALSE]))[1:15])
datas=c("1950","1972","1982", "1992","2012")
ndatas=as.numeric(datas)

plot_dmg=ggplot(data=TotalStormYear[EVTYPE %in% events],aes(x=Year,y=log(1+TOTAL_DAMAGE),colour=EVTYPE))+geom_point()+ylab("LOG(1+TOTAL DAMAGE (US$))")+scale_x_continuous(breaks=ndatas,labels=datas)+
        geom_vline(xintercept=1992)


        
plot_health=ggplot(data=TotalStormYear[EVTYPE %in% events],aes(x=Year,y=log(1+TOTAL_HEALTH_DAMAGE),colour=EVTYPE))+geom_point()+scale_x_continuous(breaks=ndatas,labels=datas)+geom_vline(xintercept=1982)+geom_vline(xintercept=1992)+ylab("LOG(1+# PEOPLE KILLED/INJURIED)")
        

multiplot(plot_dmg,plot_health)
#plot_grid(plot_dmg, plot_health, labels=c("Economic Impact/Year", "Health Impact/Year"), ncol = 2, nrow = 1)
```

It is clear from the data that the properties damages was recorded only for TORNADOS until 1992. Similarly FLOOD fatalities were included only after 1982 and other weather related fatalities/injuries were included after 1992. Lets cut the data before 1993 and analyse between FLOOD and HURRICANE damaging events:

```{r,cache=T}
flood=ggplot(data=TotalStormYear[EVTYPE %in% c("FLOOD","HURRICANE/TYPHOON","TORNADO") & Year>1992],aes(x=Year,y=TOTAL_DAMAGE,colour=EVTYPE))+geom_line()+ylab("TOTAL DAMAGE (US$)")+geom_point()
most.damaging.event=storm[which.max(storm$PROPDMG)]

```

```{r,cache=T}
TotalStorm93=storm[Year >1992,.(FATALITIES=sum(FATALITIES),INJURIES=sum(INJURIES),CROP_DAMAGE=sum(CROPDMG),PROPERTY_DAMAGE=sum(PROPDMG)),by=EVTYPE]
post92=getTop("POST 92 TOTAL",TotalStorm93,T,5)
post92v=getTopValues("POST 92 TOTAL",TotalStorm93,5)


post92
post92v
```

Now it appears that HEAT is the main cause of fatalities. In these analysis we failed to account for population growth and inflation. To consider this impact a ![consumer price index](http://www.usinflationcalculator.com/inflation/historical-inflation-rates/) and a ![population](http://www.census.gov/population/international/data/idb/region.php) table are obtained for this purpose.

#Results

```{r,cache=T}
#Account for inflation
rates=data.table(read.csv("inflation.rates.csv",header = T,sep=";"))
aux=1
rate=rates[which(with(rates,Year>1992 & Year < 2012))]
rate$Acum=0
for(i in (length(rate$Year)-1):1){
        aux=aux*(1+as.numeric(as.character(rate[i+1,]$Ave))/100)
        rate$Acum[i]=aux
}
rate$Acum[length(rate$Year)]=1
rate=rate[,c(1,15),with=F]
economic=storm[Year >1992,.(TOTAL_PROPERTY_DAMAGE=sum(CROPDMG+PROPDMG)),by=.(EVTYPE,Year)]
economic.impact=merge(economic,rate,by="Year",all=T)
economic.impact=economic.impact[,.(TOTAL_PROPERTY_DAMAGE=sum(TOTAL_PROPERTY_DAMAGE*Acum)),by=EVTYPE]
setorder(economic.impact,-TOTAL_PROPERTY_DAMAGE)
ei=melt(head(economic.impact,8),id.vars = "EVTYPE")
cacPalette=c("black","red","blue", "#CCCCFF","dark blue", "dark grey", "cyan", "yellow")
economic.plot=ggplot(data=ei,aes(x=variable ,y=value,fill=EVTYPE,order=value)) + geom_bar(stat="identity")+scale_fill_manual(values=cacPalette)+ylab("Acumulated Damages (values in US$ corrected to 2011)")+xlab("")+ggtitle("Total Acumulated Damage by Weather Event")

#Account for populational growth
growth=data.table(read.csv("population.csv",header =T,sep=";"))
growth=growth[which(with(growth,Year>1992 & Year < 2012))]
growth=growth[,c(1,2),with=F]
health=storm[Year >1992,.(MORTALITIES=sum(FATALITIES),MORBIDITIES=sum(FATALITIES+INJURIES)),by=.(EVTYPE,Year)]
health.impact=merge(health,growth,by="Year",all=T)
health.impact$MORTALITIES=health.impact$MORTALITIES*100000/health.impact$Population
health.impact$MORBIDITIES=health.impact$MORBIDITIES*100000/health.impact$Population
health.impact=health.impact[,.(MORTALITIES=mean(MORTALITIES,na.rm=T),
                               MORBIDITIES=mean(MORBIDITIES,na.rm=T)),by=EVTYPE]
setorder(health.impact,-MORTALITIES,-MORBIDITIES)
head(health.impact,1)
hi=melt(head(health.impact,8),id.vars = "EVTYPE")
cbcPalette=c("light blue","#CCCCFF","blue","red", "dark grey","dark blue",  "cyan", "yellow")
health.plot=ggplot(data=hi,aes(x=variable ,y=value,fill=EVTYPE,order=value)) + geom_bar(stat="identity")+scale_fill_manual(values=cbcPalette)+ylab("# of People (per 100000)")+xlab("")+ggtitle("Time Average of Mortality and Morbidity per Weather Event")
multiplot(economic.plot,health.plot)
```   

After reviewing the data and removing several bias, It can be concluded that FLOOD is the main event responsible for the economic impact of Weather events and Heat is the event that physically affects the population the most. 
