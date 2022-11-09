

rm(list = ls())
#.rs.restartR()
gc()

#----Set Working Dir----
setwd("C:/DATA/MIS/PigData/Dec2020/")

write.path<-"C:/Documents/Manuscripts/Feral Swine - MIS Data Description/Data/"


#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(modeest)
library(operators)
library(utils)


#----Required Functions
source("C:\\Documents\\Manuscripts\\Feral Swine - MIS Data Description\\Code\\FNC.MIS.calc.aerial.chronology.R")
source("C:\\Documents\\Manuscripts\\Feral Swine - MIS Data Description\\Code\\FNC.Misc.Utilities.R")


#----Prep Data ----

# Read data
dat.Agr<-read.csv("processed.PropertyPull7Jan2021.csv",stringsAsFactors=FALSE)

dat.Kill<-read.csv("processed.PigTakePropMeth16Dec2020.csv",stringsAsFactors=FALSE)

dat.Eff<-read.csv("processed.Effort16Dec2020.csv",stringsAsFactors=FALSE)

dat.PropKill<-read.csv("processed.PigTakeByProperty16Dec2020.csv",stringsAsFactors=FALSE)

lut.property.acres<-read.csv("processed.lut.property.acres.csv",stringsAsFactors=FALSE)

# Convert Dates to R Dates
dat.Eff$WT_WORK_DATE <- as.Date(as.character(dat.Eff$WT_WORK_DATE),"%Y-%m-%d")
dat.PropKill$WT_WORK_DATE <- as.Date(as.character(dat.PropKill$WT_WORK_DATE),"%Y-%m-%d")

##----END DATA PREP----



#--Subset Data
aerial.vec <- c("HELICOPTER","FIXED WING")

tmp<-dat.Eff[dat.Eff$CMP_NAME %in% aerial.vec,]
tmp<-tmp[tmp$UOM_NAME=="HOBBS METER",]

unique(tmp$CMP_NAME)
unique(tmp$CMP_TYPE)
count(tmp$USET_NAME)

tmp<-tmp[tmp$USET_NAME %not in% c("DISCHARGED"),]

#--Remove implosable values
summary(tmp$WTM_QTY)
summary(tmp$WTCM_QTY)

tmp$Flight.Hours <- tmp$WTCM_QTY*tmp$WTM_QTY
tmp$Flight.Days <- (tmp$WTCM_QTY/24)*tmp$WTM_QTY


wide.data <-aggregate(cbind(WTM_QTY,WTCM_QTY,Flight.Hours,Flight.Days)~ALWS_AGRPROP_ID+AGRP_PRP_ID+CMP_NAME+WT_WORK_DATE, data=tmp, FUN=sum)

#Ensure data is ordered
wide.data <- wide.data[order(wide.data$AGRP_PRP_ID, wide.data$ALWS_AGRPROP_ID, wide.data$WT_WORK_DATE),, drop=FALSE]

colnames(wide.data)[which(colnames(wide.data)=="WTM_QTY")] <- "HOURS"
colnames(wide.data)[which(colnames(wide.data)=="WTCM_QTY")] <- "VEHICLES"

in.dat<-wide.data

wide.data[order(-wide.data$VEHICLES),]

##END


#----Generate chronology


#------------------------
#----Generate effort ----

#-----------------------------
#----Generate trap effort ----

#Subset to area of interest
#trap.dat<-in.dat[in.dat$AGRP_PRP_ID %in% unique.properties,]

trap.dat<-in.dat

#Generate trap type list to process
trap.vec <- unique(in.dat$CMP_NAME)

#--Remove implosable values
summary(trap.dat$HOURS)
summary(trap.dat$VEHICLES)
summary(trap.dat$Flight.Hours)

par(mfrow=c(3,1))
hist(trap.dat$HOURS,breaks=100,xlab="Hours",main="Hours")
hist(trap.dat$VEHICLES,breaks=100,xlab="Vehicles",main="Vehicles")
plot(trap.dat$HOURS,trap.dat$VEHICLES,xlab="Hours",ylab="Vehicles",main="Vehicles vrs Hours")

#Restrict number of vehicles
#nrow(trap.dat[trap.dat$VEHICLES>3,])
#trap.dat <- trap.dat[trap.dat$VEHICLES<=3,]
#nrow(trap.dat)

#Restrict hours
#nrow(trap.dat[trap.dat$HOURS>10,])
#trap.dat <- trap.dat[trap.dat$HOURS<=10,]
#nrow(trap.dat)

#trap.dat[trap.dat$ALWS_AGRPROP_ID=="366874" & trap.dat$AGRP_PRP_ID=="370276",]



#----Generate trap chronology for each trap type

harvest.chronology<-generate.trap.chronology(trap.dat, dat.PropKill, trap.vec, time.thershold=25)
nrow(harvest.chronology)

#----Generate summary of trap nights and kill by each trapping event
agg.out.dat <- aggregate(cbind(HOURS,VEHICLES,Flight.Hours,Flight.Days,Take)~AGRP_PRP_ID+ALWS_AGRPROP_ID+event.id+CMP_NAME, data=harvest.chronology, FUN=sum)
agg.out.dat <- agg.out.dat[order(agg.out.dat$AGRP_PRP_ID, agg.out.dat$event.id),]
nrow(agg.out.dat)
#agg.out.dat[agg.out.dat$AGRP_PRP_ID==224386,]


#----Make start and end dates for aggregated data
str.date <- aggregate(WT_WORK_DATE~event.id+AGRP_PRP_ID+ALWS_AGRPROP_ID+CMP_NAME, data=harvest.chronology, FUN=min)
end.date <- aggregate(WT_WORK_DATE~event.id+AGRP_PRP_ID+ALWS_AGRPROP_ID+CMP_NAME, data=harvest.chronology, FUN=max)

dates.event <- merge(str.date,end.date,by=c("event.id","AGRP_PRP_ID","ALWS_AGRPROP_ID","CMP_NAME"))
colnames(dates.event)<-c("event.id","AGRP_PRP_ID","ALWS_AGRPROP_ID","CMP_NAME","Start.Date","End.Date")

dates.event$event.length <- dates.event[,"End.Date"]-dates.event[,"Start.Date"]
dates.event[dates.event$event.length==0,"event.length"]<-1

agg.out.dat <- merge(agg.out.dat, dates.event, by=c("event.id","AGRP_PRP_ID","ALWS_AGRPROP_ID","CMP_NAME"))

agg.out.dat <- agg.out.dat[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","event.id","CMP_NAME","Start.Date","End.Date","event.length","HOURS","VEHICLES","Flight.Hours","Flight.Days","Take")]

agg.out.dat <- agg.out.dat[order(agg.out.dat$AGRP_PRP_ID,agg.out.dat$event.id),, drop=FALSE]


#----Merge County location data

#Generate final data
final.agg.out.dat <- merge(agg.out.dat, lut.property.acres, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID"),all.x=TRUE)
final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","event.id","ST_NAME","CNTY_NAME", "ST_FIPS", "CNTY_FIPS","FIPS",
                                          "Start.Date","End.Date",
                                          "COUNTY.OR.CITY.LAND", "FEDERAL.LAND","MILITARY.LAND","PRIVATE.LAND","STATE.LAND","TRIBAL.LAND", "TOTAL.LAND",
                                          "CMP_NAME", "HOURS", "VEHICLES", "Flight.Hours","Flight.Days","Take")]
final.agg.out.dat <- final.agg.out.dat[order(final.agg.out.dat$AGRP_PRP_ID,final.agg.out.dat$event.id),]
nrow(final.agg.out.dat)

final.agg.out.dat<-final.agg.out.dat[is.na(final.agg.out.dat$AGRP_PRP_ID)==FALSE,]
nrow(final.agg.out.dat)

#Remove those with no FIPS Code thus no area values
final.agg.out.dat<-final.agg.out.dat[is.na(final.agg.out.dat$FIPS)==FALSE,]
nrow(final.agg.out.dat)

#----END fill in missing values


#----Write Data
write.csv(final.agg.out.dat, paste0(write.path,"feral.swine.effort.take.aerial.ALL.",Sys.Date(),".csv"))
write.csv(harvest.chronology, paste0(write.path,"feral.swine.effort.take.aerial.chronology.ALL.",Sys.Date(),".csv"))

##----END----##





count(final.agg.out.dat$ST_NAME)
length(unique(final.agg.out.dat$AGRP_PRP_ID))
nrow(final.agg.out.dat)

summary(final.agg.out.dat$Take)


##---- MAKE PLOTS ----
par(mfrow=c(2,2))

hist(final.agg.out.dat$Take, xlab="Take", breaks=30,main=NULL)

summary(final.agg.out.dat$Take)

plot(log(final.agg.out.dat$TOTAL.LAND),final.agg.out.dat$Take,xlab="log Property Size",ylab="Take")
plot(log(final.agg.out.dat$TOTAL.LAND),final.agg.out.dat$Flight.Days,xlab="log Property Size",ylab="Flight Days")
plot(log(final.agg.out.dat$Flight.Days),log(final.agg.out.dat$Take),xlab="log Flight Days",ylab="log Take")


