
#------------------------
#
# Preprocess MIS Data
#
#------------------------

rm(list = ls())
#.rs.restartR()
gc()

#----Set Working Dir----
setwd("C:/DATA/MIS/PigData/Mar2021/")

write.path<-"C:/DATA/MIS/PigData/Dec2020/"


#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(modeest)
library(operators)
library(utils)
library(anytime)

#----Required Functions
source("C:\\Documents\\Manuscripts\\Feral Swine - MIS Data Description\\Code\\FNC.MIS.calc.aerial.chronology.R")
source("C:\\Documents\\Manuscripts\\Feral Swine - MIS Data Description\\Code\\FNC.Misc.Utilities.R")
source("C:\\Documents\\Manuscripts\\Feral Swine - MIS Data Description\\Code\\FNC.MIS.Pre.Process.R")


#----Prep Data ----

#--Property Data
file.name<-"PropertyMar2021b.csv"

dat.Agr<-read.csv(file.name,stringsAsFactors=FALSE)
dat.Agr<-unique(dat.Agr)

#--Assume max PRPS_QTY is property size
dat.Agr<-aggregate(PRPS_QTY ~ AGRP_PRP_ID+ALWS_AGRPROP_ID+ALWS_DA_ID+PRP_NAME+ST_NAME+ST_GSA_STATE_CD+CNTY_NAME+CNTY_GSA_CNTY_CD+PRPS_PROP_TYPE+DA_NAME, data=dat.Agr, FUN=max)

#--Add combined id for convience
#dat.Agr$unk.id <- paste0(dat.Agr$AGRP_PRP_ID,".",dat.Agr$ALWS_AGRPROP_ID,".",dat.Agr$ALWS_DA_ID)

#dat.Agr<-dat.Agr[dat.Agr$DA_NAME=="SWINE, FERAL",]

dat.Agr<-dat.Agr[complete.cases(dat.Agr$ST_GSA_STATE_CD),]

dat.Agr<-alter.columns(dat.Agr)

#Assign 
spc.lut<-read.csv("C:/DATA/MIS/PigData/species.look.up.csv",stringsAsFactors=FALSE)

tmp<-merge(dat.Agr, spc.lut, by.x="DA_NAME", by.y="species", all.x=TRUE)

colnames(tmp)[ncol(tmp)] <- "DA_NAME_TYPE"

write.csv(tmp, paste0(write.path,"processed.trapping.",file.name))


dat.Agr<-dat.Agr[dat.Agr$DA_NAME=="SWINE, FERAL",]

write.csv(tmp, paste0(write.path,"processed.",file.name))

#--Make property lut
lut.property.acres<-make.property.lut(dat.Agr)

write.csv(lut.property.acres, paste0(write.path,"processed.lut.property.acres.csv"))





#--Take Data
file.name<-"PigTakePropMeth16Dec2020.csv"

dat.Kill<-read.csv(file.name,stringsAsFactors=FALSE)
dat.Kill<-unique(dat.Kill)

# Convert Dates to R Dates
dat.Kill$WT_WORK_DATE <- as.Date(dat.Kill$WT_WORK_DATE,"%d-%b-%y")

write.csv(dat.Kill, paste0(write.path,"processed.",file.name))




#--Effort
file.name<-"Effort16Dec2020.csv"

dat.Eff<-read.csv(file.name,stringsAsFactors=FALSE)
dat.Eff<-unique(dat.Eff)

dat.Eff<-alter.column.names(dat.Eff)

# Convert Dates to R Dates
dat.Eff$WT_WORK_DATE <- as.Date(dat.Eff$WT_WORK_DATE,"%d-%b-%y")

write.csv(dat.Eff, paste0(write.path,"processed.",file.name))




#--Take by Property
file.name<-"PigTakeByProperty16Dec2020.csv"

dat.PropKill<-read.csv(file.name,stringsAsFactors=FALSE)
dat.PropKill<-unique(dat.PropKill)

# Convert Dates to R Dates
dat.PropKill$WT_WORK_DATE <- as.Date(as.character(dat.PropKill$WT_WORK_DATE),"%d-%b-%y")

write.csv(dat.PropKill, paste0(write.path,"processed.",file.name))


##----END DATA PREP----




new.kill<-read.csv(paste0("C:/DATA/MIS/PigData/Mar2021/KillEffortMar2021.csv"),stringsAsFactors=FALSE)







