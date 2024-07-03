
#------------------------
#
# Preprocess MIS Data
#
#------------------------

rm(list = ls())
#.rs.restartR()
gc()

#---- read path ----
read.path <- "data/raw"

#---- write path ----
write.path <- "data/processed"

#----Load Libraries----
library(reshape2)
library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(modeest)
library(operators)
library(utils)
library(anytime)

#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")
source("R/FNC.MIS.Pre.Process.R")


#----Prep Data ----

#--Property Data
csv.name <- "fs_national_take_by_property_01JUL2024.csv"
file.name <- file.path(read.path, csv.name)
df <- read_csv(file.name)
dat.Agr.take <- df |>
  distinct() |>
  select(-PRPS_QTY) |>
  mutate(AGRPROP_ID = WT_AGRPROP_ID)

csv.name <- "fs_national_property_01JUL2024.csv"
file.name <- file.path(read.path, csv.name)
df <- read_csv(file.name)
dat.Agr.property <- df |>
  distinct() |>
  group_by(AGRP_PRP_ID, ALWS_AGRPROP_ID, ALWS_DA_ID, PRP_NAME, ST_NAME, ST_GSA_STATE_CD, CNTY_NAME, CNTY_GSA_CNTY_CD, PRPS_PROP_TYPE) |>
  filter(PRPS_QTY == max(PRPS_QTY)) |> # Assume max PRPS_QTY is property size
  ungroup() |>
  mutate(AGRPROP_ID = ALWS_AGRPROP_ID)

dat.Agr <- left_join(dat.Agr.take, dat.Agr.property)

#--Add combined id for convience
#dat.Agr$unk.id <- paste0(dat.Agr$AGRP_PRP_ID,".",dat.Agr$ALWS_AGRPROP_ID,".",dat.Agr$ALWS_DA_ID)

#dat.Agr<-dat.Agr[dat.Agr$DA_NAME=="SWINE, FERAL",]

dat.Agr2 <- dat.Agr[complete.cases(dat.Agr$ST_GSA_STATE_CD),]

dat.Agr3 <- alter.columns(dat.Agr2)

#Assign
# spc.lut <- read_csv("C:/DATA/MIS/PigData/species.look.up.csv")
#
# tmp<-merge(dat.Agr, spc.lut, by.x="DA_NAME", by.y="species", all.x=TRUE)
#
# colnames(tmp)[ncol(tmp)] <- "DA_NAME_TYPE"
#
# write.csv(tmp, paste0(write.path,"processed.trapping.",file.name))


write.csv(tmp, paste0(write.path,"processed.",file.name))

#--Make property lut
dat.Agr4 <- dat.Agr3[dat.Agr3$DA_NAME=="SWINE, FERAL",]
lut.property.acres <- make.property.lut(dat.Agr4)

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







