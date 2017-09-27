
###########################################################################
########  Chaenorhinum rodriguezii    #####################################
###########################################################################


# chaenorhinum.R

# Created on: under construction

# Contact: Xavier Rotllan-Puig (xavi@rotllanpuig.cat)

# Description: The aim of this script is to 
# 
#
# 


# ------------------------------------------


getwd()
setwd("~/Dropbox/chaenorhinum_rodriguezii/data")
load("~/Dropbox/chaenorhinum_rodriguezii/data/chaenorhinum_ws.RData")
remove(mtcars)

library(xlsx)

chaeno_biorep <- read.xlsx("~/Dropbox/chaenorhinum_rodriguezii/data/bio_rep_final_0516.xlsx", sheetName = "biorep_gener16", header = TRUE, as.data.frame= TRUE)
View(chaeno_biorep)
str(chaeno_biorep)
levels(chaeno_biorep$any)


### Number of seeds ###

chaeno_biorep[chaeno_biorep[!is.na(chaeno_biorep$),] & ]
aggdata[aggdata[,"Group.1"]==6 & aggdata[,"Group.2"]==0.05,"x"]



#















save.image("~/Dropbox/chaenorhinum_rodriguezii/data/chaenorhinum_ws.RData")

