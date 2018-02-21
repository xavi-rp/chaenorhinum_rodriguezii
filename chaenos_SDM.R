
###########################################################################
########                                                       ############
########      Chaenorhinum rodriguezii on the Balearic Is.     ############
########              Species Distribution Modeling            ############
########                                                       ############
###########################################################################


# chaenos_SDM.R
#
# Created on: Autumn 2017 (under construction)
#
# Created by: Xavier Rotllan-Puig (xavi.rotllan.puig@gmail.com)
#
# Description: The aim of this script is to make the SDM for Chaenorhinum rodriguezii on 
# the Balearic Islands. We use presence data from Bioatles, and free climatic, altitude 
# and Land Use data as predictors.
# We use MaxEnt to build the model


# ------------------------------------------


getwd()
wd <- "~/Dropbox/chaenorhinum_rodriguezii/data"
setwd(wd)
#load("~/Dropbox/chaenorhinum_rodriguezii/data/chaenos_SDM_ws.RData")

#### packages ####
library(sp)
library(rgdal)
library(raster)
library(graphics)
library(rgeos)
library(dismo)
library(devtools)
library(virtualspecies)
install_github("bobmuscarella/ENMeval@master")   #to install the last version of ENMeval from the GitHub repository of Bob Muscarella (https://github.com/bobmuscarella/ENMeval)
library(ENMeval)



#### Importing and mapping presences on the Balearics ####
# Data downloaded from Bioatles (http://bioatles.caib.es/serproesfront/VisorServlet), on 27/09/2017

chaenos_bioatles <- read.csv("ch_bioat170927.csv", header = TRUE, sep = ";") 
chaenos_bioatles
ch_pres <- chaenos_bioatles[chaenos_bioatles$state=="presence", 1:2]
ch_all <- chaenos_bioatles
coordinates(ch_pres) <- c("x", "y")  # set spatial coordinates
plot(ch_pres)
proj4string(ch_pres) <- CRS("+init=EPSG:23031")  # define projection: European Datum 1950 (31N)
summary(ch_pres)
ch_pres

coordinates(ch_all) <- c("x", "y")  # set spatial coordinates
plot(ch_all)
proj4string(ch_all) <- CRS("+init=EPSG:23031")  # define projection: European Datum 1950 (31N)
summary(ch_all)
ch_all

CRS.new <- CRS("+init=EPSG:25831") #new Coordinate Reference System to project data: ETRS89 31N

ch_presETRS89 <- spTransform(ch_pres, CRS.new)  #projecting 
ch_presETRS89
summary(ch_presETRS89)
write.csv(ch_presETRS89, file = "ch_presETRS89.csv", row.names = FALSE)

ch_allETRS89 <- spTransform(ch_all, CRS.new)  #projecting 
ch_allETRS89$col <- ifelse(ch_allETRS89$state == "extinction", "black", "red")
ch_allETRS89
summary(ch_allETRS89)
write.csv(ch_allETRS89, file = "ch_allETRS89.csv", row.names = FALSE)


#### Reading in a contour of the Balearics (polygon) and a mask (raster) ####
# see "https://github.com/xavi-rp/ephedra_sdm/blob/master/ephedra_balears.R"

mask <- raster(paste0("/Users/xavi/Google Drive/ephedra_balears/data/variables_cneor/etrs89/", "mask_balearix.img"))
plot(mask, legend=FALSE)
mask
balearix_ETRS89 <- readOGR(dsn = "/Users/xavi/Google Drive/ephedra_balears/data/variables_cneor/etrs89/balearix_ETRS89/", layer = "balearix_ETRS89")
balearix_ETRS89
plot(balearix_ETRS89)

# Plotting presences
pdf("presences.pdf")
plot(mask, legend = FALSE, col = "grey77")
plot(balearix_ETRS89, add=TRUE)
plot(ch_presETRS89, add=TRUE, col = 2, pch = 18)
legend("bottomright", inset = c(0.05, 0.1), legend = c("Chaenorhinum rodriguezii"), col=2, pch = 18)
dev.off()

pdf("presences_all.pdf")
plot(mask, legend = FALSE, col = "grey77")
plot(balearix_ETRS89, add=TRUE)
plot(ch_allETRS89, add=TRUE, col = ch_allETRS89$col, border = ch_allETRS89$col, pch = 18)
legend("bottomright", inset = c(0.05, 0.1), title = "Chaenorhinum rodriguezii", legend = c("Extinction", "Presences"), col = c("black", "red"), pch = 18)
dev.off()

#### Importing climatic data from WorldClim (19 bioclimatic variables in a RasterStack) ####
# WorldClim 1.4; date of download: 27/09/2017; 30 arc-seconds (~1 km);
# latitude / longitude coordinate reference system (not projected) and the datum is WGS84
# temperature data are in °C * 10; precipitation data is mm (millimeter)

if(!file.exists(paste0(wd, "/worldclim05/bioclim_16_ETRS89.img"))){
  bioclim_16 <- getData('worldclim', var='bio', res=0.5, lon=3, lat=39,
                        path = paste0(wd, "/worldclim05"))              # importing tile 16
  bioclim_16_WGS84 <- bioclim_16
  proj4string(bioclim_16_WGS84) <- CRS("+init=epsg:4326")

  # Projecting to ETRS89 31N
  bioclim_16_ETRS89 <- bioclim_16_WGS84
  bioclim_16_ETRS89 <- projectRaster(bioclim_16_WGS84, crs="+init=EPSG:25831") #to ETRS89
  save(bioclim_16_ETRS89, file = paste0(wd, "/worldclim05/bioclim_16_ETRS89.rdata"))
} else {
  load(paste0(wd, "/worldclim05/bioclim_16_ETRS89.rdata"), verbose = TRUE)
}


# Clipping the variables with a polygon (Balearic Is.)

bioc_16_bal <- crop(bioclim_16_ETRS89, balearix_ETRS89)
#plot(bioc_16_bal)


#### Importing a Digital Elevation Model (DEM) ####

# The data is downloaded from http://srtm.csi.cgiar.org/
# Citation: Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM
# data V4, International  Centre for Tropical  Agriculture (CIAT), available  from
# http://srtm.csi.cgiar.org.
# Download date: 9/02/2016
# Original resolution: approx. 90m (3arc-sec)
# Version: 4.1
# Coord Sys: geographic coordinate system - WGS84 datum.
# 
# http://www.ngdc.noaa.gov/mgg/topo/gltiles.html    this is another option (100m res)


#elev_36_04 <- raster("elevation/srtm_36_04/srtm_36_04.asc") # NW Iberian Peninsula
#elev_36_05 <- raster("elevation/srtm_36_05/srtm_36_05.asc") # SW Ib. P.
#elev_37_04 <- raster("elevation/srtm_37_04/srtm_37_04.asc")  # NE Ib. P.
elev_37_05 <- raster("elevation/srtm_37_05/srtm_37_05.asc")  # SE + Balearics

#elev <- merge(elev_37_04, elev_37_05, tolerance=0.05)
plot(elev_37_05)

elev_WGS84 <- elev_37_05 
proj4string(elev_WGS84) <- CRS("+init=epsg:4326") # giving coord system, etc
elev_ETRS89 <- projectRaster(elev_WGS84, crs="+init=EPSG:25831") #to ETRS89

elev_bal <- crop(elev_ETRS89, balearix_ETRS89) # croping to Balearics

resample(elev_bal, bioc_16_bal, method="bilinear", filename="elev_bal_agg", overwrite=TRUE)  # upscaling ( to lower resolution: larger cells)
elev_bal <- raster("elev_bal_agg")
elev_bal@data@names <- "elev_bal"

plot(elev_bal)


#### Distance to the coast ####

#mask_inv <- mask
#mask_inv[is.na(mask_inv)] <- 2
#mask_inv[mask_inv == 1] <- NA

#dist_coast <- distance(mask_inv)
#resample(dist_coast, bioc_16_bal, method="bilinear", filename="dist_coast_agg", overwrite=TRUE)  # upscaling ( to lower resolution: larger cells)
#dist_coast <- raster("dist_coast_agg")
#dist_coast@data@names <- "dist_coast"
#plot(dist_coast)
#plot(balearix_ETRS89, add=TRUE)

#


#### Selecting non-correlated variables ####

# Creating a stack with the variables

stack_rstrs <- stack()  # creating a RasterStack (for predictors)
#stack_rstrs <- stack(stack_rstrs, bioc_16_bal, elev_bal, dist_coast)    #filling in the stack
stack_rstrs <- stack(stack_rstrs, bioc_16_bal, elev_bal)    #filling in the stack

pdf("vars_collinearity.pdf")
vables_NoC <- removeCollinearity(stack_rstrs,
                                 multicollinearity.cutoff = 0.85,
                                 select.variables = TRUE,  # if TRUE, randomly select one variable of the group. If FALSE, returns a list with the groups
                                 sample.points = FALSE,
                                 plot = TRUE)
dev.off()   # saving plot
vables_NoC


# A rasterStack with not-correlated variables

predictors <- subset(stack_rstrs, vables_NoC)
predictors@layers



#### Modelling parameters ####
# ENMEval package
# Evaluating which is the optimal combination of regularization and features parameters 
# in order to improve model performance while limiting overfitting.
# The Akaike Information Criterion corrected for small samples sizes reflects both model
# goodness-of-fit and complexity and it is independent of the partitioning method because 
# it is computed with the full set of presences.
# The model with the lowest AICc value (i.e. Delta_AICc = 0) is considered the best model out 
# of the current suite of models.
# Big AUC_diff, equal to overfit models.
#


# presences
cha_rod <- read.csv("ch_presETRS89.csv", header=TRUE)
cha_rod <- round(cha_rod, 0)
cha_rod

# running ENMEval: https://github.com/bobmuscarella/ENMeval       #source code of the library
# MaxEnt: Version 3.3.3e, November 2010 (https://github.com/mrmaxent/Maxent/tree/master/ArchivedReleases/3.3.3e)

enm_eval <- ENMevaluate(cha_rod, predictors, bg.coords = NULL, occ.grp = NULL,
                        bg.grp = NULL, RMvalues = seq(0.5, 2, 0.5),
                        fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                        categoricals = NULL, n.bg = 10000, method = "jackknife",
                        overlap = FALSE, aggregation.factor = c(2, 2),
                        kfolds = NA, bin.output = FALSE, clamp = TRUE,
                        rasterPreds = TRUE, parallel = FALSE, numCores = NULL,
                        progbar = TRUE, updateProgress = TRUE)

save(enm_eval, "enm_eval.rdata")

View(enm_eval@results) # data.frame with comparison results
best_comb <- enm_eval@results[complete.cases(enm_eval@results) & enm_eval@results$delta.AICc == 0, ]  # best result (rm, fc)

features <- as.vector(best_comb$features)
rm <- best_comb$rm
bc <- as.vector(rownames(best_comb))

best_model <- enm_eval@models[[20]]  # best model

pdf("resp_curves.pdf")
response(best_model)   # response curves of the best model
dev.off()   # saving plot


best_model_preds <- enm_eval@predictions[[20]]  # predictions of the best model ("row" format)

pdf("model_predictions_raw.pdf")
plot(best_model_preds)
plot(balearix_ETRS89, add=TRUE)
#plot(ch_presETRS89, add=TRUE, cex=0.01, col=2)
dev.off()

best_model_preds_log <- predict(best_model, predictors, args = c("outputformat=logistic")) # predictions of the best model ("logistic" format)
e <- extent(430000, 550000, 4320000, 4440000)
best_model_preds_log <- crop(best_model_preds_log, e)
#plot(best_model_preds_log)
pdf("sdm_chaenos_logistic.pdf")   
plot(best_model_preds_log)
plot(balearix_ETRS89, add=TRUE)
plot(ch_allETRS89, add=TRUE, col = ch_allETRS89$col, border = ch_allETRS89$col, pch = 18, cex=0.5)
legend("bottomleft", inset = c(0.05, 0.1), title = "Chaenorhinum rodriguezii", legend = c("Extinction", "Presences"), col = c("black", "red"), pch = 18)
dev.off()   # saving plot

#






