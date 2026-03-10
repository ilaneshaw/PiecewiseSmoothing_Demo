

#library("conflicted")
library("SpaDES")
library("terra")
library("LandR")

#############################################################################################
outputsDir <- checkPath("../../outputs", create = TRUE)
inputsDir <- checkPath("../../inputs", create = TRUE)

setPaths(modulePath = file.path("../../modules"),
         cachePath = file.path("../../cache"),
         scratchPath = file.path("../../scratch"),
         inputPath = inputsDir,
         outputPath = outputsDir)

rasterToMatchLocation <- file.path(inputsDir, "ALFL-meanBoot_BCR-60_studyArea_AB_BCR6")
rasterToMatch <- terra::rast(rasterToMatchLocation)
#rasterToMatch <- prepInputsLCC(year = 2005)

#get StudyArea shapefile
print("get studyArea shapefile from local drive")

# specify studyArea
locationStudyArea <- checkPath(file.path(Paths$inputPath, "studyArea/studyArea_AB_BCR6"), create = TRUE)
.studyAreaName <- "studyArea_AB_BCR6.shp"
studyArea <- terra::vect(file.path(locationStudyArea,.studyAreaName))

#postProcess studyArea
studyArea <- reproducible::postProcess(studyArea,
                                       destinationPath = getwd(),
                                       #filename2 = "studyArea", 
                                       #useTerra = FALSE,
                                       #fun = "sf", #use the function vect
                                       targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
                                       overwrite = FALSE,
                                       verbose = TRUE)

rasterToMatch <- crop(rasterToMatch, studyArea)
rasterToMatch <- mask(rasterToMatch, studyArea) 
names(rasterToMatch) <- "rasterToMatch"
plot(rasterToMatch)

#LCC10 <- prepInputsLCC(year = 2010, rasterToMatch = rasterToMatch)
LCC10Location <- file.path(inputsDir, "CAN_LC_2010_CAL.tif")
LCC10 <- terra::rast(LCC10Location)
LCC10 <- postProcessTerra(from = LCC10,
                          to = rasterToMatch,
                          overwrite = FALSE,
                          verbose = TRUE)
LCC10 
plot(LCC10)

valsLCC10 <- values(LCC10)
uniqueValsLCC10 <- sort(unique(valsLCC10))


reclassTab <- read.csv("../../inputs/LCC10_reclass.csv", sep = ";", header = TRUE)
reclassTab$landCoverClass <- as.factor(reclassTab$landCoverClass)
landCoverClassRaster <- terra::classify(LCC10, reclassTab)
landCoverClassRaster
plot(landCoverClassRaster)
is.factor(landCoverClassRaster)


terra::writeRaster(x = landCoverClassRaster,
                   filename = file.path(outputsDir, "landCoverRas_AB_BCR6_2010"),
                   filetype= "GTiff",
                   gdal="COMPRESS=NONE",
                   overwrite = TRUE)

