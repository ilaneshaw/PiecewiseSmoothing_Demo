library("conflicted")
library("SpaDES.core")

options(spades.useRequire = FALSE)

## set paths ####
setPaths(
  modulePath = file.path("./modules"),
  cachePath = file.path("./cache"),
  scratchPath = file.path("./scratch"),
  inputPath = file.path("./inputs"),
  outputPath = file.path("./outputs")
)
simPaths <- getPaths()

# specify where outputs go ####

runName <- "17x10YrAgeClasses" #put results for each set of age classes or different study area in named folder
outputFolderSpPreds <- checkPath(file.path(Paths$outputPath, runName, "spPreds/"), create = TRUE)
outputFolderSpPredsRasters <- checkPath(file.path(Paths$outputPath, runName, "spPredsRasters"), create = TRUE)

# specify inputs and input paths (to be provided as parameters), and download example input files ####

#studyArea
locationStudyArea <- checkPath(file.path(Paths$inputPath, "studyArea"), create = TRUE)
.studyAreaName <- "studyArea_AB_BCR6.shp"
zen4R::download_zenodo(doi = "10.5281/zenodo.17409820", path = locationStudyArea)
unzip(file.path(locationStudyArea, paste(tools::file_path_sans_ext(.studyAreaName), ".zip", sep = "")), exdir = locationStudyArea)
locationStudyArea <- file.path(Paths$inputPath, "studyArea", tools::file_path_sans_ext(.studyAreaName))

#landscape rasters
locationLandscapeRasters <- checkPath(file.path(Paths$inputPath, "landscapeRasters"), create = TRUE)
zen4R::download_zenodo(doi = "10.5281/zenodo.17410019", path = locationLandscapeRasters)
unzip(file.path(locationLandscapeRasters, "landscapeRasters.zip"), exdir = locationLandscapeRasters)

nameForClassRas <- "vegTypesRas_AB_BCR6_2011"
locationForClass <- locationLandscapeRasters

nameLandClassRas <- "landCoverRas_AB_BCR6_2010"
locationLandClass <- locationLandscapeRasters

nameAgeRas <- "ageRas_AB_BCR6_2011"
locationAge <- locationLandscapeRasters

#species rasters
locationSpRas <- checkPath(file.path(Paths$inputPath), create = TRUE)
zen4R::download_zenodo(doi = "10.5281/zenodo.17516504", path = locationSpRas)
unzip(file.path(locationSpRas, "meanSpRasters.zip"), exdir = locationSpRas)
locationSpRas <- file.path(Paths$inputPath, "meanSpRasters")

#specify rasterToMatch - this can be any raster that covers the whole study area at the desired resolution
locationRasterToMatch <- locationSpRas #here I use one of the species rasters
rasterToMatchName <- "ALFL-meanBoot_BCR-60_studyArea_AB_BCR6"

# specify BCR (to be provided as parameter)
nameBCR <- "60"

# provide species list (to be provided as parameter) ####
spList <- sort(c("ALFL", "OVEN")) # tester list
# spList <- sort(c(
#   "ALFL", "BBWA", "BCCH", "BOCH", "BRCR", "CMWA", "COYE",
#   "DEJU", "GCKI", "GRAJ", "LEFL", "MOWA", "OVEN", "PAWA",
#   "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"
# ))

# #read spList from csv file (as saved from birdRange module)
# speciesListName <- "speciesList"
# speciesListLocation <- checkPath(file.path(Paths$inputPath, "speciesList"), create = TRUE)
# 
# spList <- read.csv(file.path(speciesListLocation, speciesListName))
# spList <- spList[,2]
# spList <- sort(c(spList[2:length(spList)]))


# set simulation and module parameters ####
simModules <- list("PS", "examinePS")
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  PS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    nTrees = 10, # 5000, #glm number of trees
    ageGrouping = 10, # choose age class width
    maxAgeClass = 17, # choose number of age classes
    only1DPS = FALSE, # choose 1DPS or 2DPS
    spList = spList, # species to include in analysis
    nameBCR = nameBCR, 
    studyAreaLocation = locationStudyArea, #specify folder containing the study area .shp file 
    .studyAreaName = .studyAreaName, #specify name of the study area .shp file 
    rasterToMatchLocation = locationRasterToMatch, #specify folder containing the rasterToMatch  
    rasterToMatchName = rasterToMatchName, #specify name of the raster to match 
    nameForClassRas = nameForClassRas, #specify name of the forest class raster 
    locationForClass = locationForClass,#specify folder containing the forest class raster  
    nameLandClassRas = nameLandClassRas, #specify name of the land class raster file
    locationLandClass = locationLandClass, #specify folder containing the land Class Raster 
    nameAgeRas = nameAgeRas, #specify name of the age raster 
    locationAge = locationAge,#specify folder containing the age Raster 
    locationSpRas = locationSpRas #specify folder containing the species density rasters  
  ),
  examinePS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    spList = spList, # species to include in analysis
    only1DPS = FALSE # choose 1DPS or 2DPS
  )
)


# simulation setup ####
mySim <- simInit(
  times = simTimes, 
  params = simParams,
  modules = simModules, 
  paths = simPaths
)

# run simulation ####
test <- spades(mySim)
