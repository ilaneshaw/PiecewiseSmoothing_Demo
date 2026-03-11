library("conflicted")
library("SpaDES.core")

options(spades.useRequire = FALSE)

## make a list of directory paths
setPaths(
  modulePath = file.path("./modules"),
  cachePath = file.path("./cache"),
  scratchPath = file.path("./scratch"),
  inputPath = file.path("./inputs"),
  outputPath = file.path("./outputs")
)
simPaths <- getPaths()

# specifiy where outputs go
runName <- "17x10YrAgeClasses" #put results for each set of age classes or different study area in named folder
outputFolderSpPreds <- checkPath(file.path(Paths$outputPath, runName, "spPreds/"), create = TRUE)
outputFolderSpPredsRasters <- checkPath(file.path(Paths$outputPath, runName, "spPredsRasters"), create = TRUE)

# specify where inputs come from
locationRasterToMatch <- Paths$inputPath
rasterToMatchName <- "ALFL-meanBoot_BCR-60_studyArea_AB_BCR6"

locationStudyArea <- checkPath(file.path(Paths$inputPath, "studyArea/studyArea_AB_BCR6"), create = TRUE)
.studyAreaName <- "studyArea_AB_BCR6.shp"

locationLandscapeRasters <- checkPath(file.path(Paths$inputPath, "landscapeRasters"), create = TRUE)

nameForClassRas <- "vegTypesRas_AB_BCR6_2011"
locationForClass <- locationLandscapeRasters

nameLandClassRas <- "landCoverRas_AB_BCR6_2010"
locationLandClass <- locationLandscapeRasters

nameAgeRas <- "ageRas_AB_BCR6_2011"
locationAge <- locationLandscapeRasters

locationSpRas <- checkPath(file.path(Paths$inputPath, "meanSpRasters"), create = TRUE)

# specify BCR
nameBCR <- "60"

# get species list 
spList <- sort(c("OVEN")) # tester list
# spList <- sort(c(
#   "ALFL", "BBWA", "BCCH", "BOCH", "BRCR", "CMWA", "COYE",
#   "DEJU", "GCKI", "GRAJ", "LEFL", "MOWA", "OVEN", "PAWA",
#   "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"
# ))
#TODO: get a list of species from inputs?####

#download species and landscape rasters
# TODO: figure this bit out ####



## Set simulation and module parameters
simModules <- list("PS", "examinePS")
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  PS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    nTrees = 10, # 5000, #glm number of trees
    ageGrouping = 10, # age class width
    maxAgeClass = 17, # number of age classes
    only1DPS = FALSE, # choose 1DPS or 2DPS
    spList = spList, # species to include
    nameBCR = nameBCR,
    studyAreaLocation = locationStudyArea,
    .studyAreaName = .studyAreaName,
    rasterToMatchLocation = locationRasterToMatch,
    rasterToMatchName = rasterToMatchName,
    nameForClassRas = nameForClassRas,
    locationForClass = locationForClass,
    nameLandClassRas = nameLandClassRas,
    locationLandClass = locationLandClass,
    nameAgeRas = nameAgeRas,
    locationAge = locationAge,
    locationSpRas = locationSpRas
  ),
  examinePS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    spList = spList, # species to include
    only1DPS = FALSE # choose 1DPS or 2DPS
  )
)


## Simulation setup
mySim <- simInit(
  times = simTimes, params = simParams,
  modules = simModules, paths = simPaths
)

test <- spades(mySim)
