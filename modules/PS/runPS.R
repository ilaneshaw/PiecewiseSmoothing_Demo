library("conflicted")
library("SpaDES.core")

options(spades.useRequire = FALSE)

## make a list of directory paths
setPaths(
  modulePath = file.path("../../modules"),
  cachePath = file.path("../../cache"),
  scratchPath = file.path("../../scratch"),
  inputPath = file.path("../../inputs"),
  outputPath = file.path("../../outputs")
)
simPaths <- getPaths()

# specifiy where outputs go
outputFolderSpPreds <- checkPath(file.path(Paths$outputPath, "PS/spPreds/"), create = TRUE)
outputFolderSpPredsRasters <- checkPath(file.path(Paths$outputPath, "PS/spPredsRasters"), create = TRUE)

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

# specify species to include
# spList <- sort(c("OVEN")) #tester list
spList <- sort(c(
  "ALFL", "BBWA", "BCCH", "BOCH", "BRCR", "CMWA", "COYE",
  "DEJU", "GCKI", "GRAJ", "LEFL", "MOWA", "OVEN", "PAWA",
  "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"
))


## Set simulation and module parameters
simModules <- list("PS")
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  PS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    nTrees = 10, # 5000,
    ageGrouping = 20,
    maxAgeClass = 10,
    only1DPS = FALSE,
    spList = spList,
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
  )
)


## Simulation setup
mySim <- simInit(
  times = simTimes, params = simParams,
  modules = simModules, paths = simPaths
)

test <- spades(mySim)
