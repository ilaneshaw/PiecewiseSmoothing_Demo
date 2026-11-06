library("SpaDES.core")

options(spades.useRequire = FALSE)

## set paths ####

#specify ageClasses
ageGrouping <- 10 #10, # 20, #40, # choose age class width
maxAgeClass <- 17 #17, # 9, #5, # choose number of age classes

# put outputs for each set of age classes/different study area in separate folder
province <- "AB" #"AB" #"QC" # "NT"  ## used only for naming, not needed for module functionality
bcr <-  "6" #"6"#"8_12" #"7"  ## used only for naming, not needed for module functionality
runName <- paste(maxAgeClass, "x", ageGrouping, "YrAgeClasses_", province, "_", bcr, sep = "")
#runName <- "9x20yrAgeClasses"
#runName <- "5x40yrAgeClasses"

setPaths(
  modulePath = file.path("./modules"),
  cachePath = file.path("./cache"),
  scratchPath = file.path("./scratch"),
  inputPath = file.path("./inputs"),
  outputPath = file.path("./outputs", runName)
)
simPaths <- getPaths()

# specify inputs and input paths (to be provided as parameters), and download example input files ####

# studyArea
#AB OLD
# .studyAreaName <- "studyArea_AB_BCR6.shp"
# locationStudyArea <- checkPath(file.path(Paths$inputPath, "studyArea"), create = TRUE)
# zen4R::download_zenodo(doi = "10.5281/zenodo.17409820", path = locationStudyArea)
# unzip(file.path(locationStudyArea, paste(tools::file_path_sans_ext(.studyAreaName), ".zip", sep = "")), exdir = locationStudyArea)
# locationStudyArea <- file.path(Paths$inputPath, "studyArea", tools::file_path_sans_ext(.studyAreaName))
# studyArea <- terra::vect(file.path(locationStudyArea, .studyAreaName))

.studyAreaName <- paste("studyArea_", province, "_BCR", bcr, ".shp", sep = "")
locationStudyArea <- checkPath(file.path(Paths$inputPath, "studyArea", tools::file_path_sans_ext(.studyAreaName)), create = TRUE)
studyArea <- terra::vect(file.path(locationStudyArea, .studyAreaName))

# landscape rasters
 locationLandscapeRasters <- checkPath(file.path(Paths$inputPath, "landscapeRasters"), create = TRUE)
# zen4R::download_zenodo(doi = "10.5281/zenodo.17410019", path = locationLandscapeRasters)
# unzip(file.path(locationLandscapeRasters, "landscapeRasters.zip"), exdir = locationLandscapeRasters)

nameForClassRas <- paste("forRas_", province, "_BCR", bcr, "_2020", sep = "")
locationForClass <- locationLandscapeRasters

nameLandClassRas <- paste("landRas_", province, "_BCR", bcr, "_2020", sep = "")
locationLandClass <- locationLandscapeRasters

nameAgeRas <- paste("ageRas_", province, "_BCR", bcr, "_2020", sep = "")
locationAge <- locationLandscapeRasters

# species rasters
# locationSpRas <- checkPath(file.path(Paths$inputPath), create = TRUE)
# zen4R::download_zenodo(doi = "10.5281/zenodo.17516504", path = locationSpRas)
# unzip(file.path(locationSpRas, "meanSpRasters.zip"), exdir = locationSpRas)
locationSpRas <- locationSpRas <- file.path(Paths$inputPath, "spRasters", "meanSpRasters")
locationWtRas <- file.path(Paths$inputPath, "spRasters", "wtRas_A")
#namePattern <- ".tif"
spNameSubstr <- c(1,4) 
excludeList <- c("EUST", "HOSP", "ROPI") # specify any species to exclude. Here, we exclude non-native species


# specify rasterToMatch - this can be any raster that covers the whole study area at the desired resolution/projection.
rasterToMatchName <- nameLandClassRas
locationRasterToMatch <- locationLandscapeRasters # here I use the landCoverRas


# # provide species list (to be provided as parameter) ####
# spList <- sort(c(
#   "ALFL", "BBWA", "BCCH", "BOCH", "BRCR", "CMWA", "COYE",
#   "DEJU", "GCKI", "CAJA", "LEFL", "MOWA", "OVEN", "PAWA",
#   "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"
# ))
# 
# # Alternative: read spList from csv file (as saved from birdRange module)
# speciesListName <- "birdList_AB"
# speciesListLocation <- checkPath(file.path(Paths$inputPath, "speciesList"), create = TRUE)
# spList <- read.csv(file.path(speciesListLocation, speciesListName))
# spList <- sort(spList[,2])


# set simulation and module parameters ####
loadOrder <- c("birdRange", "PS", "examinePS")
modules <- list("birdRange", "PS", "examinePS")
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  birdRange = list(
    #.doSelectInitialTime = 0,
    fromDrive = FALSE,
    locationSpRas = locationSpRas,
    #namePattern = namePattern, 
    spNameSubstr = spNameSubstr, 
    .studyAreaName = .studyAreaName,
    rasterToMatchLocation = locationRasterToMatch,
    rasterToMatchName = rasterToMatchName,
    studyAreaLocation = locationStudyArea,
    excludeList = excludeList,
    areaThreshold = 0.5,
    probOfOccurrenceThreshold = 0.05
  ),
  PS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    parallelOption = FALSE, #TRUE 
    nTrees =  1000, #glm number of trees
    ageGrouping = ageGrouping, # choose age class width
    maxAgeClass = maxAgeClass, # choose number of age classes
    only1DPS = FALSE, # choose 1DPS or 2DPS
    #spList = spList, # species to include in analysis
   # studyAreaLocation = locationStudyArea, # specify folder containing the study area .shp file
    #.studyAreaName = .studyAreaName, # specify name of the study area .shp file
   # rasterToMatchLocation = locationRasterToMatch, # specify folder containing the rasterToMatch
    #rasterToMatchName = rasterToMatchName, # specify name of the raster to match
   #locationSpRas = locationSpRas # specify folder containing the species density rasters
   incWeights = FALSE, # include weighting when making predictions
   locationWtRas = locationWtRas,
    nameAgeRas = nameAgeRas, # specify name of the age raster
    locationAge = locationAge, # specify folder containing the age Raster
   nameForClassRas = nameForClassRas, # specify name of the forest class raster
   locationForClass = locationForClass, # specify folder containing the forest class raster
   nameLandClassRas = nameLandClassRas, # specify name of the land class raster file
   locationLandClass = locationLandClass # specify folder containing the land Class Raster
      ),
  examinePS = list(
    doPredsInitialTime = 1,
    .plotInitialTime = 1,
    .saveInitialTime = 1,
    min2DStatsSample = 100,
    #spList = spList, # species to include in analysis
    only1DPS = FALSE # choose 1DPS or 2DPS
  )
)


# simulation setup ####
mySim <- simInit(
  times = simTimes,
  params = simParams,
  modules = modules,
  loadOrder = loadOrder,
  paths = simPaths
)

# run simulation ####
test <- spades(mySim)

