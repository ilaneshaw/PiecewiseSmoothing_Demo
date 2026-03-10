## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "PS",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c(""), family = "", role = c("aut", "cre"), email = "", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(PS = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "PS.Rmd"), ## same file
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@development", "ggplot2", "sf", "data.table", "terra",
    "LandR", "googledrive", "plotrix", "ggplot2", "ggpubr", "diptest", "nortest", "dplyr", "tidyverse", "reshape2"
  ),
  parameters = bindrows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(
      ".plots", "character", "screen", NA, NA,
      "Used by Plots function, which can be optionally used here"
    ),
    defineParameter(
      ".plotInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      "doPredsInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      "doPredsInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between getPreds events."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", 1, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".studyAreaName", "character", NA, NA, NA,
      "Human-readable name for the study area used - e.g., a hash of the study",
      "area obtained using `reproducible::studyAreaName()`"
    ),
    defineParameter(
      "only1DPS", "logical", FALSE, NA, NA,
      "do smoothing by cover class only (1D)? if FALSE smoothing will be done by forest type and age class where possible"
    ),
    defineParameter(
      "nTrees", "numeric", 5000, NA, NA,
      "number of trees for gbm to build"
    ),
    defineParameter(
      "maxAgeClass", "numeric", 17, NA, NA,
      "what the oldest age class will be (everything older will be included in this class)"
    ),
    defineParameter(
      "ageGrouping", "numeric", 10, NA, NA,
      "how many years included per age class"
    ),
    defineParameter(
      "spList", "character", NA, NA, NA,
      "a list of sp species in the format of 4-letter sp codes"
    ),
    defineParameter(
      "rasterToMatchLocation", "character", NA, NA, NA,
      "the file location of the rasterToMatch"
    ),
    defineParameter(
      "rasterToMatchName", "character", NA, NA, NA,
      "the name of the rasterToMatch file"
    ),
    defineParameter(
      "studyAreaLocation", "character", NA, NA, NA,
      "the file location of the studyArea"
    ),
    defineParameter(
      "nameBCR", "character", NA, NA, NA,
      "the BAM regional model BCR region that the studyArea is located in"
    ),
    defineParameter(
      "nameForClassRas", "character", NA, NA, NA,
      "the file name of the forest class raster"
    ),
    defineParameter(
      "locationForClass", "character", NA, NA, NA,
      "the location of the forest class raster"
    ),
    defineParameter(
      "archiveForClass", "character", NA, NA, NA,
      "the zip file the forest class raster is located in"
    ),
    defineParameter(
      "nameLandClassRaster", "character", NA, NA, NA,
      "the file name of the land cover raster"
    ),
    defineParameter(
      "locationLandClass", "character", NA, NA, NA,
      "the location of the land cover raster"
    ),
    defineParameter(
      "archiveLandClass", "character", NA, NA, NA,
      "the zip file the land cover raster is located in"
    ),
    defineParameter(
      "nameAgeRas", "character", NA, NA, NA,
      "the file name of the age raster"
    ),
    defineParameter(
      "locationAge", "character", NA, NA, NA,
      "the location of the age raster"
    ),
    defineParameter(
      "archiveAge", "character", NA, NA, NA,
      "the zip file the age raster is located in"
    ),
    defineParameter(
      "locationSpRas", "character", NA, NA, NA,
      "the location of the sp density rasters"
    ),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(
      ".seed", "list", list(), NA, NA,
      "Named list of seeds to use for each event (names)."
    ),
    defineParameter(
      ".useCache", "logical", FALSE, NA, NA,
      "Should caching of events or module be used?"
    )
  ),
  inputObjects = bindrows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("rasterToMatch", "SpatRaster", desc = "A raster used to determine projection of other spatial objects. Must cover all of the region covered by the studyArea"),
    expectsInput("studyArea", "SpatVector", desc = "Polygon to use as the study area."),
    expectsInput(objectName = "forClassRaster", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "landClassRaster", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "ageRaster", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "spRasters", objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA),
    createsOutput(objectName = "spDatasets", objectClass = NA, desc = NA, sourceURL = NA),
    createsOutput(objectName = "spPreds", objectClass = NA, desc = NA, sourceURL = NA),
    createsOutput(objectName = "statsGBM", objectClass = NA, desc = NA, sourceURL = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.PS <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      # checkObject(sim, name = Par$stackName, layer = "habitatQuality")

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim,
        eventTime = P(sim)$doPredsInitialTime,
        moduleName = "PS", eventType = "do1DPreds"
      )
      sim <- scheduleEvent(sim,
        eventTime = P(sim)$doPredsInitialTime,
        moduleName = "PS", eventType = "map1D"
      )

      if (P(sim)$only1DPS == FALSE) {
        sim <- scheduleEvent(sim,
          eventTime = P(sim)$doPredsInitialTime,
          moduleName = "PS", eventType = "do2DPreds"
        )
        sim <- scheduleEvent(sim,
          eventTime = P(sim)$doPredsInitialTime,
          moduleName = "PS", eventType = "map2D"
        )
      }

      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "PS", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "PS", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "PS", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "PS", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    do1DPreds = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- do1DPreds(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "do1DPreds"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    map1D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- map1D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "map1D"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    do2DPreds = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- do2DPreds(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "do2DPreds"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    map2D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- map2D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "map2D"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # save the spDatasets tables
  print(
    "saving sp datasets"
  )
  save(sim$spDatasets,
    file = file.path(outputFolderSpPreds, "spDatasets.Rdata")
  )
  # load(file.path(outputFolderSpPreds, "spDatasets.Rdata"))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  # Plots(sampleData, fn = ggplotFn)

  clearPlot()

  # PLOT INPUT LANDSCAPE RASTERS

  # rasterToMatch
  quickPlot::Plot(sim$rasterToMatch, na.color = "blue", title = "Raster To Match", new = TRUE, legend = TRUE)
  clearPlot()

  # forClassRaster
  # quickPlot::Plot(sim$forClassRaster, na.color = "blue", title = "Forest Class raster", new = TRUE, legend = TRUE)
  # clearPlot()

  # landClassRaster
  quickPlot::Plot(sim$landClassRaster, na.color = "blue", title = "Land Class Raster", new = TRUE, legend = TRUE)
  clearPlot()

  if (P(sim)$only1DPS == FALSE) {
    # ageRaster
    quickPlot::Plot(sim$ageRaster, na.color = "blue", title = "Age Raster", new = TRUE, legend = TRUE)
    clearPlot()
  }

  # PLOT KERNEL DENSIY PLOTS
  lapply(P(sim)$spList, FUN = function(sp) {
    spDataset <- eval(parse(text = paste("sim$spDatasets$", sp, sep = "")))
    spDataset$landForClass <- as.factor(spDataset$landForClass)
    spDataset$species <- as.factor(spDataset$species)
    spDataset$age <- as.integer(spDataset$age)

    if (requireNamespace("ggplot2")) {
      kernelDensityPlot <- spDataset |> ggplot2::ggplot(ggplot2::aes(x = spDensity, fill = factor(landForClass))) +
        geom_density() +
        facet_wrap(~landForClass,
          scales = "free_y"
        ) +
        theme_classic() +
        ggtitle(paste("Kernel density plots for ", sp, " by forest or land cover class", sep = "")) +
        xlab("Predicted sp density") +
        ylab("Occurence of density value")
      clearPlot()
      Plot(kernelDensityPlot, title = NULL)
    }
  })


  # PLOT 1D Predictions
  lapply(P(sim)$spList, FUN = function(sp) {
    spPred <- eval(parse(text = paste("sim$spPreds1D$", sp, sep = "")))
    spPred$landForClass <- as.factor(spPred$landForClass)
    spPred$FoLRaster <- as.factor(spPred$FoLRaster)
    spPred$species <- as.factor(spPred$species)

    if (requireNamespace("ggplot2")) {
      spPred1DPlot <- spPred |> ggplot2::ggplot(ggplot2::aes(
        x = landForClass,
        y = meanSpDensity,
        fill = landForClass
      )) +
        geom_bar(stat = "identity") +
        geom_errorbar(
          aes(
            ymin = meanSpDensity - seSpDensity,
            ymax = meanSpDensity + seSpDensity
          ),
          width = .5
        ) +
        theme_classic() +
        ggtitle(paste("Mean density predictions for ", sp, sep = "")) +
        xlab("Land or Forest Class") +
        ylab("Mean Predicted Density") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()
        )

      clearPlot()
      Plot(spPred1DPlot, title = NULL)
    }
  })

  if (P(sim)$only1DPS == FALSE) {
    # PLOT 2D PREDICTIONS
    lapply(P(sim)$spList, FUN = function(sp) {
      matrix <- eval(parse(text = paste("sim$spPreds$spMatricies$", sp, sep = "")))

      # matrix <- matrix[2:ncol(matrix)]
      # matrix <- data.matrix(matrix)
      tab <- melt(matrix)
      colnames(tab) <- c("forClass", "ageClass", "spDensityPred")
      # species <- rep(paste(sp), nrow(tab))
      # tab <- cbind(tab, species = species)
      tab$ageClass <- as.factor(tab$ageClass)
      tab$forClass <- as.factor(tab$forClass)
      # tab$species <- as.character(tab$species)
      tab$spDensityPred <- as.numeric(tab$spDensityPred)

      # spPred$landForClass <- as.factor(spPred$landForClass)
      # spPred$FoLRaster <- as.factor(spPred$FoLRaster)
      # spPred$species <- as.factor(spPred$species)
      #
      if (requireNamespace("ggplot2")) {
        plot2DPredictions <- tab |> ggplot2::ggplot(ggplot2::aes(
          fill = forClass,
          y = spDensityPred,
          x = ageClass
        )) +
          geom_bar(position = "dodge", stat = "identity") +
          facet_grid(~forClass,
            scales = "free"
          ) +
          theme_classic() +
          ggtitle(paste("Predicted density by forest and age class for ", sp, sep = "")) +
          xlab("Forest Age Class") +
          ylab("Predicted sp density") +
          theme(
            strip.background = element_rect(
              color = "white", fill = "white", linewidth = 1.1
            ),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )

        clearPlot()
        Plot(plot2DPredictions, title = NULL)
      }
    })
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
do1DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # get sp datasets

  sim$spDatasets <- lapply(X = P(sim)$spList, FUN = function(sp) {
    spLayer <- eval(parse(text = paste("sim$spRasters$", sp, sep = "")))
    if (P(sim)$only1DPS == FALSE) {
      landSpRasters <- c(
        spLayer,
        sim$forClassRaster,
        sim$landClassRaster,
        sim$ageRaster
      )
      print(landSpRasters)

      ## take the values from the rasters and input
      ## them to a data table called spDataset
      spDataset <- terra::values(landSpRasters, dataframe = TRUE)
      spDataset <- setnames(spDataset, c(
        "spDensity",
        "forClass",
        "landClass",
        "age"
      ))
    } else {
      landSpRasters <- c(
        spLayer,
        sim$forClassRaster,
        sim$landClassRaster
      )
      print(landSpRasters)
      ## take the values from the rasters and input
      ## them to a data table called spDataset
      spDataset <- terra::values(landSpRasters, dataframe = TRUE)
      spDataset <- setnames(spDataset, c(
        "spDensity",
        "forClass",
        "landClass"
      ))
    }
    spDataset <- as.data.table(spDataset)
    spDataset <- spDataset[!is.na(spDensity)] # get rid of cells with no spDensity data

    # create column saying if the cell is from the forest class or land class raster
    spDataset <- spDataset[, FoLRaster := ifelse(!is.na(landClass) & !is.na(forClass), "forClass",
      ifelse(!is.na(landClass), "landClass",
        ifelse(!is.na(forClass), "forClass", NA)
      )
    )]
    spDataset <- spDataset[!is.na(FoLRaster)] # get rid of cells with no forest or land class data.

    # set data types
    spDataset$landClass <- as.factor(spDataset$landClass)
    spDataset$forClass <- as.factor(spDataset$forClass)
    spDataset$FoLRaster <- as.factor(spDataset$FoLRaster)
    spDataset$age <- as.integer(spDataset$age)
    spDataset$spDensity <- as.numeric(spDataset$spDensity)


    # make column giving the land or forest class for each row.
    spDataset <- spDataset[, landForClass := coalesce(forClass, landClass)]
    # set data type
    spDataset$landForClass <- as.factor(spDataset$landForClass)

    # in case there are classes named the same in both rasters, combine the landForClass and FoLRaster data to make sure each class is unique
    spDataset <- unite(spDataset,
      landForClass,
      c(
        FoLRaster,
        landForClass
      ),
      remove = FALSE
    )
    print(sort(unique(spDataset$landForClass)))
    spDataset <- as.data.table(spDataset)


    nrowCV <- nrow(spDataset)
    spDataset$species <- rep(sp, nrowCV)
    spDataset$studyArea <- rep(P(sim)$.studyAreaName, nrowCV)
    # print(spDataset)
    fileName <- paste(P(sim)$.studyAreaName, "_", sp, "_fullDataset.csv", sep = "")
    write.csv(spDataset, file = file.path(outputFolderSpPreds, fileName))

    print(paste(sp, " dataset complete"))

    return(spDataset)
  })

  # # names(spDatasets) <- names(spRasters)
  # # for (i in names(spDatasets)) {
  # #   attr(spDatasets[[i]],"Species") <- i
  #
  # }

  names(sim$spDatasets) <- P(sim)$spList


  sim$spPreds1D <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)

    # get spPreds1D
    singleSpDataset <- eval(parse(text = paste("sim$spDatasets$", sp, sep = "")))
    singleSpDataset <- as.data.table(singleSpDataset)
    # print(singleSpDataset)
    spStats <- singleSpDataset[
      order(
        FoLRaster,
        landForClass
      )
      # order the rows by the land cover class
    ][, list(
      classCount = .N,
      # get the number of cells
      # each cover class
      # meanSpDensity = mean(spDensity),
      # get the mean sp density
      # for each cover class
      meanSpDensity = mean(spDensity),
      # get median sp density for each class
      medianSpDensity = median(spDensity),
      varSpDensity = var(spDensity) * (.N - 1) / .N,
      # get the population variance for sp density
      # for each cover class
      seSpDensity = std.error(spDensity),
      # get the standard error
      # for sp density
      # for each cover class
      normality_stat = tryCatch(ad.test(spDensity)$statistic, error = function(cond) {
        return(NaN)
      }),
      normality_p = tryCatch(ad.test(spDensity)$p.value, error = function(cond) {
        return(NaN)
      }),
      # ifelse(mean(spDensity) > 0,
      # tryCatch(ad.test(spDensity)$p.value,
      # error = function(cond){return(NA)}), NA),
      unimodality_stat = tryCatch(dip.test(spDensity)$statistic, error = function(cond) {
        return(NaN)
      }),
      unimodality_p = tryCatch(dip.test(spDensity)$p.value, error = function(cond) {
        return(NaN)
      }),
      species = sp,
      studyArea = P(sim)$.studyAreaName
    ),
    by = list(
      FoLRaster,
      landForClass
    )
    ]


    print(spStats)
    fileName <- paste(P(sim)$.studyAreaName, "_", sp, "_spPreds1D.csv", sep = "")
    write.csv(spStats, file = file.path(outputFolderSpPreds, fileName))

    return(spStats)
  })

  names(sim$spPreds1D) <- P(sim)$spList

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

map1D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  # get summary of assumptions/stats for 1D


  #### MAP OUT 1D PREDICTIONS
  print("get 1DMaps")

  # get non-Forest 1D data together
  print("get non-for 1D data")
  lc1DPreds <- lapply(X = P(sim)$spList, FUN = function(sp) {
    # separate out data table rows that are forested, get rid of unnecessary forestedStatus column
    landCoverDT <- as.data.table(eval(parse(text = paste("sim$spPreds1D$", sp, sep = ""))))
    landCoverDT <- landCoverDT[landCoverDT$FoLRaster == "landClass"]
    landCoverDT <- landCoverDT[, c(2, 4)]
    landCoverDT <- droplevels(landCoverDT)
    landCoverDT$landForClass <- gsub("[^0-9]", "", landCoverDT$landForClass)

    return(landCoverDT)
  })

  names(lc1DPreds) <- P(sim)$spList


  # reclassify land cover raster to get map of 1D sp preds in land class raster areas
  print("make lc1DMaps")
  sim$lc1DMaps <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)

    lcSpPreds <- eval(parse(text = paste("lc1DPreds$", sp, sep = "")))

    # make numeric
    lcSpPreds <- lcSpPreds[, landForClass := as.numeric(landForClass)]
    lcSpPreds <- lcSpPreds[, meanSpDensity := as.numeric(meanSpDensity)]
    # str(lcSpPreds) #check

    # raster1DBins <- landClassRaster
    raster1DBins <- terra::classify(sim$landClassRaster, lcSpPreds)

    names(raster1DBins) <- paste(sp)
    # plot(raster1DBins)

    print(paste(sp, "lc 1D map raster complete"))
    return(raster1DBins)
  })

  names(sim$lc1DMaps) <- P(sim)$spList


  # get Forest 1D data together
  print("get for1DPreds")
  for1DPreds <- lapply(X = P(sim)$spList, FUN = function(sp) {
    # separate out data table rows that are forested, get rid of unnecessary forestedStatus column
    forestedDT <- as.data.table(eval(parse(text = paste("sim$spPreds1D$", sp, sep = ""))))
    forestedDT <- forestedDT[FoLRaster == "forClass"]
    forestedDT <- forestedDT[, c(2, 4)]
    forestedDT <- droplevels(forestedDT)
    forestedDT$landForClass <- gsub("[^0-9]", "", forestedDT$landForClass)
    return(forestedDT)
  })

  names(for1DPreds) <- P(sim)$spList


  # reclassify forest class raster to give 1D sp prediction values for each sp sp
  print("get for1DMaps")
  sim$for1DMaps <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)

    forSpPreds <- eval(parse(text = paste("for1DPreds$", sp, sep = "")))

    # make numeric
    forSpPreds <- forSpPreds[, landForClass := as.numeric(landForClass)]
    forSpPreds <- forSpPreds[, meanSpDensity := as.numeric(meanSpDensity)]
    str(forSpPreds) # check

    # raster1DBins <- landClassRaster
    raster1DBinsForest <- terra::classify(sim$forClassRaster, forSpPreds)

    names(raster1DBinsForest) <- paste(sp)
    # plot(raster1DBinsForest)


    print(paste(sp, "for 1D map raster complete"))
    return(raster1DBinsForest)
  })

  names(sim$for1DMaps) <- P(sim)$spList

  # Get full 1D Map
  print("get for1DAndLc1DMaps")
  sim$for1DAndLc1DMaps <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)
    raster1DBinsLc <- eval(parse(text = paste("sim$lc1DMaps$", sp, sep = "")))
    raster1DBinsFor <- eval(parse(text = paste("sim$for1DMaps$", sp, sep = "")))

    spPredsRaster1D <- terra::cover(
      x = raster1DBinsFor,
      y = raster1DBinsLc
    )

    names(spPredsRaster1D) <- paste(sp)

    print(spPredsRaster1D)
    print(paste(sp, "for 1D and lc 1D map complete"))

    return(spPredsRaster1D)
  })

  names(sim$for1DAndLc1DMaps) <- P(sim)$spList

  print("save full 1D maps")

  lapply(X = P(sim)$spList, FUN = function(sp) {
    raster <- eval(parse(text = paste("sim$for1DAndLc1DMaps$", sp, sep = "")))
    names(raster) <- paste(sp)
    terra::writeRaster(
      x = raster,
      filename = file.path(outputFolderSpPredsRasters, paste(P(sim)$.studyAreaName, "_", sp, "_for1DAndLc1DMap", sep = "")),
      filetype = "GTiff",
      gdal = "COMPRESS=NONE",
      overwrite = TRUE
    )
  })


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

do2DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # get 2D sp predictions (by forest and age class)
  spGBM <- lapply(X = sim$spDatasets, FUN = function(spDF) {
    # keep track of how long each sp takes
    # print(Sys.time())

    sp <- unique(spDF$species)
    print(sp)
    spDT <- as.data.table(spDF)

    # separate out data table rows that are forested
    forestedDT <- spDT[FoLRaster == "forClass"]
    forestedDT <- forestedDT[, c(1, 2, 4)]
    forestedDT$forClass <- as.factor(forestedDT$forClass)
    forestedDT$age <- as.integer(forestedDT$age)
    forestedDT <- droplevels(forestedDT)

    # get rid of any rows with NA for age
    forestedDT <- na.omit(forestedDT, cols = "age")

    # fit gbm
    print("fit gbm")
    gbmFitted <- gbm::gbm(
      formula = spDensity ~ .,
      distribution = "gaussian",
      data = forestedDT,
      interaction.depth = 2,
      n.trees = P(sim)$nTrees,
      # verbose = TRUE,
      shrinkage = 0.3,
      n.minobsinnode = 5,
      bag.fraction = .80,
      train.fraction = 1
    ) # same number of trees as used in predict.gbm

    # print summary of relative influence by the factors
    # par(mar = c(5, 8, 1, 1))
    relInfGBM <- summary(gbmFitted)
    # relInfGBM <- title(main = sp)
    relInfGBM

    # get Freidman's h-stat
    FriedmansHStat <- gbm::interact.gbm(gbmFitted,
      data = forestedDT,
      i.var = c(1, 2),
      n.trees = P(sim)$nTrees
    )
    # print(FriedmansHStat)

    # #check gbm plot
    plotGBM <- gbm::plot.gbm(gbmFitted, i.var = c(1, 2), main = sp, xlab = "Forest Age (yrs)", ylab = "Sp Density (males/ha)")
    plotGBM

    # make into single summary object
    # statsGBM <- list(relInfGBM, FriedmansHStat) #, plotGBM)
    # names(statsGBM) <- c("relInfGBM", "FriedmansHStat") #, "plotGBM")

    relInfForClass <- relInfGBM[relInfGBM$var == "landForClass", ]
    relInfForClass <- relInfForClass$rel.inf
    relInfAge <- relInfGBM[relInfGBM$var == "age", ]
    relInfAge <- relInfAge$rel.inf
    species <- sp
    statsGBM <- cbind(species, FriedmansHStat, relInfForClass, relInfAge)
    print(statsGBM)

    # generate prediction df using expand
    sim$maxAge <- max(forestedDT$age) # get age of oldest cell listed in forestedDF
    sim$maxAgeClassAge <- P(sim)$maxAgeClass * P(sim)$ageGrouping
    ifelse(sim$maxAge > sim$maxAgeClassAge, sim$allAges <- c(0:sim$maxAge), sim$allAges <- c(0:sim$maxAgeClassAge)) # make a vector that counts from 0 to the age of the oldest cell, or the max age class age, whichever is bigger
    spPredictDT <- forestedDT %>% expand(forClass, sim$allAges) # make a data frame with two columns, landForClass and sim$allAges.The rows cumulatively provide each combination of age and forest class.
    names(spPredictDT) <- c("forClass", "age") # rename the two columns in spPredictDF

    # do prediction
    # (object, newdata, n.trees, type = "link", single.tree = FALSE,...)
    # This action produces a vector of predictions for the variables given by each row in spPredictDF
    print("do gbmPred")
    gbmPred <- gbm::predict.gbm(
      object = gbmFitted,
      newdata = spPredictDT,
      n.trees = P(sim)$nTrees,
      type = "link",
      single.tree = FALSE
    )

    noAgeClasses <- sim$maxAge / P(sim)$ageGrouping # get the number of age classes if the max age was simply divided by the age Grouping
    ageClasses <- ifelse(noAgeClasses < P(sim)$maxAgeClass, ageClasses <- P(sim)$maxAgeClass, ageClasses <- noAgeClasses)
    ageClasses <- rep(1:ageClasses, each = P(sim)$ageGrouping)
    ageClasses <- c(1, ageClasses)
    sim$ageClasses <- ageClasses
    if (noAgeClasses > P(sim)$maxAgeClass) {
      ageClassDiff <- noAgeClasses - P(sim)$maxAgeClass
      extraClasses <- P(sim)$maxAgeClass + (1:ageClassDiff)
      for (x in extraClasses) {
        print(x)
        sim$ageClasses <- replace(sim$ageClasses, sim$ageClasses == x, P(sim)$maxAgeClass)
      }
    }
    ageClasses <- sim$ageClasses
    gbmPredDT <- cbind(spPredictDT, gbmPred, ageClasses)
    gbmPredDT <- as.data.table(gbmPredDT)
    gbmPredDT$landForClass <- as.factor(gbmPredDT$forClass)
    gbmPredDT$ageClasses <- as.factor(gbmPredDT$ageClasses)

    # gbmPredDF <- aggregate( gbmPred ~ ageClasses * landForClass, gbmPredDF, mean )
    gbmPredDT <- gbmPredDT[order(list(forClass, ageClasses))][, list(gbmPred = mean(gbmPred)),
      by = list(forClass, ageClasses)
    ]


    # form matrix with landForClass as y axis and age as x axis
    print("form spMatrix")
    spMatrix <- reshape2::acast(gbmPredDT,
      forClass ~ ageClasses,
      value.var = "gbmPred"
    )

    # if any matrix prediction values are negative, make them be 0
    spMatrix[spMatrix < 0] <- 0
    print(spMatrix)
    # save 2D spPreds
    matrixName <- paste(P(sim)$.studyAreaName, "_", sp, "_spPreds2D.csv", sep = "")
    write.csv(spMatrix, file = file.path(outputFolderSpPreds, matrixName))

    # return(spMatrix)
    matrixAndSummary <- list(spMatrix, statsGBM)
    names(matrixAndSummary) <- c("spMatricies", "statsGBM")

    return(matrixAndSummary)
  })

  names(spGBM) <- P(sim)$spList

  # separate matricies from summaries
  print("separate out matricies")

  sim$spMatricies <- lapply(X = spList, FUN = function(sp) {
    print(sp)
    spMatrix <- eval(parse(text = paste("spGBM$", sp, "$spMatricies", sep = "")))

    matrixName <- paste(P(sim)$.studyAreaName, "_", sp, "_matrix.csv", sep = "")
    write.csv(spMatrix, file = file.path(outputFolderSpPreds, matrixName))

    return(spMatrix)
  })

  names(sim$spMatricies) <- P(sim)$spList

  # make table of all GBM Stats
  print("make table of GBM stats")
  statsList <- lapply(X = spList, FUN = function(sp) {
    print(sp)
    GBMStats <- as.data.table(eval(parse(text = paste("spGBM$", sp, "$statsGBM", sep = ""))))
    return(GBMStats)
  })

  sim$statsGBM <- rbindlist(statsList)
  write.csv(sim$statsGBM, file = file.path(outputFolderSpPreds, paste(P(sim)$.studyAreaName, "_statsGBM.csv", sep = "")))


  # create table defining age classes

  # diffMaxAge <- sim$maxAge- sim$maxAgeClassAge
  # if (!diffMaxAge < 1){
  #   ageClasses <- c(1, rep(1:P(sim)$maxAgeClass, each = P(sim)$ageGrouping),   rep(P(sim)$maxAgeClass, times = diffMaxAge)) #make vector of age classes to correspond with the vector allAges
  #   ageClassDefs <- cbind(sim$allAges, ageClasses)
  # } else {
  #   allAges <- c(allAges, (sim$maxAge+1):sim$maxAgeClassAge)
  #   ageClasses <- c(1, rep(1:maxAgeClass, each = P(sim)$ageGrouping)) #make vector of age classes to correspond with the vector allAges
  #   ageClassDefs <- cbind(sim$allAges,ageClasses)
  # }

  ageClassDefs <- as.data.table(cbind(sim$allAges, sim$ageClasses)) # makes data table that in one column has every age from 0 to max age (or max age class age if bigger), and the corresponding age class in the second column
  names(ageClassDefs) <- c("allAges", "ageClasses")
  # ageClassDefs <- as.data.table(ageClassDefs)
  sim$ageClassDefs <- ageClassDefs[, ageClasses := as.character(ageClasses)]
  write.csv(sim$ageClassDefs, file = file.path(outputFolderSpPreds, paste(P(sim)$.studyAreaName, "_ageClassDefs", sep = "")))


  # make list object of all outputs needed for MB Module
  print("make spPreds object")
  sim$spPreds <- list(sim$spMatricies, sim$spPreds1D, sim$ageClassDefs)
  names(sim$spPreds) <- c("spMatricies", "spPreds1D", "ageClassDefs")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

map2D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test


  #### GET 2D MAPS OF MATRIX PREDICTIONS
  print("GET 2D MAPS")

  print("make age class raster")
  # reclassify forAgeRaster into a raster of forest age classes
  ageReClassTab <- sim$spPreds$ageClassDefs
  ageReClassTab <- ageReClassTab[, ageClasses := as.numeric(ageClasses)] # change data type of ageClassDefs
  str(ageReClassTab) # check
  ageClassRaster <- sim$ageRaster # make copy of forAgeRaster to be reclassified
  ageClassRaster <- terra::classify(ageClassRaster, ageReClassTab) # do the reclassification based on ageClassDefs
  names(ageClassRaster) <- "ageClassRaster"
  sim$ageClassRaster <- ageClassRaster # check over the raster that has been reclassified
  sim$ageClassRaster <- terra::mask(terra::crop(sim$ageClassRaster, sim$forClassRaster), sim$forClassRaster)
  print(sim$ageClassRaster)


  # get for2Dmaps
  print("get for2DMaps")
  sim$for2DMaps <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)
    # check that spatial extent is the same for ageClassraster and forClassraster
    print("extent of forClassRaster same as ageClassRaster?")
    print(terra::ext(sim$forClassRaster) == terra::ext(sim$ageClassRaster))
    print("extent of forClassRaster same as the for1DMap Raster?")
    print(terra::ext(eval(parse(text = paste("sim$for1DMaps$", sp, sep = "")))) == terra::ext(sim$forClassRaster))
    print("same number of cells forClassRaster  as the for1DMap Raster?")
    print(length(terra::values(eval(parse(text = paste("sim$for1DMaps$", sp, sep = ""))))) == length(terra::values(sim$forClassRaster)))


    # reform matrix to make reclassification tab
    matrix <- eval(parse(text = paste("sim$spPreds$spMatricies$", sp, sep = "")))
    map1D <- eval(parse(text = paste("sim$for1DMaps$", sp, sep = "")))
    raster2DBins <- map1D


    reclassTab2D <- reproducible::Cache(reshape2::melt, matrix)
    colnames(reclassTab2D) <- c("forClass", "ageClass", "spDensityPred")
    reclassTab2D <- as.data.table(reclassTab2D)
    reclassTab2D$forClass <- as.factor(reclassTab2D$forClass)
    reclassTab2D$ageClass <- as.factor(reclassTab2D$ageClass)
    # reclassTab2D <- rbind(reclassTab2D, c(NA, NA, NA))
    # reclassify Raster according to reclassTab2D, ageClassRaster and forClassRaster
    # make an empty NA raster the same as the for1DMap
    # raster2DBins[] <- NA


    # # Combine categories and match with reclassification data
    # forClassValues <- terra::values(sim$forClassRaster)
    # ageClassValues <- terra::values(sim$ageClassRaster)
    # raster2DBinsValues <- data.table(forClass = forClassValues, ageClass = ageClassValues)
    # colnames(raster2DBinsValues) <- c( "forClass","ageClass")
    # raster2DBinsValues$forClass <- as.factor(raster2DBinsValues$forClass)
    # raster2DBinsValues$ageClass <- as.factor(raster2DBinsValues$ageClass)
    # raster2DBinsValues <- merge(raster2DBinsValues, reclassTab2D, by = c("forClass", "ageClass"), all.x = TRUE)
    # raster2DBinsValues <- raster2DBinsValues$spDensityPred
    # raster2DBinsValues <- raster2DBinsValues[!is.na(raster2DBinsValues)]
    #
    # Create a function to map raster values to density values

    # Define the function to map raster values to density values
    map_density <- function(x, y, reclassTab2D) {
      # Handle NA values: If either x or y is NA, return NA
      if (is.na(x) || is.na(y)) {
        return(NA)
      }

      # Look up the density value based on x and y
      density_value <- reclassTab2D[forClass == x & ageClass == y, spDensityPred]

      # Return density value if found, otherwise NA
      if (length(density_value) == 0) {
        print("warning! did not find density_value")
        return(NA)
      } else {
        return(density_value)
      }
    }

    # Define a function to apply to raster layers using app
    calc_density_raster <- function(x, y) {
      mapply(map_density, x, y, MoreArgs = list(reclassTab2D = reclassTab2D))
    }

    # Apply the function to the raster layers
    raster2DBins <- app(c(sim$forClassRaster, sim$ageClassRaster), fun = function(x) calc_density_raster(x[[1]], x[[2]]))

    names(raster2DBins) <- paste(sp)

    # check the new raster
    print(raster2DBins)
    # clearPlot()
    # Plot(raster2DBins, na.color = "blue", title = sp)

    # terra::writeRaster(x = raster2DBins,
    #                    filename = file.path(outputFolderSpPredsRasters, paste(P(sim)$.studyAreaName, "_", sp, "-for2DMap", sep = "")),
    #                    filetype= "GTiff",
    #                    gdal="COMPRESS=NONE",
    #                    overwrite = TRUE)


    return(raster2DBins)
  })

  names(sim$for2DMaps) <- P(sim)$spList

  # as Rdata file

  # save(for2DMaps,
  #      file =  file.path(outputFolderSpPredsRasters, "for2DMaps.Rdata"))
  # # #load(file.path(outputFolderSpPredsRasters, "for2DMaps.Rdata"))
  # sim$for2DMaps <- for2DMaps

  # get 2D map with Lc areas filled in with 1D predictions
  print("get for2DAndLc1DMaps")

  sim$for2DAndLc1DMaps <- lapply(X = P(sim)$spList, FUN = function(sp) {
    print(sp)
    raster1DBins <- eval(parse(text = paste("sim$lc1DMaps$", sp, sep = "")))
    raster2DBins <- eval(parse(text = paste("sim$for2DMaps$", sp, sep = "")))

    spPredsRaster <- terra::cover(
      x = raster2DBins,
      y = raster1DBins
    )

    names(spPredsRaster) <- paste(sp)

    # spPredsRaster #visually check Raster
    # clearPlot()
    # Plot(spPredsRaster, na.color = "blue", title = paste(sp,  " 2D smoothing map", sep = ""))

    # writeRaster(x = spPredsRaster, filename = file.path(outputFolderSpPredsRasters, paste(sp, "-spPredsRaster", sep = "")), format = "GTiff", overwrite = TRUE)

    print(paste(sp, "for 2D and lc 1D map raster complete"))
    return(spPredsRaster)
  })

  names(sim$for2DAndLc1DMaps) <- spList

  print("save for2DAndLc1DMaps")

  # as Rdata file

  # save(for2DAndLc1DMaps,
  #      file =  file.path(outputFolderSpPredsRasters, "for2DAndLc1DMaps.Rdata"))
  # #load(file.path(outputFolderSpPredsRasters, "for2DAndLc1DMaps.Rdata"))
  #
  # as tif files


  lapply(X = P(sim)$spList, FUN = function(sp) {
    raster <- eval(parse(text = paste("sim$for2DAndLc1DMaps$", sp, sep = "")))
    names(raster) <- paste(sp)
    terra::writeRaster(
      x = raster,
      filename = file.path(outputFolderSpPredsRasters, paste(P(sim)$.studyAreaName, "_", sp, "-for2DAndLc1DMap", sep = "")),
      filetype = "GTiff",
      gdal = "COMPRESS=NONE",
      overwrite = TRUE
    )
  })


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #


  # get rasterToMatch
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    print("get rasterTomatch from local drive")
    sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation, P(sim)$rasterToMatchName))
  }

  # get studyArea shapefile
  if (!suppliedElsewhere("studyArea", sim)) {
    print("get studyArea shapefile from local drive")
    studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation, P(sim)$.studyAreaName))

    # postProcess studyArea
    # Cache(projectRaster, raster, crs = crs(newRaster))
    message("cache studyArea")
    sim$studyArea <- reproducible::Cache(reproducible::postProcess, studyArea,
      # destinationPath = P(sim)$studyAreaLocation,
      # filename2 = "studyArea",
      useTerra = TRUE,
      fun = "terra::vect", # use the function vect
      targetCRS = crs(sim$rasterToMatch), # make crs same as rasterToMatch
      # overwrite = FALSE,
      verbose = TRUE
    )
  }

  # crop and mask rasterToMatch
  sim$rasterToMatch <- reproducible::Cache(terra::crop, sim$rasterToMatch, sim$studyArea)
  sim$rasterToMatch <- reproducible::Cache(terra::mask, sim$rasterToMatch, sim$studyArea)
  names(sim$rasterToMatch) <- "rasterToMatch"

  # get forest class raster
  if (!suppliedElsewhere("forClassRaster", sim)) {
    print("get forClassRaster from local Drive")
    sim$forClassRaster <- terra::rast(file.path(P(sim)$locationForClass, P(sim)$nameForClassRas))

    sim$forClassRaster <- reproducible::Cache(reproducible::postProcess, sim$forClassRaster,
      # destinationPath = downloadFolderForestClass,
      # use the function raster
      # targetCRS = crs(sim$rasterToMatch),
      fun = "terra::rast",
      useTerra = TRUE,
      # use the specified rasterToMatch to reproject to
      rasterToMatch = sim$rasterToMatch,
      studyArea = sim$studyArea,
      useCache = getOption("reproducible.useCache", TRUE),
      # overwrite = TRUE,
      verbose = TRUE
    )
    names(sim$forClassRaster) <- c("forClassRaster")
    sim$forClassRaster[sim$forClassRaster == 0] <- NA
  }

  # get land cover raster
  if (!suppliedElsewhere("landClassRaster", sim)) {
    print("get landClassRaster from Drive")
    sim$landClassRaster <- terra::rast(file.path(P(sim)$locationLandClass, P(sim)$nameLandClassRas))
    sim$landClassRaster <- reproducible::Cache(terra::crop, sim$landClassRaster, sim$studyArea)
    sim$landClassRaster <- reproducible::Cache(terra::mask, sim$landClassRaster, sim$studyArea)
    # sim$landClassRaster <- postProcess(sim$landClassRaster,
    #                                 #destinationPath = downloadFolderForestClass,
    #                                 #use the function raster
    #                                 #targetCRS = crs(sim$rasterToMatch),
    #                                 fun = "terra::rast",
    #                                 useTerra = TRUE,
    #                                 #use the specified rasterToMatch to reproject to
    #                                 rasterToMatch = sim$rasterToMatch,
    #                                 studyArea = sim$studyArea,
    #                                 useCache = getOption("reproducible.useCache", TRUE),
    #                                 #overwrite = TRUE,
    #                                 verbose = TRUE)
    #
    # landClassRaster[landClassRaster == 0] <- NA

    names(sim$landClassRaster) <- c("landClassRaster")
    print(sim$landClassRaster)
    # Plot(sim$landClassRaster, na.color = "blue")
    print(terra::unique(sim$landClassRaster))
    sim$landClassRaster <- terra::mask(sim$landClassRaster, sim$forClassRaster, inverse = TRUE)

    print(sim$landClassRaster)
    # Plot(sim$landClassRaster, na.color = "blue")
    print(terra::unique(sim$landClassRaster))
  }


  # get ageRaster
  if (P(sim)$only1DPS == FALSE) {
    if (!suppliedElsewhere("ageRaster", sim)) {
      print("get ageRaster from Drive")
      ageRaster <- terra::rast(file.path(P(sim)$locationAge, P(sim)$nameAgeRas))
      sim$ageRaster <- reproducible::Cache(reproducible::postProcess, ageRaster,
        # destinationPath = downloadFolderForestClass,
        # use the function raster
        useTerra = TRUE,
        fun = "terra::rast",
        # targetCRS = crs(sim$rasterToMatch),
        # use the specified rasterToMatch to reproject to
        rasterToMatch = sim$rasterToMatch,
        # studyArea = sim$studyArea,
        useCache = getOption("reproducible.useCache", TRUE),
        overwrite = FALSE,
        verbose = TRUE
      )

      names(sim$ageRaster) <- c("ageRaster")
    }
  }

  # get sp density rasters
  if (!suppliedElsewhere("spRasters", sim)) {
    P(sim)$spList <- sort(P(sim)$spList)
    ## for each item in turn from rastersForSplist the following function is applied:
    sim$spRasters <-
      lapply(
        X = P(sim)$spList,
        FUN = function(sp) {
          print(sp)
          # sp <- paste(sp, ".tif", sep = "")
          rasterFile <- paste(sp, "-meanBoot_BCR-60_studyArea_AB_BCR6", sep = "") # TODO: MAKE FLEXIBLE FOR OTHER NAMING CONVENTIONS####
          return(terra::rast(file.path(P(sim)$locationSpRas, rasterFile)))
        }
      )

    # get the species codes as names for the downloadedRasters object, rather than using the whole filepath
    # X <- lapply(sim$rastersForSpList, substr, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
    X <- lapply(sim$rastersForSpList, substr, 1, 4) # works for strings of the form "XXXX-meanBoot.tif"
    names(sim$spRasters) <- X

    sim$spRasters <- lapply(X = sim$spRasters, FUN = function(RasterLayer) {
      ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path

      print(RasterLayer)
      proRaster <- reproducible::postProcess(RasterLayer,
        # studyArea = sim$studyArea,
        rasterToMatch = sim$rasterToMatch,
        useTerra = TRUE,
        fun = "terra::rast",
        # destinationPath = downloadFolderSp,
        # filename2 = paste(downloadFolderSp, "/", names(RasterLayer), ".tif", sep = ""),
        # overwrite = TRUE,
        verbose = TRUE
      )
      # clearPlot()
      # Plot(proRaster, na.color= "grey")
      return(proRaster)
    })

    names(sim$spRasters) <- P(sim)$spList
  }


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
