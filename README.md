# PiecewiseSmoothing_Demo

runPSAndExaminePS.R will run the two key piecewise smoothing modules (PS and examinePS).

plotResults.Rmd includes code to reproduce all graphs in the Piecewise Smoothing manuscript and the supplementary material.

Also included in the modules folder are 
1) birdRange, which provides a method to select bird species that meet a given density threshold over a certain amount of the study area. A code chunk in the birdRange.Rmd file provides a useage example.
2) BRC is the module used to produce mean rasters of bootstrapped bird density models. As the bootstrapped models have been updated and are no longer available the code will not run, however it demonstrates the method used and could be redily adapted for other similar use cases.
3) WBI_vegReclass is the module code that was used to produce the example landscape rasters. 
