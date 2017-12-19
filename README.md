## Synopsis

A suite of tools based on the *still under development* `virustiter` package to analyze BioRad digital PCR data. This code requires the `MASS`, `bbmle`, `cutoff`, `diptest`, `genefilter` and `lattice` packages.

## Overview

This is a quick attempt to adapt the virus titer code to process two channel ddPCR results. Data exported as CSV files from QuantaSoft are merged with phenotype data to identify positive drops.

The key function implemented here is `findBgnd()` which is the logic behind `getCut()` to determine the cutoff thresholds. If the population shows evidence of being bimodal, code from the package `cutoff` is used to determine the break point. This code has the potential to accommodate different distributions (Gaussian, log-normal, Poisson, etc.) for the left and right populations. The code here only uses Gaussian populations. If most of the drops are negative and symmetric, the population is fit to a Gaussian curve to determine the mean and standard deviation. If the population is asymmetric and mostly negative with some positive samples to the right, the left half of the population is assumed to define the true negative values with the option `asym` set to `TRUE`. This half-population is used to determine the best values for a mean and standard deviation with `genefilter::half.range.mode()`. The cutoff is set at a default value of 6 x &sigma; above the mean but this can be changed by the user. 

There is no code for handling outlying or indeterminate values. These can be handled with the usual tools in R. 

## Installation

**This is NOT meant for installation as an R package just yet.** This repository can be cloned and "installed" locally by setting `path` to the local directory with `devtools::load_all(path)`. (The `devtools` package may have to be installed as well.)

## Working notes

Phenotype data should be a data frame with the variable `well` as a factor such as "A1" or "A01" where the case and zeros used for padding are ignored.

Additional properties can be placed in the phenotype data frame as factor variables. These will be merged with the raw data. 

After ensuring the packages have been installed, a sample work flow is illustrated below. The sample data are from a demo run at Wake Forest University on December 4, 2017.  

```
## select one CSV file within the directory of files exported by QuantaSoft
  fd <- system.file("extdata",
    "20171204_DEMO_ORNELLES_A01_Amplitude.csv", package = "ddpcr")

## load phenotype data, here as a data frame named 'pd'
  data(pd)  # load phenotype data frame

## read data, merge with phenotype data, determine cutoff and score
  df <- readData(fd)      # read values into data frame
  df <- mergeData(pd, df) # merge with phenotype data
  cut <- getCut(df, mult = c(5, 8)) # determine cutoff with adjustments
  df <- score(df, cut)    # determine positives and quadrants

## filter outliers
  df <- subset(df, ch1 < 600)   # HAdV probe in FAM
  df <- subset(df, ch2 < 2000)  # CMV probe in HEX

## One interesting sample displayed with lattice
  tp <- trellis.par.get()
  tp$superpose.symbol <- list(alpha = 0.5, col = c("gray", "#0080ff", "#ff00ff", "darkgreen"),
    fill = c("gray", "#0080ff", "#ff00ff", "darkgreen"), pch = c(1, 21, 21, 21))
  obj <- xyplot(ch1 ~ ch2 | sample + well:donor, df, subset = well == "A02", groups = quad)
  obj <- update(obj, panel = function(...) {panel.grid(h = -1, v = -1); panel.superpose(...)})
  obj <- update(obj, xlab = "CMV (ch2, HEX) amplitude", ylab = "HAdV (ch1, FAM) amplitude")
  obj <- update(obj, par.settings = tp)
  plot(obj)
```
Supporting functions to be migrated sometime:
```
plotCut(df)    # TO DO: show cutoff values with densityplot 
plotHist(df)   # TO DO: show histogram with cutoff values
plotPlate(df)  # TO DO: all-at-once 1D plate à la BioRad with lattice
```  
## License

GPL-3
