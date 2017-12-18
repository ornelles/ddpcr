## Synopsis
This is a suite of tools based on the `virustiter` package to analyze BioRad digital PCR data. This code requires the 	`MASS`, `bbmle`, `cutoff`, `diptest`and `lattice` packages.

## Overview
This is the first attempt to adapt the virus titer code to process two channel ddPCR results. Data exported as CSV files from QuantaSoft are merged with phenotype data to count positives.  

## Installation

**This is NOT meant for installation yet.** If all goes well, the contents can be cloned and "installed" locally from the local directory with `devtools::load_all()`.

## Working notes
Phenotype date should be a data frame with the variable `well` as a character string such as "A1" or "A01".

Additional properties as variables in the phenotype data frame will be merged with the raw data. 

The expected work flow requires loading the data, assigning binary cutoff values (intermediate values are not yet accommodated.) 

Typical workflow:
```
## libraries
	library(MASS)
	library(bbmle)
	library(cutoff)
	library(diptest)
	library(lattice)

  fd <- file.choose() # CSV file
  fp <- file.choose() # phenodata CSV file (or RDA file)
  df <- readData(fd)
  pd <- read.csv(fp)  # or pd <- readRDS(fp)
  df <- mergeData(pd, df)
  cut <- getCut(df)
  df <- score(df, cut)
# filter df for outliers (excessively high values)
  res <- tally(df)
```
Supporting functions:
```
  plotCut(df)    # TO DO: calculate and show cutoff values with densityplot 
  plotHist(df)   # TO DO: histogram of each well with optional cutoff values
  plotPlate(df)  # TO DO: lattice plot plate showing cell position and positives
  plotWell(df, well) # TO DO:
```  
## License
GPL-3
