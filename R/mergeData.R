#########################################################################################
# mergeData
#
# Merge phenotype data with ddPCR data obtained by readData()
# phenoData must have "well". All other variables will be preserved
#
#########################################################################################

mergeData <- function(phenoData, pcrData)
{
# determine data type, check arguments and harmonize well names
	if (!"well" %in% names(phenoData))
		stop("phenotype data requires 'well'")

# remove variables in ddPCR data that are present in phenoData EXCEPT 'well'
	vars <- names(pcrData)[!names(pcrData) %in% names(phenoData)]
	if ("well" %in% names(pcrData)) vars <- c(vars, "well")
	vars <- unique(vars)
	pcrData <- pcrData[vars]
	res <- merge(phenoData, pcrData)

# convert strings to factors
	sel <- sapply(res, is.character)
	res[sel] <- lapply(res[sel], as.factor)

# reorganize data
	pdnames <- c("file", "event", "well", "column", "row")
	first <- pdnames[pdnames %in% names(res)]
	last <- names(res)[!names(res) %in% pdnames]
	res <- res[c(first, last)]

# exclude unused levels (allows pd to hold information than required)
	res <- droplevels(res)
	rownames(res) <- NULL
	return(res)
}
