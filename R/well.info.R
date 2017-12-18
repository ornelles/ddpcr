#########################################################################################
# well.info
#
# support function to parse character vector into numeric prefix, well, row and column,
# return as a list with well reformatted according to sprintf format in "format".
#
#########################################################################################

well.info <- function(v, format="%02d") {
	v <- tolower(as.character(v))
	vv <- strsplit(v, "[[:alpha:]]+")					# separate prefix from column
	row <- gsub("[[:digit:]]", "", v)					# keep only letters
	column <- as.numeric(sapply(vv, function(x) x[2]))	# keep only numbers
	prefix <- sapply(vv, function(x) x[1])
	if (!all(row %in% letters[1:26]))
		stop("bad row value")
	if (any(column < 1 | column > 384))
		stop("bad column value")
	well <- paste(row, sprintf(format, column), sep="")
	return(list(prefix = prefix, well = well, row = row,
				column = as.character(column)))
}
