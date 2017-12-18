#########################################################################################
# tally
#
# tally positive and negative values data.frame from readIJResults() by well or file
#
# returns "result" data.frame with dir, well, moi, pos, neg and y and unit
# merges additional phenotype data in optional pd
#
#########################################################################################

tally <- function(df, pd = NULL)
{
	stopifnot(c("positive", "moi") %in% names(df))
	if (!any(c("well", "file") %in% names(df)))
		stop("requires 'well' or 'file' in data")

# select well or file as grouping variable
	if ("well" %in% names(df))
		group <- df$well
	else
		group <- df$file

# extract data frame name
	if (!is.null(df$dir) & nlevels(df$dir)==1)
		dir <- levels(df$dir)
	else
		dir <- "unknown"

# tally positive and create results data.frame
	pos <- tapply(df$positive==TRUE, group, sum)
	neg <- tapply(df$positive==FALSE, group, sum)
	y <- pos/(pos + neg)
	moi <- sapply(names(pos),function(v) df$moi[group==v][1])
	unit <- df$unit[1]
	if ("well" %in% names(df)) {
		well <- names(pos)
		row <- well.info(well)$row
		column <- well.info(well)$column
		res <- data.frame(dir, well, row, column, moi, unit, pos, neg, y)
	}
	else {
		file <- names(pos)
		res <- data.frame(dir, file, moi, unit, pos, neg, y)
	}
	if (!is.null(pd))
		res <- mergePdata(pd, res)
	rownames(res) <- NULL
	return(res)
}

