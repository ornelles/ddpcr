################################################################################
# getThresh
#
# Determine threshold values for ddPCR amplitudes based on values in well, row,
# column or all values using threshold()
#
# Arguments
#	df	data frame from readData
#	by	character vector indicating "well", "row", "column" or "all"
#		`by` can be the same length as the number of channels.
#	...	params passed to `threshold` such as `mult`, `beta`, and `asym`
#		each can be a vector the same length as the number of channels.
#
# Value
#   List of length 1 or 2 (according to channels) with named values
#
################################################################################

getThresh <- function(df, by = c("well", "well"), ...)
{
	if (missing(df)) {
		usage <- c("getThresh examples:",
			'  getThresh(df, "well", mult = 6, asym = FALSE)',
			'  getThresh(df) # same as above',
			'  getThresh(df, by = c("well", "column"), mult = c(6, 10))')
		cat(usage, sep="\n")
		return(invisible(NULL))
	}
	if (!is.data.frame(df))
		stop(deparse(substitute(df)), " must be a data.frame")

# identify the amplitude channels
	ch <- c("ch1", "ch2")[which(c("ch1", "ch2") %in% names(df))]
	nchan <- length(ch)

# use partial matching to parse and adjust `by` argument
	if (length(by) == 1)
		by <- rep(by, length(ch))
	valid.by <- c("well", "row", "column", "all")
	sel <- pmatch(by, valid.by, duplicates.ok = TRUE)[seq_along(ch)]
	by <- valid.by[sel]

# extract and adjust arguments ... to pass to threshold
	dots <- list(...)
	dots <- lapply(dots,
			function(v) if(length(v) == nchan) v else rep(v, nchan))
	
# split and extract data to find background
	ans <- list()
	for (i in seq_along(ch)) {
		if (by[i] == "all")	# special case
			xx <- df[[ch[i]]]
		else
			xx <- split(df[[ch[i]]], df[by[i]])
		arg <- lapply(dots, "[", i)
		ans[[i]] <- sapply(xx,
				function(x) do.call(threshold, c(list(x), arg)))
	}
	names(ans) <- ch
	return(ans)	
}
