################################################################################
# score
#
# Assign positive values to events exceeding the cutoff values in 'cut' where
# 'cut' can be a single value or list of named values for well, row, or column
# that was produced by getCut()
#
# Arguments
#	df		data frame from readIJResults()
#	bg		background cutoff value(s) as a named list of named values
#
# Returns original data frame with 'positive' scored as TRUE or FALSE
#
################################################################################

score <- function(df, bg)
{
	if (missing(df) | missing(bg)) {
		usage <- c("score examples:",
			'  score(df, bg)         ## bg holds cutoff values',
			'  score(df, getCut(df)) ## cutoff values obtained from control wells',
			'  score(df, 150)        ## single value used as cutoff')
		cat(usage, sep="\n")
		return(invisible(NULL))
	}
# adjust and check arguments
	if (!is(df, "data.frame"))
		stop("'df' must be a data frame")
	ch <- grep("^ch[12]$", names(df), value = TRUE)
	nchan <- length(ch)
	if (!nchan %in% 1:2)
		stop("'df' must have one or two channels (ch1, ch2)")
	bg <- as.list(bg)	# expect list
	if (length(bg) != nchan)
		stop("the length of bg (", length(bg), ") must equal number of channels (", nchan, ")")
	if (is.null(names(bg)))
		names(bg) <- ch
	if (!all(names(bg) %in% names(df)))
		stop("not all of '", paste(names(bg), collapse = ", "), "' are in 'df'")

# create variable names for positive values
	pvars <- paste(ch, "pos", sep = ".")

# process each channel separately
	for (i in seq_along(ch)) {
		bvars <- lapply(bg, names)[[i]]
		if (length(bg[[i]]) <= 1)			# single value
			df[[pvars[i]]] <- df[[ch[i]]] > bg[[i]]
		else if (all(bvars %in% levels(df$well)))
			df[[pvars[i]]] <- df[[ch[i]]] > bg[[i]][as.character(df$well)]
		else if (all(bvars %in% levels(df$row)))
			df[[pvars[i]]] <- df[[ch[i]]] > bg[[i]][as.character(df$row)]
		else if (all(bvars %in% levels(df$column)))
			df[[pvars[i]]] <- df[[ch[i]]] > bg[[i]][as.character(df$column)]
		else
			stop("bg must contain single values or a named vector (file, well, row, or column)")
	}

# add quadrant indicator if two channels are present
	if (nchan == 2) {
		a <- as.numeric(df[[pvars[1]]])
		b <- as.numeric(df[[pvars[2]]])
		df$quad <- 1 + a + 2*b
	}
	return(df)
}
