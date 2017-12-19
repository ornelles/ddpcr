################################################################################
# tally
#
# tally total events, positive events, and calculate target number by Poisson
#
# merges additional phenotype data in optional pd
#
################################################################################

tally <- function(df, pd = NULL)
{
# working with channels (ignoring quadrant for now)
	vars <- grep("^ch[[:digit:]].pos$", names(df))
	ch <- sub(".pos", "", names(df)[vars], fixed = TRUE)
	nchan <- length(vars)
	if (nchan < 1 | nchan > 2)
		stop("found ", nchan, " channels, expected 1 or 2")

# collect number of events, number of positives and number of targets
	res <- aggregate(df[vars], df["well"], length)
	names(res)[-1]  <- paste(ch, "events", sep = ".")
	val <- aggregate(df[vars], df["well"], sum)
	res <- cbind(res, val[-1])
	val <- aggregate(df[vars], df["well"], function(x)
			round(length(x) * (-log(sum(!x)/length(x)))))
	names(val)[-1] <- paste(ch, "targets", sep = ".")
	res <- cbind(res, val[-1])

# group data by channel
	idx <- c(matrix(seq_len(3*nchan), ncol = nchan, byrow = T))
	res <- cbind(res[1], res[idx+1])

# merge with pd and return
	if (!is.null(pd))
		res <- mergeData(pd, res)
	rownames(res) <- NULL
	return(res)
}

