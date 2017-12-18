################################################################################
# readData
#
# Read csv files from QuantaSoft in current directory or in parent directory of
# file argument. Optionally randomly reorder data with the random.seed.
#
# Merge this output with a phenotype data file to create a file of the same form.
#
# Arguments
#	f	path to an amplitude file of the form "...+A04_Amplitude.csv"
#	nchan	number of channels (2 or 1)
#	seed	integer value, NA or NULL. If NA, data will be returned as
#			stored. If NULL (default), data will be randomly ordered,
#			otherwise `seed` specifies an integer for for seed() to
#			generate reproducibly randomized data.
#
# Returns
#	data frame with well, row, and column information as well as amplitude
#		values in ch1 (and ch2)
#
# Use mergePdata on data before proceeding further
#
################################################################################

readData <- function(f, nchan = 2, seed = NULL)
{
# process argument
	if (missing(f))
		path <- getwd()
	else
		path <- dirname(f)

# read data from CSV files
	ff <- list.files(path, pattern = "Amplitude.csv$", full = TRUE,
		ignore.case = TRUE)

# extract and assemble well, row and column information
	temp <- expand.grid(row = LETTERS[1:8], column = sprintf("%02d", 1:12))
	temp$well <- paste0(temp$row, temp$column)
	temp$well <- factor(temp$well, levels = temp$well)

# read data into list of data frames with optional randomization
	dd <- lapply(ff, read.csv)
	if (is.null(seed))
		dd <- lapply(dd, function(v) v[sample(nrow(v)),])
	else if (!is.na(seed)) {
		set.seed(as.integer(seed))
		dd <- lapply(dd, function(v) v[sample(nrow(v)),])
	}

# extract well information from file name, assemble channel variable name
	m <- regexpr("[ABCDEFGH][[:digit:]]{2}", ff)
	well <- regmatches(ff, m)
	if (length(well) != length(ff))
		stop("unable to parse well names properly")
	ch <- paste0("ch", seq_len(nchan))	# channel names

# assemble data, ignore cluster calls, add event index
	for(i in seq_along(dd)) {
		dd[[i]] <- dd[[i]][seq_len(nchan)]
		names(dd[[i]]) <- factor(ch)
		dd[[i]]$well <- well[i]
		dd[[i]]$event <- seq.int(length.out = nrow(dd[[i]]))
	}
	df <- do.call(rbind, dd)
	rownames(df) <- NULL
	return(df[c("event", "well", ch)])
}
