#########################################################################################
# plotHist
#
# display histogram for each well or file with lattice graphics with background cutoff
# 'cut' value as a single value or named vector from getCut()
#
# if missing and 'positive' is present, the maximum value will be shown
#
#########################################################################################
plotHist <- function(df, cut, by = c("default", "well", "file", "row", "column"),
		smooth = 1, mult = 5, log = TRUE, main = NULL, as.table = TRUE, layout = NULL,
		...)
{
	if (missing(df)) {
		usage <- c("plotHist examples:",
			'  plotHist(df)      # calculates and plots default cut values in df',
			'  plotHist(df, cut) # where cut was from getCut()')
		cat(usage, sep="\n")
		return(invisible(NULL))
	}
	library(lattice)
	library(latticeExtra)

# parse arguments and perform error checking
	by <- match.arg(by)
	if (by == "default") {
		if ("well" %in% names(df))
			by <- "well"
		else if ("file" %in% names(df))
			by <- "file"
		else
			stop("'well' and 'file' not in data set")
	}
	else if (!by %in% names(df))
		stop("'", by, "' not in data set")
	d.adj <- smooth	# to hand to density plot

# calculate background cutoff value and create strip labels 
	if (missing(cut))
		cut <- do.call(getCut, list(df, by, "val", mult, log))
	else {
		labs <- as.character(unique(df[[by]]))
		cut <- rep(cut, length.out = length(labs))
		names(cut) <- labs
	}

# assemble lattice plot
	if (is.null(layout))
		layout <- c(1, nlevels(df[[by]]))
	if (is.null(main))
		main <- paste(levels(df$dir), collapse=" + ")
	xlist <- list()	# for log argument in scales
	if (log == TRUE) {
		xlist <- list(log = 10)
		cut <- log10(cut)
	}
	form <- as.formula(paste("~ val |", by))
	obj <- histogram(form, data = df, main = main,
		layout = layout, n = 64, as.table = as.table,
		panel = function(x, ..., subscripts)
		{
			panel.histogram(x,  ...)
			idx <- unique(df[[by]][subscripts])
			panel.abline(v = cut[idx], col = 2)
		},
		scales = list(x = xlist, y = list(relation = "free", rot = 0)),
		xscale.components = xscale.components.log10ticks,
		...)
	plot(obj)
	invisible(obj)
}
