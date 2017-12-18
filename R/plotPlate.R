#########################################################################################
# plotPlate
#
# display entire plate with lattice graphics (expects well values as "a03", etc.
# otherwise treats it as a 
#
#########################################################################################

plotPlate <- function(df, cex=1/2, alpha=1/2, main, invert.y=TRUE, ... ) {
	library(lattice)

	if ("well" %in% names(df)) {
		byWell <- TRUE
		n <- nlevels(df$well)
		if (n > 96)	{rows <- 32; columns <- 24}		# 384-well plate
		else if (n > 48) {rows <- 8; columns <- 12}	# 96-well plate
		else if (n > 24) {rows <- 6; columns <- 8}	# 48-well plate
		else if (n > 12) {rows <- 4; columns <- 6}	# 24-well plate
		else if (n > 6) {rows <- 3; columns <- 4}	# 12-well plate
		else if (n == 6) {rows <- 2; columns <- 3}	# 6-well plate
		else {rows <- 1; columns <- n}				# fewer than 6
		if (any(paste(letters[1:8],rep(2:4,each=8),sep="") %in% levels(df$well)))
			ww <- sprintf("%s%d", rep(letters[1:rows], each=columns), 1:columns)
		else
			ww <- sprintf("%s%02d", rep(letters[1:rows], each=columns), 1:columns)
		df$well <- factor(df$well, levels=ww)	# revise levels
		skip <- ! levels(df$well) %in% unique(as.character(df$well))
	}
	else if ("file" %in% names(df)) {
		byWell <- FALSE
		n <- nlevels(df$file)
		flevels <- abbreviate(levels(df$file), 18, method = "both")
		df$file <- factor(df$file, levels = flevels) 
		rows <- columns <- ceiling(sqrt(n))
	}
	else
		stop("requires variable 'well' or 'file' in ", deparse(substitute(df)))

	if (missing(main) & !("dir" %in% names(df)))
		main <- Sys.Date()
	else
		main <- df$dir[1]

	if (!"positive" %in% names(df)) df$positive <- FALSE
	if (invert.y) df$ym <- -df$ym

	if (byWell == TRUE) {
		obj <- xyplot(ym ~ xm | well, data=df, group=positive, cex=cex, alpha=alpha,
			as.table=TRUE, aspect="iso", layout=c(columns,rows), skip=skip,
			xlab = "", ylab = "", scales = list(draw=FALSE), main = main, ...)
	}
	else {
		obj <- xyplot(ym ~ xm | file, data=df, group=positive, cex=cex, alpha=alpha,
			as.table=TRUE, aspect="iso", layout=c(columns,rows),
 			xlab = "", ylab = "", scales = list(draw=FALSE), main = main, ...)
	}
	plot(obj)
	invisible(obj)
}
