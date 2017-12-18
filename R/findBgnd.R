################################################################################
# find.bgnd
# 
# Calculate the likely cutoff value between a bimodal Gaussian distribution or
# a cutoff value to the right of an asymmetric normal distribution by Otsu's
# method for a robust mean and standard deviation. Requires 'cutoff', 'bbmle',
# 'diptest', and 'MASS' packages. Suggests 'genefilter' package. 
#
# Arguments
#	x		values to evaluate
#	mult	standard deviation multiplier for unimodal distribution (6)
#	beta	fraction of values to sample for bimodal calculations (for n > 3000)
#	asym	logical, if TRUE, use lower half of unimodal distribution to estimate
#			mean and standard deviation, otherwise use entire population
#	log		logical, if TRUE, non-zero x values log-transformed before analyzing
#
# Result
#	upper limit of background values as four significant digits
#
################################################################################

findBgnd <- function(x, mult = 6, beta = 0.25, asym = FALSE, log = FALSE)
{
# determine background according to bimodal nature
	bimodal <- suppressMessages(diptest::dip.test(x)$p.value < 0.05)

# allow use without genefilter package
	if (asym == TRUE && !require(genefilter, quietly = TRUE)) {
		message("No genefilter package available: ignoring 'asym' option")
		asym <- FALSE
	}

# use sample of input if bimodal
	if (bimodal & length(x) > 3000)
		x <- sample(x, round(beta * length(x)))
	if (bimodal & log == TRUE) {
		minx <- min(x) - 1
		x <- x - minx
		mm <- cutoff::em(x, "log-normal", "log-normal")
		ans <- cutoff::cutoff(mm)["Estimate"]
		ans <- unname(ans) + min(x)
	}
	else if (bimodal & log == FALSE) {
		mm <- cutoff::em(x, "normal", "normal")
		ans <- cutoff::cutoff(mm)["Estimate"]
		ans <- unname(ans)
	}
	else {
		if (log == TRUE)
			x <- log(x[x>0])
		if (asym == TRUE) {
			x.mu <- genefilter::half.range.mode(x)
			side <- (x - x.mu)[x < x.mu]
			x.sd <- sqrt(sum(side^2)/(length(side)-1))
		}
		else {
			x.mu <- mean(x)
			x.sd <- sd(x)
		}
	# return from possible log-transformation
		ans <- x.mu + mult*x.sd
		if (log)
			ans <- exp(ans)
	}
	return(signif(ans, 4))
}
