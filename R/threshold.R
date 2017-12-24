################################################################################
# threshold
# 
# Calculate the likely threshold between a bimodal Gaussian distribution or
# a threshold to the right of an asymmetric normal distribution by Otsu's
# method for a robust mean and standard deviation. Requires 'cutoff', 'bbmle',
# 'diptest', and 'MASS' packages. Suggests 'genefilter' package. 
#
# Arguments
#	x		values to evaluate
#	mult	standard deviation multiplier for unimodal distribution (6)
#	beta	fraction of values to sample for bimodal calculations (for n > 3000),
#			default value of 0.25
#	asym	logical value to use lower half of an asymmetric unimodal distribution
#			to estimate mean and standard deviation, otherwise use entire population
#	full	logical value to return a list with mu (mean) and sigma (sd) for 
#			the peak population(s), bimodal status, and parameter values. If `bimodal`
#			is `TRUE`, the value returned for `mult` is the "best-fit" value
#			given by `(thresh - mu1)/sd1` and **not** the original parameter.
#
# Result
#		upper limit of background values as four significant digits or a list with 
#		this value named 'thresh' with additional parameters as indicated
#
################################################################################

threshold <- function(x, mult = 6, beta = 0.25, asym = FALSE, full = FALSE)
{
# test for bimodal distribution
	bimodal <- suppressMessages(diptest::dip.test(x)$p.value < 0.05)

# allow further use if the genefilter package is not available
	if (asym == TRUE && !require(genefilter, quietly = TRUE)) {
		message("No genefilter package available: ignoring 'asym' option")
		asym <- FALSE
	}
# use fraction of input specified by 'beta' to speed calculations if bimodal
	if (bimodal & length(x) > 3000)
		x <- sample(x, round(beta * length(x)))

# assign NA as default for second peak values
	x.mu2 <- x.sd2 <- NA_real_

# determine properties of peak population(s)
	if (bimodal) {	# bimodal Gaussian peaks assumed
		mm <- cutoff::em(x, "normal", "normal")
		x.mu <- mm$param[["mu1"]]; x.sd <- mm$param[["sigma1"]]
		x.mu2 <- mm$param[["mu2"]]; x.sd2 <-mm$param[["sigma2"]]
		thresh <- cutoff::cutoff(mm)[["Estimate"]]
	}
	else if (asym == TRUE) { # right-leaning asymmetric Gaussian 
		x.mu <- genefilter::half.range.mode(x)
		side <- (x - x.mu)[x < x.mu]
		x.sd <- sqrt(sum(side^2)/(length(side)-1))
		thresh <- x.mu + mult * x.sd
	}
	else {	# normal Gaussian fit
		x.mu <- mean(x)
		x.sd <- sd(x)
		thresh <- x.mu + mult * x.sd
	}
# return desired value
	if (full == FALSE)
		return(signif(thresh, 4))
	else {
		mult <- ifelse(bimodal, (thresh - x.mu)/x.sd, mult) 
		return(list(thresh = thresh, mu = x.mu, sd = x.sd, mult = mult,
					asym = asym, bimodal = bimodal, mu2 = x.mu2, sd2 = x.sd2))
	}
}
