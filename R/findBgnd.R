################################################################################
# findBgnd
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
#	full	logical, if TRUE, return list with mu (mean) and sigma (sd) and
#			arguments. 
#
# Result
#	upper limit of background values as four significant digits or a list with 
#	this value named 'ans' and parameters indicated under the full = TRUE option
#
################################################################################

findBgnd <- function(x, mult = 6, beta = 0.25, asym = FALSE, full = FALSE)
{
# test for bimodal distribution
	bimodal <- suppressMessages(diptest::dip.test(x)$p.value < 0.05)

# allow use without genefilter package
	if (asym == TRUE && !require(genefilter, quietly = TRUE)) {
		message("No genefilter package available: ignoring 'asym' option")
		asym <- FALSE
	}
# use fraction of input specified by 'beta' if bimodal for speed
	if (bimodal & length(x) > 3000)
		x <- sample(x, round(beta * length(x)))

	if (bimodal) {	# bimodal Gaussian peaks assumed
		mm <- cutoff::em(x, "normal", "normal")
		x.mu <- mm$param[["mu1"]]
		x.sd <- mm$param[["sigma1"]]
		ans <- cutoff::cutoff(mm)[["Estimate"]]
	}
	else if (asym == TRUE) { # right-leaning asymmetric Gaussian 
		x.mu <- genefilter::half.range.mode(x)
		side <- (x - x.mu)[x < x.mu]
		x.sd <- sqrt(sum(side^2)/(length(side)-1))
		ans <- x.mu + mult * x.sd
	}
	else {	# normal Gaussian fit
		x.mu <- mean(x)
		x.sd <- sd(x)
		ans <- x.mu + mult * x.sd
	}
# return desired value
	if (full == FALSE)
		return(signif(ans, 4))
	else
		return(list(ans = ans, mu = x.mu, sd = x.sd, mult = mult,
					asym = asym, bimodal = bimodal))
}
