################################################################################
# plot1d
#
# plot 1-dimensional display of events in the manner of QuantaSoft
#
################################################################################

plot1d <- function(df, cond = TRUE, ch = c("ch1", "ch2"),
		by.direction = c("column", "row"), symbol = NULL,
		xlab = "Event/Well", ylab = "Amplitude", main = NULL, ...)
{
# error checking

# parse parameters
	by.direction <- match.arg(by.direction)
	ch <- match.arg(ch)

# lattice superpose.symbol based on 'ch'
	if (is.null(symbol))
		symbol <- list(alpha = 0.5, cex = 0.5, font = 1,
			col  = c("gray", ifelse(ch == "ch1", "#0080ff", "#ff00ff")),
			fill = c("gray", ifelse(ch == "ch1", "#0080ff", "#ff00ff")),
			pch  = c(1, 21))

# create subset from 'cond'
	sel <- substitute(cond)
	myData <- subset(df, subset = eval(sel, df))
	myData <- droplevels(myData)

# relevel 'well' if direction is "row"
	if (by.direction == "row")
		myData$well <- with(myData, factor(well, levels = sort(levels(well))))
	myData <- myData[order(myData$well), ]

#	prepare x-values
	xw <- table(myData$well)	# width of each group
	xat <- c(1, cumsum(xw))		# x axis positions (plus 1)

# assemble formula and groups arguments
	form <- paste(ch, "~ seq_along(event)")
	form <- as.formula(form)
	grps <- paste(ch, "pos", sep = ".")

# assemble lattice object
	tp <- trellis.par.get()
	tp$superpose.symbol <- symbol
	if (is.null(main))
		main <- paste(deparse(substitute(df)), deparse(substitute(cond)), sep=", ")
	obj <- xyplot(form, myData, groups = myData[[grps]], par.settings = tp, ...)
	obj <- update(obj, panel = function(...) {
		panel.abline(v = xat, reference = TRUE)
		panel.grid(h = -1, v = 0)
		panel.superpose(...) })
	obj <- update(obj, scales = list(x = list(at = xw/2 + xat[-length(xat)],
		tck = 0, labels = levels(myData$well), rot = 90, cex = 2/3)))
	obj <- update(obj, xlab = xlab, ylab = ylab, main = main)
	obj
}
