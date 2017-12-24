#########################################################################################
# usage - display functional information
#
usage <- function() {
message("NOT IMPLEMENTED YET")
return(NULL)

	txt <- c(
	" Usage:",
	"   df <- readData()   # read ddPCR data from CSV files",
	"",
	"   pd <- data.frame(well = levels(df$well), ...)",
	"",
	"   df  <- mergePdata(pd, df)  # merge with phenotype data in 'pd'",
	"   cut <- getThresh(df)  # determine threshold by well, row, column or all",
	"   df  <- score(df, cut) # assign positive values from threshold",
	"   res <- tally(df)      # tally positive drops and assign quadrant",
	"", 
	" Support:",
	"   threshold(x)   # determine threshold with option for more details",
	"   plot1d(df)     # plot tally with lattice",
	"   plotHist(df)   # histogram of each well with optional cutoff values")
	ft <- tempfile()
	writeLines(txt, con = ft, sep = "\n")
	file.show(ft)
}
