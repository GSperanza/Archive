#' Package RxpsG
#'
#' Processing tools for X ray Photoelectron Spectroscopy Data.
#' The package implements classes and methods to read, visualize and manipulate XPS spectra files.
#' The spectra can be obtained from .pxt and .vms data format from different instruments.
#'
#' More generally, any data that is recorded as a list of (x,y) is suitable.
#' @import limSolve digest grid latticeExtra SparseM tcltk gWidgets2 gWidgets2tcltk
#' @importFrom baseline baseline baseline.peakDetection 
#' @importFrom FME modFit
#' @importFrom grDevices getGraphicsEvent getGraphicsEventEnv setGraphicsEventHandlers
#' @importFrom graphics grconvertX grconvertY
#' @importFrom methods as formalArgs new show slot slot<-
#' @importFrom minpack.lm nlsLM nls.lm nls.lm.control summary.nls.lm
#' @importFrom NORMT3 wofz
#' @importFrom rootSolve gradient
#' @importFrom signal filter sgolay hamming fir1 filtfilt butter
#' @importFrom stats as.formula coef convolve fft fitted formula getInitial lm model.weights na.omit nls.control numericDeriv predict residuals setNames spline window
#' @importFrom utils capture.output install.packages read.table write.csv write.csv2 write.table
#' @importFrom wavelets mra
#' @importFrom tcltk tktoplevel tkwm.geometry tkgrid tkconfigure tkframe tktitle tkwinfo tkbind tkfocus
#' @importFrom tkrplot tkrplot tkrreplot
#' @importFrom grDevices X11 bmp dev.cur dev.print graphics.off jpeg pdf png postscript recordPlot replayPlot tiff
#' @importFrom graphics arrows axTicks axis box grid layout legend lines locator matlines matplot mtext par plot.new points rect segments text
#' @docType package
#' @name RxpsG
NULL




