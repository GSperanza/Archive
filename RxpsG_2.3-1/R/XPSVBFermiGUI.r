#'GUI to estimate the position of the Valence Band Top
#'
#'Interactive GUI to add BaseLines and Fitting components to
#'the region of the VB proximal to the Fermi Edge needed
#'for the estimation of the VB-Top position
#'
#'@examples
#'
#'\dontrun{
#' XPSVBFermi()
#'}
#'
#'@export
#'


XPSVBFermi <- function() {

  LoadCoreLine <- function(h, ...){
     Object_name <- get("activeFName", envir=.GlobalEnv)
     Object <<- get(Object_name, envir=.GlobalEnv)  #load the XPSSample from the .Global Environment
     ComponentList <<- names(slot(Object[[coreline]],"Components"))
     if (length(ComponentList)==0) {
         gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
         return()
     }
     plot(Object)   #replot spectrum of the selected component
  }


  set.coreline <- function(h, ...) {
     CL <- svalue(Core.Lines)
     CL <- unlist(strsplit(CL, "\\."))   #select the NUMBER. before the CoreLine name
     coreline <<- as.integer(CL[1])

     if (coreline == 0) {    #coreline == "All spectra"
         svalue(plotFit) <- "normal"
         enabled(T1group1) <- FALSE  #block NB-baseline  tab
         enabled(T2group1) <- FALSE  #block NB-components tab
         plot(Object)
         gmessage(msg="Please select a Valence Band spectrum", title="WRONG SPECTRUM", icon="error")
         return()
     } else {
         if (length(Object[[coreline]]@Components) > 0) {
             gmessage(msg="Analysis already present on this Coreline!", title = "WARNING: Analysis Done",  icon = "warning")
             return()
         }
         enabled(T1group1) <- TRUE   #enable NB-baseline
         enabled(T2group1) <- TRUE   #enable plot options
         Object[[coreline]]@RSF <<- 0 #set the VB sensitivity factor to zero to avoid error wornings

         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)

     }
     ObjectBKP <<- Object[[coreline]]
     plot(Object[[coreline]])
  }


  reset.baseline <- function(h, ...) {
     if (coreline != 0) {   #coreline != "All spectra"
         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)
         enabled(T2group1) <<- FALSE
     }
  }


  update.outputArea <- function(...) {
     coreline <- svalue(Core.Lines)
     coreline <- unlist(strsplit(coreline, "\\."))   #drops the NUMBER. before the CoreLine name
     coreline <- as.integer(coreline[1])
  }


#--- Functions, Fit and VB_Top estimation

  reset.LinRegions <- function(h, ...) {
     Object[[coreline]]@Components <<- list()
     Object[[coreline]]@Fit <<- list()
     plot(Object[[coreline]])
  }


  add.FitFunct <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     gmessage(msg="Please define the mid point of the VB descendent tail", title="DEFINE POSITION", icon="warning")
     pos <- locator(n=1, type="p", col="red", lwd=2, cex=1.5, pch=3)
     if (coreline != 0 && hasBaseline(Object[[coreline]])) {
#Fit parameter are set in XPSAddComponent()
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBFermi",
                                             peakPosition = list(x = pos$x, y = pos$y) )
         Object[[coreline]]@Fit$y <<- Object[[coreline]]@Components[[1]]@ycoor-Object[[coreline]]@Baseline$y #subtract the Baseline
         Object[[coreline]]@RegionToFit$x <- ObjectBKP@RegionToFit$x #restore original abscissas changed in XPSAddComponent()
         plot(Object[[coreline]])
     }
  }


  del.FitFunct <- function(h, ...) {  #title="DELETE COMPONENT KILLS CONSTRAINTS!!!"
     ObjectBKP <<- Object[[coreline]]
     if (gconfirm(msg="Deleting fit function. Are you sure you want to proceed?", title="DELETE", icon="warning")) {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #Rimuovo tutti i CONSTRAINTS
              Object[[coreline]] <<- XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]])) {
             txt <- c("Select the fit component to delete")
             delWin <- gwindow("DELETE", parent = window, visible = FALSE)
             g <- gvbox(container=delWin); g$set_borderwidth(10L)
             glabel(txt, container=g)
             gseparator(container=g)
             compIdx <- gcombobox(c(names(slot(Object[[coreline]],"Components")),"All"), selected=1, container = g, handler = NULL)
             bg <- ggroup(container=g); addSpring(bg)
             gbutton("OK", container=bg, handler=function(...){
                     if (svalue(compIdx) != "All"){
                         indx <- as.numeric(svalue(compIdx, index=TRUE))
                         Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
                         if (length(Object[[coreline]]@Components) > 0 ) {
                             #to update the plot:
                             tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                             Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                         }
                     } else {
                         Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                     }
                     svalue(plotFit) <- "normal"
                     dispose(delWin)
             } )
             gbutton("Cancel", container=bg, handler=function(...) dispose(delWin))
             visible(delWin) <- TRUE
         }
     }
  }

  do.Fit <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     FitRes <- NULL

     if (coreline != 0) {  #VB Hill Sigmoid
         if (reset.fit==FALSE){
#Fit parameters and X coords are set in XPSAddComponent()
#also the FermiSigmoid was defined using the X coords
             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)   #Levenberg Marquardt fit
             if (svalue(plotFit) == "normal") plot(Object[[coreline]])             
             if (svalue(plotFit) == "residual") XPSresidualPlot(Object[[coreline]])
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             plot(Object[[coreline]])
         }
     }
  }

  do.VBFermi <- function(h, ...) {
      Ef.Posx <- Object[[coreline]]@Components[[1]]@param[2,1]
      Ef.Posx <- round(Ef.Posx, digits=4)
      idx <- findXIndex(Object[[coreline]]@RegionToFit$x, Ef.Posx)
      Ef.Posy <- Object[[coreline]]@Fit$y[idx]
      Object[[coreline]]@Components[[1]]@label <<- "VBFermi"        #Label indicating the VBFermi in the plot
      Ef.Pos <- list(x=Ef.Posx, y=Ef.Posy)
      plot(Object[[coreline]])
      points(Ef.Pos, col="orange", cex=2, lwd=2, pch=3)
      txt <- paste("==> Estimated position of Fermi Level : ", Ef.Posx, sep="")
      svalue(sb) <- txt
      cat("\n", txt)
  }


#----Set Default Variable Values
  ResetVars <- function(){
     Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")

     LL <- length(Object[[coreline]]@.Data[[1]])
     Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
     Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
     Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
     Object[[coreline]]@Boundaries$y[1] <- min(Object[[coreline]]@Boundaries$y)
     Object[[coreline]]@Boundaries$y[2] <- min(Object[[coreline]]@Boundaries$y)
     Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "linear", deg, splinePoints )

     VBbkgOK <<- FALSE
     VBlimOK <<- FALSE
     BType <<- "linear"
     svalue(sb) <<- "Estimated position of VB top : "
     enabled(T2group1) <<- FALSE
  }

#=====VARIABLES==================

  if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
  }

  Object <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
  Object_name <- get("activeFName", envir = .GlobalEnv) #XPSSample name
  ObjectBKP <- NULL   #CoreLine bkp to enable undo operation
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  coreline <- 0
  plot_win <- as.numeric(get("XPSSettings", envir=.GlobalEnv)$General[4]) #the plot window dimension
  BType <- "linear"
  VBbkgOK <- FALSE
  VBlimOK <- FALSE
  FitFunct <- "VBFermi"
  reset.fit <- FALSE

  WinSize <- as.numeric(XPSSettings$General[4])
  hh <- NULL
  ww <- NULL
  WinScale  <- NULL

#====== Widget definition =======

  VBwindow <- gwindow("XPS VB Top GUI", parent=c(80, 0), toolkit = "tcltk", visible = FALSE)
  size(VBwindow) <- c(400, 430)
  VBGroup <- ggroup(container = VBwindow, horizontal = TRUE)

  ## Core lines
  MainGroup <- ggroup(expand = FALSE, horizontal = FALSE, spacing = 5, container = VBGroup)

  SelectFrame <- gframe(text = " XPS Sample and Core line Selection ",horizontal = TRUE, container = MainGroup)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName <- svalue(XPS.Sample)
                                 Object <<- get(activeFName, envir=.GlobalEnv)
                                 Object_name <<- activeFName
                                 SpectList <<- XPSSpectList(activeFName)
                                 compIndx <<- grep("VB", SpectList)
                                 delete(SelectFrame, Core.Lines)
                                 Core.Lines <<- gcombobox(c("0.All spectra", SpectList), selected=1, handler = set.coreline, container = SelectFrame)
                                 coreline <<- 0
                                 VBbkgOK <<- FALSE
                                 VBlimOK <<- FALSE
                                 reset.baseline()
                                 enabled(T2group1) <<- FALSE
                                 plot(Object)
                       }, container = SelectFrame)
  svalue(XPS.Sample) <- activeFName

  Core.Lines <- gcombobox(c("0.All spectra", SpectList), selected=1, handler = set.coreline, container = SelectFrame)

  T1group1 <- ggroup(horizontal=FALSE, container = MainGroup)
  T1Frame1 <- gframe(text = " Process ", horizontal=FALSE, container = T1group1)

  T2group1 <- ggroup(horizontal=FALSE, container = MainGroup)
  T2Frame1 <- gframe(text = " Plot ", spacing=1, container = T2group1)
  plotFit <- gradio(items=c("normal", "residual"), selected=1,  expand=TRUE, horizontal = TRUE, handler = plot, container = T2Frame1)


#----- Fermi Sigmoid

  add2_btn1 <- gbutton("Add Fermi-Dirac function", handler = add.FitFunct, container = T1Frame1)
  Fit2_btn1 <- gbutton("Fit", expand=FALSE, handler = do.Fit, container = T1Frame1 )
  Fit2_GetFermi <- gbutton("Get Fermi Edge Position", expand=FALSE, handler = do.VBFermi, container = T1Frame1 )
  Reset2_btn1 <- gbutton("Reset Fit", expand=FALSE, handler = function(h, ...){
                              reset.fit <<- TRUE
                              do.Fit()
                              svalue(sb) <- "Estimated position of VB top : "
                           }, container = T1Frame1 )


#----- SAVE&CLOSE button -----
#  gseparator(container = MainGroup)
  ButtGroup <- ggroup(expand = FALSE, horizontal = TRUE, spacing = 3, container = MainGroup)

  gbutton("           SAVE           ", handler = function(h, ...){
                  activeSpecIndx <- coreline[1]
                  assign(Object_name, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  plot(Object[[coreline]])
                  XPSSaveRetrieveBkp("save")
              }, container = ButtGroup)

  gbutton("        SAVE & EXIT        ", handler=function(h,...){
                  activeSpecIndx <- coreline
                  assign(Object_name, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                  assign("activeSpectName", coreline, envir = .GlobalEnv)
                  dispose(VBwindow)
                  XPSSaveRetrieveBkp("save")
                  plot(Object[[coreline]])
              }, container = ButtGroup)

  gbutton("           EXIT           ", handler=function(h,...){
                  dispose(VBwindow)
                  XPSSaveRetrieveBkp("save")
                  plot(Object[[coreline]])
              }, container = ButtGroup)

  sb <- gstatusbar("status", container = VBwindow)

  enabled(T1group1) <- FALSE
  enabled(T2group1) <- FALSE

  visible(VBwindow) <- TRUE
}
