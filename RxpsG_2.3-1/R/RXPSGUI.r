#-----------------------------------------------------------------
#GUI to analyze XPS data files from Kratos and Scienta instruments
#-----------------------------------------------------------------
#'
#'Main GUI to process XPS-Samples data files
#'
#'@examples
#'
#'\dontrun{
#'xps()
#'}
#'@export
#'
#'
#' @import digest latticeExtra limSolve SparseM tcltk gWidgets2 gWidgets2tcltk
#' @importFrom baseline baseline baseline.peakDetection  .except baselineGUI
#' @importFrom FME modFit
#' @importFrom minpack.lm nlsLM nls.lm nls.lm.control
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
#' @importFrom "methods" "as" "formalArgs" "new" "show" "slot" "slot<-"

require(digest)
require(latticeExtra)
require(limSolve)
require(SparseM)
require(tcltk)
require(gWidgets2)
require(gWidgets2tcltk)

import::from(baseline, baseline, baseline.peakDetection, .except="baselineGUI")
import::from(FME, modFit)
import::from(grDevices, getGraphicsEvent, getGraphicsEventEnv, setGraphicsEventHandlers)
import::from(graphics, grconvertX, grconvertY)
import::from(minpack.lm, nlsLM, nls.lm, nls.lm.control)
import::from(NORMT3, wofz)
import::from(rootSolve, gradient)
import::from(signal, filter, sgolay, hamming, fir1, filtfilt, butter)
import::from(stats, as.formula, coef, convolve, fft, fitted, formula, getInitial, lm,
             model.weights, na.omit, nls.control, numericDeriv, predict, residuals,
             setNames, spline, window)
import::from(utils, capture.output, install.packages, read.table, write.csv, write.csv2, write.table)
import::from(wavelets, mra)
import::from(tcltk, tktoplevel, tkwm.geometry, tkgrid, tkconfigure, tkframe, tktitle, tkwinfo, tkbind, tkfocus)
import::from(tkrplot, tkrplot, tkrreplot)
import::from(grDevices, X11, bmp, dev.cur, dev.print, graphics.off, jpeg, pdf, png, postscript, recordPlot, replayPlot, tiff)
import::from(graphics, arrows, axTicks, axis, box, grid, layout, legend, lines, 
             locator, matlines, matplot, mtext, par, plot.new, points, rect, segments, text)
import::from(methods, as, formalArgs, new, show, slot, "slot<-")



xps <- function(){

   options(guiToolkit = "tcltk")
#===== Default variable settings ======

   tableXPS <- NULL

#===== GTable: active window INIT/UPDATE =====

setFileWin <- function(refresh) {
   if (refresh=="INIT") {         #GTable is created for the first time
      FNameList <- "              " #A temporary FName list is created to open a suitable GTableWin
      layoutXPS[1,1] <<- tableXPS <<- gtable(FNameList, container = layoutXPS)
      names(tableXPS) <<- "XPS Samples"
      size(tableXPS) <- c(200,260)
   }
   if (refresh=="UPDATE") {
      FNameList <- XPSFNameList()    #Update the FName list
      layoutXPS[1,1] <<- tableXPS <<-gtable(FNameList, container = layoutXPS)
      names(tableXPS) <<- "XPS Samples"
      size(tableXPS) <- c(200,260)
   }

   addHandlerDoubleclick(tableXPS,handler=function(h,...){   #The selected XPSSample name is read only with doubleclick
           if(FNameList[1]!="              "){
              FName <- svalue(tableXPS)
              activeFName <- FName
              SpectList <- unlist(XPSSpectList(FName))       #here the list of Core Lines is generated from the FName datafile
              FName <- get(FName,envir=.GlobalEnv)
              activeSpectName <- FName[[1]]@Symbol
              assign("activeFName", activeFName, envir=.GlobalEnv)
              assign("activeSpectIndx", 1, envir=.GlobalEnv) #select first spectrum as activeSpectIndx in GlobalEnv.
              assign("activeSpectName", activeSpectName, envir=.GlobalEnv)  #select name of the first spectrum as come activeSpectIndx in GlobalEnv.
              Gdev <- unlist(strsplit(XPSSettings$General[6], split="'")) #drop the wrong title
              Gdev <- paste(Gdev[1], "activeFName)", sep="")     #add the correct window title
              graphics.off() #switch off the graphic window
              eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
              plot(FName)  #plot selected XPSSample with all the corelines

              menulist <- list()
              LL <- length(SpectList) #Right mouse button activated the list of XPSSample Corelines
              for (ii in 1:LL){     #each of the gactions has the name of the correspondent Core Line
                  menulist[ii] <- list(gaction(label=SpectList[ii], action=ii, handler=function(h,...) {
                                XPSComponent <- SpectList[as.integer(h$action)]
                                XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #remove "NUMBER." at beginning of coreline name
                                indx <- as.integer(XPSComponent[1])
                                XPSComponent <- XPSComponent[2]
                                FName <- get(activeFName, envir=.GlobalEnv)
                    	           par(mfrow=c(1,1))   #reset plot to single panel (1 row 1 column)
                                plot(FName[[indx]]) #plot single spectrum
                                assign("activeFName", activeFName, envir=.GlobalEnv)  #save the active XPSSample i the .Global Env
                                assign("activeSpectIndx", indx, envir=.GlobalEnv)     #save the  index relative to the active Core Line in the .Global Env
                                assign("activeSpectName", FName[[indx]]@Symbol, envir=.GlobalEnv)  #save the name of the active Core Line in the .Global Env
                                XPSFitInfo()
                              })
                           )
              }
              popup <- addRightclickPopupMenu(tableXPS, menulist, ID=TRUE)
          }
      })
}



#=======================================================

#===== Menu FILE: actions definition =====

   FileActions <- list(

      gaction("= Load VMS, PXT Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                          filter = list("VMS, PXT files" = list(patterns=c( ".vms", ".pxt"))),
					                         multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            activeFName <- FName
            FName <- XPSread(file=PathFile,Genplot=FALSE)
            assign(activeFName, FName, envir=.GlobalEnv)             #save FName class XPSSample to run XPSSpectList()
            assign("activeFName", activeFName, envir=.GlobalEnv)     #selected FName is set the activeFName class character
            SpectList <- names(FName)
            if (length(indx <- grep("survey",SpectList))==0){
               indx <- grep("Survey",SpectList)
            }
            if (length(indx)==0){
               if (length(indx <- grep("wide",SpectList))==0){
                  indx <- grep("Wide",SpectList)
               }
            }
            if (length(indx)==0){
               WideSpectName <- NULL
               winCL <- gwindow(" UNKNOWN NAME OF WIDE SPECTRUM ", visible=FALSE)
               CLGroup <- ggroup(horizontal=FALSE, container=winCL)
               glabel("  ", container=CLGroup)
               txt <- glabel("Please give the name used for your wide spectra", container=CLGroup)
               font(txt) <- list(family="sans",size=12)
               glabel("  ", container=CLGroup)
               WSname <- gedit(text="", initial.msg = "Name of wide spectrum", spacing = 10, handler=function(h, ...){
                                   WideSpectName <<- svalue(WSname)
                                   dispose(winCL)
                               }, container=CLGroup)
               visible(winCL) <- TRUE
               winCL$set_modal(TRUE)  #nothing can be done while running this macro
               indx <- grep(WideSpectName,SpectList)
            }
#---Controls on XPSSample
            if (length(indx) > 0){ #Change the name WIDE in the vamas files in SURVEY
               LL <- length(indx)
               for (ii in 1:LL){ #if more than one WIDE spectra are present in the SpectList, indx is a vector containing the WIDES of the SpectList
                  names(FName)[ indx[ii] ] <- "Survey"
                  FName[[indx[ii] ]]@Symbol <- "Survey"
                  SpectList[[indx[ii] ]] <- paste(as.character(indx[ii]),".Survey", sep="")
               }
            }
            FName <- XPSpkgCtrl(FName) #controls the "package" attributes "package" if it is  the old Rxps version
            FName@Sample <- PathFile   #save the file location
            XPSComponent <- unlist(strsplit(SpectList[1], "\\."))   #Skip the "number." at beginning CoreLine name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the CoreLine index as active index in .Global env
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #save the CoreLine name as active name in the .Global env
            assign(activeFName, FName, envir=.GlobalEnv)  #Save the XPSSample name as active in the .Global env

            setFileWin("UPDATE")
            print(summary(FName))
            Gdev <- unlist(strsplit(XPSSettings$General[6], split="'")) #drop the Gdev wrong title
            Gdev <- paste(Gdev[1], "activeFName)", sep="")     #add the correct window title
            graphics.off() #switch off the graphic window
            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("   Load Old Scienta files",handler=function(h,...){
            gmessage(msg="Please select one of the files in the .../ANALYSIS/ folder", title="LOAD OLD SCIENTA", icon="warning")
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                  filter = list(),
					               multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            assign("activeFName", FName, envir=.GlobalEnv)   #selected FName is set the activeFName class character
            FName <- XPSread(file=PathFile,Genplot=FALSE)
            FName@Sample <- DirName                          #agrees the real path with that saved in the file which could be copied from another place
            assign(activeFName, FName, envir=.GlobalEnv)     #save FName class XPSSample to run XPSSpectList()
            SpectList <- names(FName)
            if (length(indx <- grep("survey",SpectList))==0){
               indx <- grep("Survey",SpectList)
            }
            if (length(indx)==0){
               if (length(indx <- grep("wide",SpectList))==0){
                  indx <- grep("Wide",SpectList)
               }
            }
            if (length(indx)==0){
               WideSpectName <- NULL
               winCL <- gwindow(" UNKNOWN NAME OF WIDE SPECTRUM ", visible=FALSE)
               CLGroup <- ggroup(horizontal=FALSE, container=winCL)
               glabel("  ", container=CLGroup)
               txt <- glabel("Please give the name used for your wide spectra", container=CLGroup)
               font(txt) <- list(family="sans",size=12)
               glabel("  ", container=CLGroup)
               WSname <- gedit(text="", initial.msg = "Name of wide spectrum", spacing = 10, handler=function(h, ...){
                                   WideSpectName <<- svalue(WSname)
                                   dispose(winCL)
                               }, container=CLGroup)
               visible(winCL) <- TRUE
               winCL$set_modal(TRUE)  #nothing can be done while running this macro
               indx <- grep(WideSpectName,SpectList)
            }
            #---Controls on XPSSample
            if (length(indx)>0){  #tipicamente per gli spettri .vms cambio il nome WEIDE in SURVEY
               LL=length(indx)
               for (ii in 1:LL){  #se c'e' piu' di un WIDE nel XPSSamp, indx e' un vettore contenete gli indici dei WIDE in SpectList
                  names(FName)[ indx[ii] ] <- "Survey"
                  FName[[indx[ii] ]]@Symbol <- "Survey"
                  SpectList[[indx[ii] ]] <- paste(as.character(indx[ii]),".Survey", sep="")
               }
            }
            FName <- XPSpkgCtrl(FName) #control that the "package" attributes of FName : it should'nt be Rxps  (Canteri)
#------------
            FName@Sample <- PathFile   #save the file location
            XPSComponent <- unlist(strsplit(SpectList[1], "\\."))   #skip the "NUMBER." at beginninc of Core Line Name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index of the first Core Line of XPSSample as active index
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  # save the name of the first Core Line of the loaded XPSSample as active Spectrum Name
            assign(activeFName, FName, envir=.GlobalEnv)    #save the loaded XPSSample in the GlobalEnv.
            setFileWin("UPDATE")
            print(summary(FName))
            Gdev <- unlist(strsplit(XPSSettings$General[6], split="'")) #drop the Gdev wrong title
            Gdev <- paste(Gdev[1], "activeFName)", sep="")     #add the correct window title
            graphics.off() #switch off the graphic window
            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("   Load PXT+RPL Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
       	                      filter = list("PXT files" = list(patterns= ".pxt")),
                              multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            command <- (paste(FName,"<-XPSread(file='",PathFile,"',Genplot=TRUE)", sep=""))
            eval(parse(text=command),envir=.GlobalEnv)           #here FName is a string
            activeFName <- FName
            assign("activeFName", activeFName, envir=.GlobalEnv) #set the active FName to the last read file
            FName <- get(activeFName,envir=.GlobalEnv)
#---Controls on XPSSample
            SpectList <- names(FName)
            if (length(indx <- grep("survey",SpectList))==0){
               indx <- grep("Survey",SpectList)
            }
            if (length(indx)==0){
               if (length(indx <- grep("wide",SpectList))==0){
                  indx <- grep("Wide",SpectList)
               }
            }
            if (length(indx)==0){
               WideSpectName <- NULL
               winCL <- gwindow(" UNKNOWN NAME OF WIDE SPECTRUM ", visible=FALSE)
               CLGroup <- ggroup(horizontal=FALSE, container=winCL)
               glabel("  ", container=CLGroup)
               txt <- glabel("Please give the name used for your wide spectra", container=CLGroup)
               font(txt) <- list(family="sans",size=12)
               glabel("  ", container=CLGroup)
               WSname <- gedit(text="", initial.msg = "Name of wide spectrum", spacing = 10, handler=function(h, ...){
                                   WideSpectName <<- svalue(WSname)
                                   dispose(winCL)
                               }, container=CLGroup)
               visible(winCL) <- TRUE
               winCL$set_modal(TRUE)  #nothing can be done while running this macro
               indx <- grep(WideSpectName,SpectList)
            }
            if (length(indx)>0){  #tipicamente per gli spettri .vms cambio il nome WEIDE in SURVEY
               LL=length(indx)
               for (ii in 1:LL){
                   names(FName)[ indx[ii] ] <- "Survey"
                   FName[[indx[ii] ]]@Symbol <- "Survey"
                   SpectList[[indx[ii] ]] <- paste(as.character(indx[ii]),".Survey", sep="")
               }
            }
            FName <- XPSpkgCtrl(FName) #control on the package attributes
#------------
            FName@Sample <- PathFile   #save the file location
            XPSComponent <- unlist(strsplit(SpectList[1], "\\."))
            assign("activeSpectIndx", 1, envir=.GlobalEnv)
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)
            assign(activeFName, FName, envir=.GlobalEnv)

            setFileWin("UPDATE")
            print(summary(FName))
            Gdev <- unlist(strsplit(XPSSettings$General[6], split="'")) #drop the Gdev wrong title
            Gdev <- paste(Gdev[1], "activeFName)", sep="")     #add the correct window title
            graphics.off() #switch off the graphic window
            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("= Load Analyzed Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                 filter = list("RData files" = list(patterns= c("*.RData", "*.RDF"))),
			               	  multi = FALSE)
				        if (length(PathFile)==0) {return()}       #when load-file-action aborted
            FName <- basename(PathFile)
            activeFName <- FName
            DirName <- dirname(PathFile)
            setwd(DirName)
            cat("\n New Working Directory: ", DirName)
            CheckName <- unlist(strsplit(FName, "\\." ))
            FNameList <- NULL
            if (CheckName[2]=="RData"){               #Load .Rdata files
               FNameList <- load(PathFile,envir=.GlobalEnv) #It could be that the dataFile is a group of XPSSample saved together. Then I need a FNameList
               LL <- length(FNameList)
               for(ii in 1:LL){                       #Update the FileName of the XPSSample changing the extension to .RData
                   FName <- get(FNameList[ii],envir=.GlobalEnv)  #load XPSSample data in FName
                   FName <- XPSpkgCtrl(FName)         #controls the attribute "package" of FName and set it to ".GlobalEnv"
                   FName@Sample <- DirName            #agrees the real path with that saved in the file which could be copied from another place
                   if (activeFName != FNameList[1]){  #It happen that the XPSSample==FNameList[1] still contains the original name XXX.vms or XXX.pxt instead of XXX.RData
                      badFName <- FNameList[ii]
                      CheckName[1] <- unlist(strsplit(FNameList[ii], "\\." ))
                      FNameList[ii] <- paste(CheckName[1], ".RData", sep="") #build the correct FileName with extension .RData
                      FName@Filename <- FNameList[ii]        #Save the new FileName in the XPSSample
                      CheckName[1] <- paste(DirName, "/", FNameList[ii], sep="")
                      FName@Sample <- CheckName[1]           #Save the new FileName in the XPSSample
                      remove(list=badFName,pos=1,envir=.GlobalEnv)   #remove old FName from .GlobalEnv
                      assign(FNameList[ii], FName, envir=.GlobalEnv) #store the XPSSample in the GlobalEnv
                   } 
               }
               FName <- get(FNameList[1],envir=.GlobalEnv)#the first element of the FNameList must be the active XPSSample
               activeFName <- FNameList[1]
               assign("activeFName", FNameList[1], envir=.GlobalEnv) #The first XPSSample_name loaded becomes the activeFName
            }

            if (CheckName[2]=="RDF"){                       #Load .RDF files
               FName <- readRDS(PathFile)
               FName <- XPSpkgCtrl(FName)           #controls the attribute "package" of FName and set it to ".GlobalEnv"
               CheckName <- paste(CheckName[1], ".RData", sep="") #build the correct FileName with extension .RData
               FName@Filename <- CheckName              #Save the new FileName in the XPSSample
               activeFName <- CheckName
               assign("activeFName", CheckName, envir=.GlobalEnv)     #selected FName is set the activeFName class character
               CheckName <- paste(DirName, "/", CheckName, sep="")
               FName@Sample <- CheckName                #Save the new FileName in the XPSSample
               assign(activeFName, FName, envir=.GlobalEnv)         #save FName data  (class XPSSample)
            }
#---Assignements
            SpectList <- XPSSpectList(activeFName)
            XPSComponent <- unlist(strsplit(SpectList[1], "\\."))    #drop the "NUMBER." at beginninf of the coreline name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index corresponding to the active CoreLine in the .GlobalEnv.
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #salvo l'INDICE del file caricato come ATTIVO nel GlobalEnv.
            assign(activeFName, FName, envir=.GlobalEnv)  #save the loaded XPSSample in the .GlobalEnv.
            setFileWin("UPDATE")
            print(summary(FName))
            Gdev <- unlist(strsplit(XPSSettings$General[6], split="'")) #drop the Gdev wrong title
            Gdev <- paste(Gdev[1], "activeFName)", sep="")     #add the correct window title
            graphics.off() #switch off the graphic window
            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("= Save Analyzed Data", handler=function(h,...){
            XPSSaveData() #errmsg==1 XPSSaveData()executed regularly
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Import Ascii", handler=function(h,...){
            FName <- XPSImport.Ascii()
            if (is.na(FName)) {
               cat("\n import Ascii file Aborted")
            } else {
               setFileWin("UPDATE")
               plot(FName)
            }
      }, container=groupMenu),

      gaction("   Export Ascii", handler=function(h,...){
            FName<-get(activeFName, envir=.GlobalEnv)
            XPSExportAscii()
      }, container=groupMenu),

      gaction("   Split XPS Data File", handler=function(h,...){
            XPSSplit()
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Change Spectrum Label",handler=function(h,...){
            XPSSpectNameChange()
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Remove Current XPS-Sample", handler=function(h,...){
            answ <- gconfirm(msg="Removing the XPS Samples: all data/analyses will be lost. Proceed anyway?", title="Confirm Remove XPSSample", icon="warning")
            if (answ==TRUE){
               FName=get("activeFName",envir=.GlobalEnv)
               remove(list=FName,pos=1,envir=.GlobalEnv)
               FNameList <- XPSFNameList(warn=FALSE)
               LL <- length(FNameList)
               activeFName <- FNameList[1]
               activeSpectIndx <- 1
               assign("activeFName", activeFName, envir=.GlobalEnv)
               assign("activeSpectIndx", activeSpectIndx, envir=.GlobalEnv)

               setFileWin("UPDATE")
               if (LL>0){
                  FName=get(activeFName,envir=.GlobalEnv)
                  plot(FName)
               } else {
                  Gdev <- get("XPSSettings", envir=.GlobalEnv)[[1]][6] #graphic device selected
                  graphics.off()                                   #reset graphic window
                  eval(parse(text=Gdev),envir=.GlobalEnv)          #open graphic window
               }
            }
      }, container=groupMenu),

      gaction("   Remove All XPS-Samples", handler=function(h,...){
            answ <- gconfirm(msg="Removing all the XPS Samples: all data/analyses will be lost. Proceed anyway?", title="Confirm Remove XPSSample", icon="warning")
            if (answ==TRUE){
               FNameList <- XPSFNameList(warn=TRUE)
               LL=length(FNameList)
               for(ii in 1:LL){
                  FName <- FNameList[ii]
                  remove(list=FName,pos=1,envir=.GlobalEnv)
               }
               FNameList <- XPSFNameList(warn=FALSE)
               assign("activeFName", "", envir=.GlobalEnv)
               assign("activeSpectIndx", "", envir=.GlobalEnv)

               setFileWin("UPDATE")
               Gdev <- get("XPSSettings", envir=.GlobalEnv)[[1]][6] #graphic device selected
               graphics.off()                                   #reset graphic window
               eval(parse(text=Gdev),envir=.GlobalEnv)          #open graphic window
            } else {
              return()
            }
      }, container=groupMenu),

      gaction("   Set Working DIR", handler=function(h,...){
            XPSSetWD()
      }, container=groupMenu),

      gaction("   Preferences", handler=function(h,...){
            XPSPreferences()
      }, container=groupMenu),

      gaction("   Retrieve BKP-data", handler=function(h,...){
            XPSSaveRetrieveBkp("retrieve")
            setFileWin("UPDATE")
            FNameList <- XPSFNameList() #read the list of XPSSample loaded in the .GlobalEnv
            XPSSample <- get(FNameList[1], envir=.GlobalEnv)
            plot(XPSSample)
      }, container=groupMenu),

      gaction("   Refresh XPS Sample List",handler=function(h,...){
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Quit", handler=function(h,...){
            ReturnVal <- tkmessageBox(message = "Do you want to save data before quitting?",
                                    icon = "question", type = "yesnocancel", default = "yes")
            answ <- tclvalue(ReturnVal)
            if (answ == "yes"){
               XPSSaveData()
            }
            else if (answ == "no"){
                dispose(winXPS)
            }
            else if (answ == "cancel"){ }
      }, container=groupMenu))

#===== Menu ANALYSIS: actions definition =====

   AnalysisActions <- list(
      gaction("Spectrum selection",handler=function(h,...){
             XPSSetFNameCLine()
      }, container=groupMenu),

      gaction("= Analyze",handler=function(h,...){
             XPSAnalysis()
             FName <- get(activeFName,envir=.GlobalEnv)
      }, container=groupMenu),

      gaction("= Fit Constraints",handler=function(h,...){
             XPSConstraints()
      }, container=groupMenu),

      gaction("   FIT Lev.Marq.",handler=function(h,...){
             FName <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             FName[[indx]] <- XPSFitLM(FName[[indx]], , plt=TRUE)
             assign(activeFName, FName, envir=.GlobalEnv)
      }, container=groupMenu),

      gaction("   FIT ModFit",handler=function(h,...){
             FName <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             FName[[indx]] <- XPSModFit(FName[[indx]])
             assign(activeFName, FName, envir=.GlobalEnv)
      }, container=groupMenu),

      gaction("= Move Components",handler=function(h,...){
             XPSMoveComp()
      }, container=groupMenu),

      gaction("= Quantify",handler=function(h,...){
             XPSQuant()
      }, container=groupMenu),

      gaction("= Energy Shift",handler=function(h,...){
             XPSEshift()
      }, container=groupMenu),

      gaction("   Process Coreline",handler=function(h,...){
             XPSProcessCoreLine()
      }, container=groupMenu),

      gaction("   Extract from survey",handler=function(h,...){
             SpectList <- XPSSpectList(activeFName)
             SpectName <- "Survey"
             indx <- grep(SpectName, SpectList, value=FALSE)
             if (length(indx) == 0){
                SpectName <- "survey"
                indx <- grep(SpectName, SpectList, value=FALSE)
                if (length(indx) > 0){ assign("activeSpectIndx", indx, envir=.GlobalEnv) }
             }
             if (length(indx) == 0){
                answ <- gconfirm(msg = "Sorry, no survey in this XPSsample. Proceed anyway?", title="SPECTRUM ERROR", icon = "warning")
                if (answ == FALSE) return
                XPSExtract()
             } else if (indx > 0){
                assign("activeSpectIndx", indx[1], envir=.GlobalEnv)
                XPSExtract()
             }
      }, container=groupMenu),

      gaction("   Move Baseline",handler=function(h,...){
             XPSMoveBaseLine()
      }, container=groupMenu),

      gaction("   Smoothing",handler=function(h,...){
             XPSFilter()
      }, container=groupMenu),

      gaction("   Differentiate",handler=function(h,...){
             XPSDiff()
      }, container=groupMenu),

      gaction("   VBtop estimation",handler=function(h,...){
             XPSVBTop()
      }, container=groupMenu),

      gaction("   Fermi edge estimation",handler=function(h,...){
             XPSVBFermi()
      }, container=groupMenu),

      gaction("   Reset Analysis",handler=function(h,...){
             XPSResetAnalysis()
      }, container=groupMenu),

      gaction("   Sprucing Up",handler=function(h,...){
             XPSSprucingGUI()
      }, container=groupMenu),

      gaction("   Element Identification",handler=function(h,...){
             XPSSurveyElementIdentify()
      }, container=groupMenu),

      gaction("   Corelines Auger Transition Tables",handler=function(h,...){
             XPSElemTab()
      }, container=groupMenu),

      gaction("   VMS Data Transmission Correction",handler=function(h,...){
             XPSVmsCorr()
      }, container=groupMenu))


#===== Menu PLOT: actions definition =====

   PlotActions <- list(
      gaction("Plot",handler=function(h,...){
             #Load the active XPSSample
             FName <- get(activeFName,envir=.GlobalEnv)
             plot(FName)
      }, container=groupMenu),

      gaction("Spectrum Selection",handler=function(h,...){
             XPSSetFNameCLine()
      }, container=groupMenu),

      gaction("Overlay Spectra",handler=function(h,...){
             XPSOverlay()
      }, container=groupMenu),

      gaction("Compare XPS-Samples",handler=function(h,...){
             XPSCompare()
      }, container=groupMenu),

      gaction("Custom Plot",handler=function(h,...){
             XPSCustomPlot()
      }, container=groupMenu),

      gaction("Two-Yscale Plot",handler=function(h,...){
             XPSTwoScalePlot()
      }, container=groupMenu),

      gaction("Annotate",handler=function(h,...){
             XPSAnnotate()
      }, container=groupMenu),

      gaction("Zoom & Cursor",handler=function(h,...){
             XPSZoomCur()
      }, container=groupMenu),

      gaction("Switch BE/KE scale",handler=function(h,...){
             XPSSwitch.BE.KE()
      }, container=groupMenu),

      gaction("Graphic Device Options",handler=function(h,...){
             XPSSetGraphDev()
      }, container=groupMenu),

      gaction("Set Analysis Window Size",handler=function(h,...){
          XPSSetWinSize()
      }, container=groupMenu))


#===== MENU infoFile: ACTIONS definition =====

   ListInfoActions <- list(
      gaction("XPS Sample Info",handler=function(h,...){
          XPSSampleInfo()
      }, container=groupMenu),

      gaction("Core Line Fit Info",handler=function(h,...){
          XPSFitParamInfo()
      }, container=groupMenu),
   
      gaction("Analysis Report",handler=function(h,...){
          XPSReport()
      }, container=groupMenu),

      gaction("Help", handler = function(h,...){
          pth <- system.file("doc/manual.pdf", package="RxpsG")
          if (file.exists(pth)) {
            OS <- unname(tolower(Sys.info()["sysname"]))
            switch(OS,
                   "windows" = {
                     #since spaces (such as c:\Program Files\...) are interpreted by shell command
                     WD <- getwd()
                     #it is necessary to set the folder containing manual.pdf to open it
                     setwd(system.file("doc/", package = "RxpsG"))
                     shell("manual.pdf", wait = FALSE)
                     setwd(WD)     #restore previous WD
                   },
                   "linux" = {
                     CMD <- paste("evince ", pth)
                     system(CMD)
                   },
                   "darwin" = {
                     CMD <- paste("open ", pth)
                     system(CMD)
                   })
          }
          else {
            txt <- paste( "Oops... Manual not found! \nPlease check the folders doc/ if manual.pdf is present", sep = "")
            gmessage(txt, title = "WARNING: MANUAL NOT FOUND", icon = "warning")
          }
      }, container=groupMenu))

   MenuBarList <- list(File=FileActions, Analysis=AnalysisActions, Plot=PlotActions, Info_Help=ListInfoActions)

#==== Loading RxpsG picture=
   IMGpath <- system.file("extdata/xps.gif", package="RxpsG")
   if ( ! file.exists(IMGpath) ) {
       gmessage(msg="ATTENTION: xps.gif file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
       return()
   }

#===== XPS main Panel ======

   winXPS <- gwindow("RxpsG MAIN PANEL", parent=c(5,5), visible=FALSE)
   gmenu(MenuBarList, container=winXPS)
   groupXPS <- ggroup(horizontal=TRUE, container=winXPS)

   layoutXPS <- glayout(homogeneous=FALSE, spacing=3, container=groupXPS)
   FNameList <- "              " #A temporary FName list is created to open a suitable GTableWin
   layoutXPS[1,1] <-tableXPS <-gtable(FNameList, container = layoutXPS)
   names(tableXPS) <-"XPS Samples"
   size(tableXPS) <- c(200,260)
   layoutXPS[1,2] <- imageXPS <-gimage(filename = IMGpath, dirname ="", size=c(100,70), toolkit=guiToolkit(), container = layoutXPS)

   FNameList <- XPSFNameList(warn=FALSE)
   if (length(FNameList) > 0) {
      setFileWin("UPDATE")
   }
   visible(winXPS) <- TRUE


#Reading XPS settings which can be customized by users
   XPSSettings <- data.frame(stringsAsFactors=FALSE)
   FontPref <- list(Font="", Style="", Size="")
   
   Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
   if (file.exists(Ini.pthName)) {
     XPSSettings <- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
   }
   else {
     gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
     return()
   }

# setting the proper graphic device
#--- get System info and apply correspondent XPS Settings ---
   OS <- unname(tolower(Sys.info()["sysname"]))
   switch (OS,
           "linux" =   {Gdev <- "X11(xpos=600, ypos=5, title=' ')" },
           "windows" = {Gdev <- "X11(xpos=600, ypos=5, title=' ')"},
           "darwin"  = {Gdev <- "quartz()" })
   XPSSettings$General[6] <- Gdev
   if (length(XPSSettings$General[7]) == 0 || length(dir(XPSSettings$General[7])) == 0){
      gmessage("Working Dir NOT defined: please select your default Working Directory", title="SET THE WORKING DIR!", icon="error")
      XPSSetWD()
   } else {
      setwd(XPSSettings$General[7]) 
   }
   graphics.off()                            #reset graphic window
   eval(parse(text=Gdev),envir=.GlobalEnv)
   assign("XPSSettings", XPSSettings, envir=.GlobalEnv)

# Recover the R version used by Rstudio and save in .GlobalEnv
   RVersion <- R.Version()$version.string  #get the $version.string from the output list of R.Version()
   assign("RVersion", RVersion, envir=.GlobalEnv)
}

