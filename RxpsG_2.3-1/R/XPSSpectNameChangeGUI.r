#'GUI to change the name of the XPSSample and/or the name associated to corelines
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSSpectNameChange()
#'}
#'
#'@export
#'


XPSSpectNameChange <- function(){


#--- Global variables definition ---
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   SpectList <- "" #list of corelines present in the selected XPSSample
   FName <- NULL #FName represents the selected XPSSample(class XPSSample)
   activeFName <- NULL #activeFName is the name associated to the selected XPSSample (class character)
   XPSSampName <- NULL

#--- Widget  
      
      Labwin <<- gwindow("CHANGE LABELS", visible=FALSE)

      Labgroup1 <- ggroup(container = Labwin, horizontal=FALSE)
      Labframe1 <- gframe(" Select XPS-Sample ", horizontal=FALSE, spacing=5, container=Labgroup1)
      Labobj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h, ...){
                          activeFName <<- svalue(Labobj1)  #save the XPSSample name
                          XPSSampName <<- activeFName
                          FName <<- get(activeFName, envir=.GlobalEnv)  #load in FName the XPSSample data
                          OldSpectName <- names(FName)
                          SpectList <- data.frame("Names"=OldSpectName, stringsAsFactors=FALSE)
                          plot(FName)
                          svalue(Labobj5) <- FName@Filename
                          LL <- length(FName)
                          delete(Labframe2, LabGDF)
                          LabGDF <<- gdf(items=SpectList, container=Labframe2) #ridefine the combobox with the coreline names

                          size(LabGDF) <- c(100,100)
                          addHandlerChanged(LabGDF, handler=function(h, ...){
                                              idx <- svalue(LabGDF, drop=TRUE) <- ""
                                              NewSpectName <- as.character(h$obj[])
                                              names(FName) <<- NewSpectName
                                              for (ii in 1:LL){
                                                  if (NewSpectName[ii] != OldSpectName[ii] && NewSpectName[ii] != FName[[ii]]@Symbol){
                                                     FName[[ii]]@Symbol <<- NewSpectName[ii]
                                                     FName[[ii]]@RSF <<- 0 #otherwise XPSClass does not set RSF (see next row)
                                                     FName[[ii]] <<- XPSsetRSF(FName[[ii]])
                                                  }
                                              }
                                              svalue(Labobj3) <- " Spectrum Label and RSF changed!"
                                          })
                     }, container=Labframe1)

      Labframe2 <- gframe(" Change Spectrum Label Change ", horizontal=FALSE, spacing=5, container=Labgroup1)
      Labobj3 <- glabel(" ", container=Labframe2)
      LabGDF <- gdf(items=SpectList, container=Labframe2)
      size(LabGDF) <- c(100,100)


      Labframe4 <- gframe(" Set the New XPS-Sample Name ", horizontal=FALSE, spacing=5, container=Labgroup1)
      Labobj5 <- gedit("", handler=function(h,...){
                          blockHandler(Labobj5)
                          XPSSampName <<- svalue(Labobj1)
                          if (length(XPSSampName)==0){
                             gmessage(msg="Please Select the XPS-Sample", title="WARNING", icon="warning")
                             return()
                          }
                          XPSSampName <<- svalue(Labobj5)
                          if (length(strsplit(XPSSampName, "\\.")[[1]]) < 2) {
                             answ <- gconfirm(msg=".RData extension is lacking. Add extension?", title="WARNING", icon=c("warning"))
                             if (answ){
                                 XPSSampName <<- paste(XPSSampName, ".RData", sep="")
                                 svalue(Labobj5) <- XPSSampName
                             } else {
                                 gmessage(msg="Please check your XPS-Sample name. \n Nothing was changed.", title="WARNING", icon="warning")
                                 return()
                             }
                          }
                          FName@Filename <<- XPSSampName
                          PathName <- FName@Sample
                          FolderName <- dirname(PathName)
                          PathName <- paste(FolderName, "/", XPSSampName, sep="")
                          FName@Sample <<- PathName
                          unblockHandler(Labobj5)
                     }, container=Labframe4)
      Labobj6 <- glabel("", container=Labframe4)

      gbutton(" SAVE ", container=Labgroup1, handler=function(...){
       	                 if( activeFName != XPSSampName){ rm(list=activeFName, envir=.GlobalEnv) }
       	                 assign(XPSSampName, FName, envir=.GlobalEnv)
       	                 assign("activeFName", XPSSampName, envir=.GlobalEnv)
                         svalue(Labobj3) <- "  "
                         plot(FName)
                         XPSSaveRetrieveBkp("save")
                     })

      gbutton(" SAVE and EXIT ", container=Labgroup1, handler=function(...){
       	                 if( activeFName != XPSSampName){ rm(list=activeFName, envir=.GlobalEnv) }
       	                 assign(XPSSampName, FName, envir=.GlobalEnv)
       	                 assign("activeFName", XPSSampName, envir=.GlobalEnv)
                         plot(FName)
      	                  dispose(Labwin)
      	                  XPSSaveRetrieveBkp("save")
                     })

      visible(Labwin) <- TRUE
      Labwin$set_modal(TRUE)

}
