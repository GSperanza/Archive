#function to save XPS dataframes analyzed by XPS program

#'To Save data in the Hard Disk
#'
#'Provides a userfriendly interface to select a FileName and Directory
#'to save the analyzed XPS-Sample data.
#'Analyzed data by default have extension .Rdata.
#'No parameters are passed to this function
#'@examples
#'
#'\dontrun{
#'	XPSSaveData()
#'}
#'
#'@export
#'


XPSSaveData <- function() {

     ChDir <- function(){
          PathName <<- tk_choose.dir( default=getwd() )
          svalue(DestFolder) <- PathName
     }

     CutPathName <- function(PathName){
          if (nchar(PathName) > 40){
             splitPathName <- strsplit(PathName, "/")
             LL <- nchar(PathName[[1]])
             HeadPathName <<- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="") 
             ShortPathName <- paste(HeadPathName, substr(PathName, (LL-30), LL), sep="")
             return(ShortPathName)
          }
          return(PathName)
     }

     SaveSingle <- function(){
          FName <- get(activeFName, envir=.GlobalEnv)
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample
          if (PathName != getwd()){  #original folder different from the current wirking directory
             txt= paste("Warning: current and original directories are different. Do you want to save data in folder: \n", PathName, sep="")
             answ <- gconfirm(msg=txt, title="SET DESTINATION FOLDER", icon="warning")
             if (answ == FALSE) {
                ChDir()
             }
          }
          FName@Filename <- saveFName #save the new FileName in the relative XPSSample slot
          PathFileName <- paste(PathName, "/",saveFName, sep="")
          FName@Sample <- PathFileName

cat("\n 1111 FileName: ", saveFName)
cat("\n 2222 Path: ",PathName)
cat("\n 3333 PathFilename: ", PathFileName)


          assign(saveFName, FName, envir=.GlobalEnv)  #save the xxx.Rdata XPSSample in the .GlobalEnv
          RVersion <- as.integer(svalue(SaveToOldR, index=TRUE)) #by default no indication of the R Version is saved
          if (RVersion == 1) {
              RVersion <- NULL
          } else if (RVersion == 2) {
              RVersion <- 1  # code for R version < 1.4
          } else if (RVersion == 3) {
              RVersion <- 2  # code for R version <= 2
          }
          save(list=saveFName, file=PathFileName, version=RVersion, compress=TRUE)
          removeFName <- unlist(strsplit(activeFName, "\\."))   #in activeFName are initially .vms or .pxt or OldScienta fileNames
          if (removeFName[2] != "RData" || saveFName != activeFName){ #activeFName contains the original XPSSample Name
             remove(list=activeFName,pos=1,envir=.GlobalEnv)  #Now remove xxx.vms, xxx.pxt or the xxx.Rdata if a new name is given
          }
          assign("activeFName", saveFName, envir=.GlobalEnv)  #change the activeFName in the .GlobalEnv
          ShortPathName <- CutPathName(PathFileName)
          txt <- paste("\n Analyzed Data saved in: ", ShortPathName, sep="")
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }

     SaveAll <- function(){
          gmessage(msg="Each XPSSample will be saved in its original folder", title="Save All Data", icon="warning")
          FNameList <- XPSFNameList()
          LL=length(FNameList)
          for(jj in 1:LL){
              FName <- get(FNameList[jj], envir=.GlobalEnv)
              PathFileName <<- FName@Sample #get the file location path+filename
              pattStr <- FName@Filename
              idx <- gregexpr(pattStr, xxx, fixed=FALSE)
              idx <- unlist(idx)
              if (idx == -1){ # PathFileName contains only the PATH but not the FILENAME
                 PathFileName <- paste(PathFileName, "/", pattStr, sep="")
              }
              if (length(idx) == 1 && idx > 0) { #PathFileName contains both the PATH and the FILENAME
                 PathName <<- dirname(PathFileName)
              }
              if (length(idx) > 1 ) { #PathFileName is like Z:/X/LAVORI/R/Analysis/IPZS/Test.vms/Test.vms
                 PathName <- substr(PathFileName, 1, idx[1]-1)  #corresponds to Z:/X/LAVORI/R/Analysis/IPZS/
                 FNameList[jj] <- substr(xxx, idx[1], idx[2]-2) #corresponds to Test.vms
              }
              saveFName <<- unlist(strsplit(FNameList[jj], "\\."))
              saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample
              FName@Filename <- saveFName  #save the new FileName in the relative XPSSample slot
              PathFileName <- paste(PathName,"/",saveFName, sep="")
              FName@Sample <- PathFileName #the first time .vms files are transformed in .Rata new name will be saved
              assign(saveFName, FName, envir=.GlobalEnv)  #save the xxx.Rdata XPSSample in the .GlobalEnv
              save(list=saveFName, file=PathFileName, compress=TRUE)
              removeFName <- unlist(strsplit(FNameList[jj], "\\."))   #in FNameList are initially .vms or .pxt or OldScienta fileNames
              if (removeFName[2] != "RData" || saveFName != FNameList[jj]){
                remove(list=FNameList[jj],pos=1,envir=.GlobalEnv)   #xxx.Rdata is saved in .GlobalEnv Now remove xxx.vms, xxx.pxt
              }
              if (FNameList[jj] == activeFName){
                 assign("activeFName", saveFName, envir=.GlobalEnv) #change the activeFName in the .GlobalEnv
              }
              ShortFName <- CutPathName(PathFileName)
              txt <- paste("\n Analyzed Data saved in: ", ShortFName, sep="")
              cat("\n", txt)
          }
          ShortPathName <- CutPathName(PathName)
          XPSSaveRetrieveBkp("save")
     }

     GroupAndSave <- function(){
          saveFName <<- svalue(GroupName)
          if (saveFName == "") {
             gmessage(msg="PLEASE GIVE THE FILE NAME TO SAVE DATA" , title = "Saving Data",  icon = "warning")
             return()
          }
          DirName <<- svalue(DestFolder)
          if (DirName == NA || DirName=="" ) {
              DirName <<- getwd()
          }
          txt <- paste("SAVE DATA IN ", DirName, " ?", sep="")
          answ <- gconfirm(msg=txt, title = "Select Folder",  icon = "warning")
          if (answ == FALSE){
              DirName <<- tk_choose.dir( default=getwd() )
          }
          if (DirName == "") {
              DirName <<- getwd()
          }
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(DirName,"/",saveFName[1],".RData", sep="")
          FNameList <- XPSFNameList()
          save(list=FNameList, file=saveFName, compress=TRUE)
          ShortFName <- CutPathName(saveFName)
          txt <- paste("\n Analyzed Data saved in: ", ShortFName, sep="")
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }


#===== Variables =====
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   saveFName <- ""
   PathName <- getwd()
   saveFName <- get("activeFName", envir=.GlobalEnv)
   saveFName <- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
   saveFName <- paste(saveFName[1], ".Rdata", sep="")  #Compose the new FileName, adding .Rdata extension
   FNameList <- XPSFNameList()
   SpectIdx <- grep(activeFName, FNameList)
   DirName <- getwd()

#===== Command Window =====
   win <- gwindow("SAVE XPS-SAMPLE DATA", parent=c(30,0), visible=FALSE)

   group1 <- ggroup(label="SAVE DATA", horizontal=FALSE, container=win)

   DirFrame <- gframe(" Destination Directory ", spacing=5, horizontal=FALSE, container=group1)
   DestFolder <- glabel(PathName, sep=2, container=DirFrame)
   font(DestFolder) <- list(family = "helvetica", size="10") #list(family = "helvetica", size="12", weight="bold", style="italic")
   gbutton("  Change Directory   ", handler=function(h,...){
                      ChDir() #function Defined Here
                      svalue(DestFolder) <- PathName
                   },container=DirFrame)

   SourceFrame <- gframe("Source XPSSample", spacing=3, horizontal=FALSE, container=group1)
   XPSSample <- gcombobox(FNameList, selected=-1, spacing=7, handler=function(h,...){
                      saveFName <<- svalue(XPSSample)
                      assign("activeFName", saveFName, envir=.GlobalEnv)   #change the activeFName in the .GlobalEnv
                      saveFName <<- unlist(strsplit(saveFName, "\\."))      #not known if extension will be present
                      saveFName <<- paste(saveFName[1], ".Rdata", sep="")  #Compose the new FileName, adding .Rdata extension
                      svalue(SampName) <- saveFName
                      FName <- get(activeFName, envir=.GlobalEnv)
                      FilePath <- dirname(FName@Sample)
                      if (FilePath == ""){
                          FilePath <<- getwd()
                      }
                      if (PathName != "" && PathName != FilePath){
                          txt= paste("Warning: current and original directories are different. Do you want to save data in folder: \n", PathName, sep="")
                          answ <- gconfirm(msg=txt, title="SET DESTINATION FOLDER", icon="warning")
                          if (answ == FALSE) {
                             ChDir()
                          }
                      } else {
                          PathName <<- FilePath
                      }
                      svalue(DestFolder) <- PathName
                      plot(FName)
                   }, container=SourceFrame)

   DestFrame <- gframe("Destination File Name", spacing=5, horizontal=FALSE, container=group1)
   SampName <- gedit(text="", spacing=3, handler=function(h,...){
                     saveFName <<- svalue(SampName)  #the XPSSample name used to save data can be edited and modified
                   }, container=DestFrame)

   SaveToOldR <- gradio(c("Default R", "Save for R < 1.4.0", "Save for R <= 2"), selected=1, horizontal=TRUE, container=DestFrame)
   gbutton("   Save Selected XPS-Sample   ", spacing=7, handler=function(h,...){
                     SaveSingle()
                     delete(DestFrame, XPSSample)
                     FNameList <<- XPSFNameList()
                     XPSSample <<- gcombobox(FNameList, selected=-1, spacing=7, handler=function(h,...){
                                             saveFName <<- svalue(XPSSample)
                                             assign("activeFName", saveFName, envir=.GlobalEnv)   #change the activeFName in the .GlobalEnv
                                             saveFName <<- unlist(strsplit(saveFName, "\\."))      #not known if extension will be present
                                             saveFName <<- paste(saveFName[1], ".Rdata", sep="")  #Compose the new FileName, adding .Rdata extension
                                             svalue(SampName) <- saveFName
                                             FName <- get(activeFName, envir=.GlobalEnv)
                                             PathName <<- dirname(FName@Sample)
                                             if (PathName == ""){
                                                 PathName <<- getwd()
                                             }
                                             svalue(DestFolder) <- PathName
                                             plot(FName)
                                   }, container=SourceFrame)
                     svalue(SampName) <- ""
                   }, container=DestFrame)

   SaveSepFrame <- gframe(" Save All XPS-Samples Separated ", spacing=5, horizontal=FALSE, container=group1)

   gbutton("   Save All XPS-Samples   ", handler=function(h,...){ SaveAll() },container=SaveSepFrame)

   SaveGroupFrame <- gframe(" Save All XPS-Samples Together ", spacing=5, horizontal=FALSE, container=group1)
   GroupName <- gedit(text="", container=SaveGroupFrame)
   addHandlerChanged(GroupName, handler=function(h,...){
                     saveFName <<- svalue(GroupName)
                   })
   gbutton(" Group XPS-Samples and Save ", handler=function(h,...){ GroupAndSave() },container=SaveGroupFrame)

   gbutton("          Exit           ", handler=function(h,...){
                     dispose(win)
                     return(1)
                   },container=group1)

   visible(win) <- TRUE
   win$set_modal(TRUE)

}
