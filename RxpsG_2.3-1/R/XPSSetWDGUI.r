#'To set the default working dir if it is not defined
#'
#'The new default working dir will be saved in the XPSSettings.ini
#'which uincludes also the user preferences.
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSSetWD()
#'}
#'
#'@export
#'

XPSSetWD <- function(){
   XPSSettings <- NULL
   WorkingDir <- getwd()

   MainWin <- gwindow("SET WORKING DIR", parent=c(0, 20), visible=FALSE)
#   size(MainWin) <- c(360, 100)
   MainGroup <- ggroup(horizontal=FALSE, container=MainWin)
   MainFrame <- gframe(text="SELECT NEW WORKING DIR", horizontal=TRUE, spacing=5, container=MainGroup)
   newDir <- gbutton("Browse Dir", spacing=1, handler=function(h, ...){
                     SysName <- Sys.info()
                     SysName <- SysName[1]
                     if (SysName=="Linux") {
                        WDir <- tk_choose.dir()
                     } else {
                        WDir <- choose.dir(default="", caption="SELECT YOUR WORKING DIRECTORY")
                     }
                     WorkingDir <<- paste(dirname(WDir), "/", basename(WDir), sep="") #cambia i separatori da \\ a
                     ForbidChars <- c("-")
                     xxx <- sapply(ForbidChars, grep, x=WorkingDir)
                     xxx <- sapply(xxx, length )
                     if (sum(xxx)>0) {
                         gmessage(msg="WARNING: Forbidden Character '-' in the Path or Filename. Please remove!" , title = "Working Dir",  icon = "warning")
                         return()
                     }
                     setwd(WorkingDir)
                     cat("\n New Working Directory: ", WorkingDir)
                     ShortPathName <- WorkingDir
                     if (nchar(ShortPathName) > 40){   #cut workingDir to less than 40 chars
                        splitPathName <- strsplit(ShortPathName, "/")
                        LL <- nchar(ShortPathName)
                        HeadPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="")
                        ShortPathName <- paste(HeadPathName, substr(ShortPathName, (LL-30), LL), sep="")
                     }
                     svalue(dispWD) <- ShortPathName
                   }, container=MainFrame)

   dispWD <- gedit(text="                                        ", initial.msg=WorkingDir, spacing=1, handler=function(h, ...){
                     WorkingDir <<- svalue(dispWD)    #if manually the workingDir has been changed
                     setwd(WorkingDir)
                     cat("\n New Working Directory: ", WorkingDir)
                   }, container=MainFrame)
   tkconfigure(dispWD$widget, width=45)

   glabel("  ", container=MainGroup)

   gbutton("SET AS DEFAULT and EXIT", handler=function(h, ...){
                     Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                     if (file.exists(Ini.pthName)) {
                         XPSSettings <<- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
                         XPSSettings$General[7] <<- WorkingDir  #personal Working Dir
                         ColNames <- names(XPSSettings)
                         write.table(XPSSettings, file = Ini.pthName, sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                         assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                         dispose(MainWin)
                     } else {
                         gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
                         dispose(MainWin)
                         return()
                     }
                   }, container=MainGroup)

   gbutton("SET and EXIT", handler=function(h, ...){
#--- get System info and apply correspondent XPS Settings ---
                     Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                     if (file.exists(Ini.pthName)) {
                         XPSSettings <<- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
                         XPSSettings$General[7] <<- WorkingDir  #personal Working Dir
                         assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                         dispose(MainWin)
                     } else {
                         gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
                         dispose(MainWin)
                         return()
                     }
                   }, container=MainGroup)


   visible(MainWin) <- TRUE
}
