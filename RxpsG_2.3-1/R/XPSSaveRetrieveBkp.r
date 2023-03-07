XPSSaveRetrieveBkp<-function(opt){

#--- get System info and apply correspondent XPS Settings ---
   Bkp.pthName <- system.file("extdata/BkpData.Rdata", package="RxpsG")
   if (file.exists(Bkp.pthName) == FALSE) {
       gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check the RxpsG package", title = "WARNING",icon = "warning" )
       dispose(MainWin)
       return()
   }

   switch(opt,
       "save"={
          FNameList <- XPSFNameList() #read the list of XPSSample loaded in the .GlobalEnv
          save(list=FNameList, file=Bkp.pthName, compress=TRUE)   #save all the XPSSamples

#          txt<-paste("\n Bkp-Data saved in: ", SaveFile, sep="")
#          cat("\n", txt)
       },
       "retrieve"={
          FNameList <- load(Bkp.pthName,envir=.GlobalEnv)   #load the data in the .GlobalEnv not in the local memory
                                                          #assign not necessary
       } )

}
