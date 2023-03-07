#-----------------------------------------
# XPS Sprucing with gWidgets2 and tcltk
#-----------------------------------------
#'Sprucing XPSSample data
#'
#'GUI to correct original XPS spectral data of the active XPSSample 
#'
#'@return Returns the \code{Object} with corrected Survey spectrum.
#'
#'@examples
#'
#'\dontrun{
#'	XPSSprucingGUI()
#'}
#'
#'@export
#'

XPSSprucingGUI <- function() {

	  setGraphicsEventHandlers(prompt = " ")  #set the Event Environment where to save the handler of the graphic window
  	eventEnv <- getGraphicsEventEnv(which = dev.cur())

   my.coords <- function(x, y) {
		     return(c(grconvertX(x, "ndc", "user"), grconvertY(y, "ndc", "user")))
	  }



#MouseUp handler allows to directly rise the focus on X11() and calls mouseBTN
#In windows right button activates X11() options => there is NO control on buttons
#Only mouse coordinates are passed to mouseBtnUp(). Then only mouseBtnUp function
#will be called, all functions MUST be activated with left mouse button
   mouseBtnUp <- function(buttons, x, y) {
		    coords <<- my.coords(x, y)
     	point.coords$x[point.index] <<- coords[1]   #abscissa
     	point.coords$y[point.index] <<- coords[2]   #ordinate
     	if (point.index==1) {
     	   point.index<<-2    #to modify the second edge of the selected area
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	   } else if (point.index==2) {
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index<<-3
  	   } else if (point.index==3) {
         D<-vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
         for (ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
             if(D[ii] <= Dmin){
                Dmin<-D[ii]
                idx=ii
             }
         }
         if (idx==1){
            Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
            Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx==2){
             Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
             Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         } else if (idx==3){
             Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
             Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx==4){
            Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
            Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         }
         if (Object[[coreline]]@Flags[1]) { #Binding energy set
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #ordina pos$x in ordine crescente
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      draw.plot()
      tkfocus(force=SPwin$widget)
      Focus <<- as.character(tkfocus())
      return(1)
  }

   undo.plot <- function(...){
      if (SelReg == 1) {
         reset.boundaries()
     	   draw.plot()
      } else if (SelReg > 1) {
	        Object[[coreline]]@Boundaries$x <<- OldCoords$x
	        Ylimits <<- OldCoords$y
     	   draw.plot()
  	   }
   }

   draw.plot <- function(...) {
#      if (NO.Fit) { return() }
      Xlimits <- Object[[coreline]]@Boundaries$x
	     if (point.index <= 2) {
	    	    plot(Object[[coreline]], xlim=Xlimits)
          points(point.coords, col="red", cex=1, lwd=1.5, pch=3)
 	     } else if (point.index > 2){
	    	    plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
          points(Corners, type="p", col="red", cex=1, lwd=1.5, pch=3)
          rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
	     }
	     svalue(statbar) <- sprintf(paste("x =",round(coords[1],1), " y =",round(coords[2]), sep=" "))
   }


   reset.boundaries <- function(h, ...) {
	     Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
      LL<-length(Object[[coreline]]@.Data[[1]])
      point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
      point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
      point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
      point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
      slot(Object[[coreline]],"Boundaries") <<- point.coords
      Ylimits<<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <<- point.coords #for undo
      Corners <- point.coords
      point.index <<- 1
      draw.plot()
   }

   do.editRegion <- function(h, ...){
              idx1 <<- point.coords$x[1]
              idx2 <<- point.coords$x[2]
              newcoreline <- Object[[coreline]]
              idx1 <<- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #indice relativo al limite superiore (inferiore KE?) della RegionToFit
              idx2 <<- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #indice relativo al limite inferiore (superiore KE?) della RegionToFit
              tmp <- unlist(Object[[coreline]]@.Data[1]) #estraggo le ascisse regione selezionata
              newcoreline@.Data[[1]] <- tmp[idx1:idx2]     #aggiungo e ascisse regione selezionata
              tmp <- unlist(Object[[coreline]]@.Data[2]) #estraggo le ordinate regione selezionata
              newcoreline@.Data[[2]] <- tmp[idx1:idx2]     #aggiungo le ordinate regione selezionata
              DataTable <<- as.data.frame(cbind(newcoreline@.Data[[1]], newcoreline@.Data[[2]]))
              names(DataTable) <<- c("  X  ", "  Y  ")
              delete(EditGroup, DataToChange) #the pointer DataToChange is still alive. I add the pointer to a still unknown widget to EditGroup
              DataToChange <<- gdf(items=DataTable, container=EditGroup) #Now I define the widget associated to the pointer DataToChange
              size(DataToChange) <<- c(200,180)
              addHandlerChanged(DataToChange, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                                     DataTable <<- h$obj[]
                               })
              enabled(ButtGroup) <- TRUE
   }


#====== VARIABLES DEFINITION=======

  Object <- get(activeFName,envir=.GlobalEnv)
  SpectList <- XPSSpectList(activeFName)
  coreline <- get("activeSpectIndx",envir=.GlobalEnv)
  if (length(indx <- grep("survey",SpectList))==0){
      indx <- grep("Survey",SpectList)
  }
  if (length(indx)==0){
      msg <- paste("Sorry Core Line ", coreLine, " NOT present in ", activeFName, " XPSSample", sep="")
      tkmessageBox(message = msg, icon = "warning", type = "ok")
  }
  XPSSettings <- get("XPSSettings",envir=.GlobalEnv)
  WinSize <- as.numeric(XPSSettings$General[4])
  DataTable <- NULL
  idx1 <- NULL
  idx2 <- NULL

  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  xx <- NULL
  yy <- NULL
  NO.Fit<-FALSE
  Focus <- NULL
  WhileExit <- NULL

#Coreline boundaries
  LL <- length(Object[[coreline]]@.Data[[1]])
  point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]
  point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]
  point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL]
  point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL]
  Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
  OldCoords <- point.coords #for undo
  Corners <- point.coords
  SelReg <- 0

  Object[[coreline]]@Boundaries$x <- c(point.coords$x)
  Object[[coreline]]@Boundaries$y <- c(point.coords$y)
  parusr <- NA
  parplt <- NA

#====== Widget Definition =======
  SPwin <- gwindow("XPS SPRUCING GUI", parent=c(100,-10), toolkit = "tcltk", visible = FALSE)
  addHandlerDestroy(SPwin, handler=function(h, ...){
               WhileExit <<- -1    #stops the while loop if MainWindow unproperly closed with X
               setGraphicsEventHandlers(onMouseMove = 1) #blocks the mouseHandler
               setGraphicsEventHandlers(onMouseUp = 1) #blocks the mouseHandler
               setGraphicsEventHandlers(onMouseDown = 1) #blocks the mouseHandler
         })

  size(SPwin) <- c(250, 600)
  MainGroup <- ggroup(horizontal = FALSE, expand=TRUE, container = SPwin)

#====== OPTIONS SECTION =======

  gframe10 <- gframe(text="SELECT CORELINE", expand=TRUE, spacing=3, container=MainGroup)
  CLobj10 <- gcombobox(SpectList, selected=coreline, editable=FALSE, handler=function(h,...){
              XPSCL <- svalue(CLobj10)
              XPSCL <- unlist(strsplit(XPSCL, "\\."))   #remove number at CoreLine beginning
              coreline <<- as.numeric(XPSCL[1])
              #Coreline boundaries
              LL <- length(Object[[coreline]]@.Data[[1]])
              point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
              point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
              point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
              point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
              Ylimits <<- c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
              OldCoords <<- point.coords #for undo
              Corners <<- point.coords
              SelReg <<- 0
              Object[[coreline]]@Boundaries$x <<- c(point.coords$x)
              Object[[coreline]]@Boundaries$y <<- c(point.coords$y)
              parusr <<- NA
              parplt <<- NA
              draw.plot()
         }, container=gframe10)

  gframe22 <- gframe(text = " SELECT DATA ", spacing=3, horizontal = FALSE, container = MainGroup)

  gbutton(" SELECT REGION ", container = gframe22, handler = function(h, ...){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              rngX <- range(point.coords$x)
              rngX <- (rngX[2]-rngX[1])/20
              rngY <- range(point.coords$y)
              rngY <- (rngY[2]-rngY[1])/20

              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x<-sort(point.coords$x, decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]+rngX/20
                 point.coords$x[2] <- point.coords$x[2]-rngX/20
              } else {
                 point.coords$x<-sort(point.coords$x, decreasing=FALSE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]-rngX/20
                 point.coords$x[2] <- point.coords$x[2]+rngX/20
              }
              point.coords$y <- sort(point.coords$y, decreasing=FALSE)
              Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
 	            slot(Object[[coreline]],"Boundaries") <<- point.coords
              draw.plot()
         } )

  gbutton(" EDIT REGION ", handler = function(h, ...){
              do.editRegion()
  	      }, container = gframe22 )

  gbutton(" UNDO ", handler = function(h, ...) {
  	           undo.plot()
  	      }, container = gframe22 )

  gbutton(" RESET BOUNDARIES ", handler = function(h, ...) {
  	           reset.boundaries()
  	      }, container = gframe22 )

  gframe23 <- gframe(text = " Sprucing ", horizontal=TRUE, container = MainGroup)

  EditGroup <- ggroup(container=gframe23)
  DataToChange <- gtext(" Data to correct:  ", container=EditGroup)
  size(DataToChange) <- c(200,180)

  ButtGroup <- ggroup(horizontal=FALSE, container=gframe23)
  buttOK<-gbutton("OK", handler=function(h, ...){
              Object[[coreline]]@.Data[[1]][idx1:idx2] <<- DataTable[[1]]
              Object[[coreline]]@.Data[[2]][idx1:idx2] <<- DataTable[[2]]
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              plot(Object[[coreline]])
            }, container=ButtGroup)
  buttCanc<-gbutton("Cancel", handler=function(...) {
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              reset.boundaries()
            }, container=ButtGroup)
  enabled(ButtGroup) <- FALSE

#=== CLOSE button ===
  gbutton("Save", handler = function(h, ...){
	             assign(activeFName, Object, envir = .GlobalEnv)
  	           reset.boundaries()
  	           XPSSaveRetrieveBkp("save")
         }, container = MainGroup)


  gbutton("Save & Close", handler = function(h, ...) {
	             assign(activeFName, Object, envir = .GlobalEnv)
              setGraphicsEventHandlers(which = dev.cur(), onMouseUP = 1)  #stops the Graphics.Event.Handler
              WhileExit <- -1
              dispose(SPwin)
  	           reset.boundaries()
              XPSSaveRetrieveBkp("save")
         }, container = MainGroup)

  gbutton("Exit", handler = function(h, ...) {
              setGraphicsEventHandlers(which = dev.cur(), onMouseUP = 1)  #stops the Graphics.Event.Handler
              WhileExit <- -1
              dispose(SPwin)
   	          reset.boundaries()
              XPSSaveRetrieveBkp("save")
         }, container = MainGroup)

  ## status bar
  statbar <- gstatusbar("status", container = SPwin)
  visible(SPwin) <- TRUE


  tkfocus(force=SPwin$widget) #force the focus on the widget
#----- Markers at Coreline extremes -----
  draw.plot()
#----- Set interrupts on graphic window -----
    WhileExit <- 1
    while(WhileExit > 0){
         if (as.character(tkwinfo("exists", SPwin$widget)) == "1"){ #widget not improperly closed with X
             Focus <- tkfocus()
             Focus <- as.character(Focus)  #length(Focus) > 0 only when MainWindow has focus
         }
         if(length(Focus) == 0){  #focus is on the graphic window
             eventEnv <- getGraphicsEvent(prompt = "",  #get the interrupt when the mouse buttons are pressed
              		                        onMouseUp = mouseBtnUp)       #on mouse button pressed whichButton() function called
         }
    }

}
