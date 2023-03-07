#--------------------------------------
# XPS Extract portion of survey spectra
#--------------------------------------
#'Extract portions of spectrum from an XPS survey
#'
#'Extracts a portion of XPS survey of the active XPSSample. It uses a GUI
#'to define the survey portion to extract and to provide the name
#'of the new XPSCoreLine
#'
#'@return returns the \code{object} with a coreline added.
#'@examples
#'
#'\dontrun{
#'	XPSExtract()
#'}
#'
#'@export
#'

XPSExtract <- function() {

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
	 	   coords  <<- my.coords(x, y)
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
          Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #initialization value
          for (ii in 1:4) {
               D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #distance P0 - P1
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
              point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)
              point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
          } else {
              point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
              point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
          }
      }
      draw.plot()
      tkfocus(force=Ewindow$widget)
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
       point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1]  #abscissa of the first survey edge
       point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1]  #ordinate of the first survey edge
       point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #abscissa of the second survey edge
       point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinate of the second survey edge
       slot(Object[[coreline]],"Boundaries") <<- point.coords
       Ylimits<<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
       OldCoords <<- point.coords #for undo
       Corners <- point.coords
       point.index <<- 1
       draw.plot()
   }

   do.extract <- function(h, ...){
      winExt <- gwindow("Extract Spectral Feature", visible=FALSE, parent=window) #
	    	gBox <- gvbox(container=winExt)
	    	flyt <- gformlayout(container=gBox)
	    	elesymbol <- gedit("", label="Element Name:", container=flyt)
	    	gseparator(container=gBox)
	    	bg <- ggroup(container=gBox)
	    	gbutton("OK", container=bg, handler=function(...){
		            Symbol<-svalue(elesymbol)
		            Symbol<- gsub(" ", "", Symbol)    #eliminates white spaces from Symbol
#              idx<-regexpr(" ",   Symbol)      #char matching: -1 if " " not present otherwise position of "" in Symbol
       	      pattern <- c("[[:alpha:]]{1,2}")  #matches only the first two char
		            mpat <- regexpr(pattern, Symbol)
	             ## symbol element
		            Element <- regmatches(Symbol, mpat)
	             if (ElementCheck(Element)==FALSE ) {    #see XPSelement.r
                  yesno<-gconfirm(msg="ATTENTION: element Name NOT correct! Proceed anyway?", icon="warning")
                  if (yesno==FALSE){
                     dispose(winExt)
                     return()
                  }
              }
              dispose(winExt)

              newcoreline<-Object[[coreline]]
              Xmax<-max(range(newcoreline@.Data[1]))
              Xmin<-min(range(newcoreline@.Data[1]))
              if (point.coords$x[1] > Xmax) {point.coords$x[1]<-Xmax}
              if (point.coords$x[1] < Xmin) {point.coords$x[1]<-Xmin}
              if (point.coords$x[2] > Xmax) {point.coords$x[2]<-Xmax}
              if (point.coords$x[2] < Xmin) {point.coords$x[2]<-Xmin}
              idx1<-findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #index corresponding to the selected BE1 (or KE1 value) of RegionToFit
              idx2<-findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #index corresponding to the selected BE2 (or KE2 value) of RegionToFit
              tmp <- unlist(Object[[coreline]]@.Data[1])  #extract correspondent X values for the selected region
              newcoreline@.Data[[1]] <- tmp[idx1:idx2]    #save the X values in the new coreline
              newcoreline@Boundaries$x<-c(tmp[idx1], tmp[idx2])
              tmp <- unlist(Object[[coreline]]@.Data[2])  #extract correspondent Y values for the selected region
              newcoreline@.Data[[2]] <- tmp[idx1:idx2]    #save the Y values in the new coreline
              newcoreline@Boundaries$y<-c(tmp[idx1], tmp[idx2])
              tmp <- unlist(Object[[coreline]]@.Data[3])  #extract correspondent transmission Factor values for the selected region
              newcoreline@.Data[[3]] <- tmp[idx1:idx2]    #save the transmission Factor values in the new coreline
              slot(newcoreline,"Symbol") <- Symbol
              ## add extracted coreline to original XPSSample
		            idx <- length(Object) + 1
		            Object[[idx]] <<- newcoreline
		            names(Object) <<- unname(sapply(Object, slot, "Symbol"))
              draw.plot()
		            svalue(statbar) <- sprintf("New %s added.", svalue(elesymbol))
      })
		    gbutton("Cancel", container=bg, handler=function(...) dispose(winExt))
		    visible(winExt) <- TRUE
   }


#----- Variables -----
  Object <- get(activeFName,envir=.GlobalEnv)
  SpectList <- XPSSpectList(activeFName)
  coreline <- get("activeSpectIndx",envir=.GlobalEnv)
  FNameList<-XPSFNameList()

  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  xx<-NULL
  yy<-NULL
  NO.Fit<-FALSE
  Focus <- NULL
  WhileExit <- NULL

#Coreline boundaries
  LL<-length(Object[[coreline]]@.Data[[1]])
  point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]
  point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]
  point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL]
  point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL]
  Xlimits<-c(min(Object[[coreline]]@.Data[[1]]), max(Object[[coreline]]@.Data[[1]]))
  Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
  Corners <- point.coords
  Object[[coreline]]@Boundaries$x <- c(point.coords$x)
  Object[[coreline]]@Boundaries$y <- c(point.coords$y)

  OldCoords <- point.coords #for undo
  OldEnergyScale<-Object[[coreline]]@.Data[[1]]
  OldFlag<-Object[[coreline]]@Flags[1]
  OldUnits<-Object[[coreline]]@units[1]
  SelReg <- 0


#====== Widget definition =======
  Ewindow <- gwindow("XPS extract GUI", visible = FALSE)
  addHandlerDestroy(Ewindow, handler=function(h, ...){
                         WhileExit <<- -1    #stops the while loop if MainWindow unproperly closed with X
                         setGraphicsEventHandlers(onMouseMove = 1) #blocks the mouseHandler
                         setGraphicsEventHandlers(onMouseUp = 1) #blocks the mouseHandler
                         setGraphicsEventHandlers(onMouseDown = 1) #blocks the mouseHandler
                      })


  Egroup1 <- ggroup(container = Ewindow, horizontal = TRUE)

  ## XPSSample and Core lines
  Egroup2 <- ggroup(expand = FALSE, horizontal = FALSE, spacing = 5, container = Egroup1)

  gframe20 <- gframe(text = " XPS Sample and Core line Selection ", container = Egroup2)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName<-svalue(XPS.Sample)
                                 Object<<-get(activeFName, envir=.GlobalEnv)
                                 SpectList<<-XPSSpectList(activeFName)
                                 delete(gframe20, Core.Lines)
                                 Core.Lines <<- gcombobox(SpectList, selected=1, expand = FALSE, handler = function(h, ...){
                                                CLine <- svalue(Core.Lines)
                                                CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
                                                if (CLine[2] != "survey" && CLine[2] != "Survey"){
                                                   gmessage("Wrong coreline: please select a survey spectrum", title="WRONG CORELINE", icon="warning")
                                                   return()
                                                }
                                                coreline <<- as.integer(CLine[1])   # Coreline index
                                                LL<-length(Object[[coreline]]@.Data[[1]])
                                                point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]
                                                point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]
                                                point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL]
                                                point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL]
                                                Xlimits<-c(min(Object[[coreline]]@.Data[[1]]), max(Object[[coreline]]@.Data[[1]]))
                                                Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
                                                Corners <- point.coords
                                                Object[[coreline]]@Boundaries$x <- c(point.coords$x)
                                                Object[[coreline]]@Boundaries$y <- c(point.coords$y)
                                                OldCoords <- point.coords #for undo
                                                OldEnergyScale<-Object[[coreline]]@.Data[[1]]
                                                OldFlag<-Object[[coreline]]@Flags[1]
                                                OldUnits<-Object[[coreline]]@units[1]
                                                SelReg <- 0
                                 }, container = gframe20)
                                 draw.plot()
                       }, container = gframe20)
  svalue(XPS.Sample)<-activeFName

  Core.Lines <- gcombobox(SpectList, selected=-1, expand = FALSE, handler = function(h, ...){
              CLine <- svalue(Core.Lines)
              CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
              if (CLine[2] != "survey" && CLine[2] != "Survey"){
                 gmessage("Wrong coreline: please select a survey spectrum", title="WRONG CORELINE", icon="warning")
                 return()
              }
              coreline <<- as.integer(CLine[1])   # Coreline index
              LL<-length(Object[[coreline]]@.Data[[1]])
              point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]
              point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]
              point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL]
              point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL]
              Xlimits<-c(min(Object[[coreline]]@.Data[[1]]), max(Object[[coreline]]@.Data[[1]]))
              Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
              Corners <- point.coords
              Object[[coreline]]@Boundaries$x <- c(point.coords$x)
              Object[[coreline]]@Boundaries$y <- c(point.coords$y)
              OldCoords <- point.coords #for undo
              OldEnergyScale<-Object[[coreline]]@.Data[[1]]
              OldFlag<-Object[[coreline]]@Flags[1]
              OldUnits<-Object[[coreline]]@units[1]
              SelReg <- 0
              draw.plot()
         }, container = gframe20)

  gframe22 <- gframe(text = " Options ", horizontal = FALSE, container = Egroup2)
  gbutton("SELECT REGION", handler = function(h, ...){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              rngX<-range(point.coords$x)
              rngX<-(rngX[2]-rngX[1])/20
              rngY<-range(point.coords$y)
              rngY<-(rngY[2]-rngY[1])/20

              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x<-sort(point.coords$x, decreasing=TRUE)  #pos$x in decreasing order
                 point.coords$x[1]<-point.coords$x[1]+rngX/20
                 point.coords$x[2]<-point.coords$x[2]-rngX/20
              } else {
                 point.coords$x<-sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                 point.coords$x[1]<-point.coords$x[1]-rngX/20
                 point.coords$x[2]<-point.coords$x[2]+rngX/20
              }
              point.coords$y<-sort(point.coords$y, decreasing=FALSE)
              Ylimits<<-c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
	           slot(Object[[coreline]],"Boundaries") <<- point.coords
              draw.plot()
         }, container = gframe22 )

  gbutton("EXTRACT REGION", handler = function(h, ...){
              do.extract()
  	      }, container = gframe22 )

  gbutton("UNDO", handler = function(h, ...) {
  	           undo.plot()
  	      }, container = gframe22 )

  gbutton("RESET BOUNDARIES", handler = function(h, ...) {
              Object[[coreline]]@.Data[[1]]<<-OldEnergyScale
              Object[[coreline]]@Flags[1]<<-OldFlag
  	           reset.boundaries()
  	      }, container = gframe22 )

  gframe23 <- gframe(text = " Plot ", container = Egroup2)
  SwitchE <- gcheckbox(" SWITCH BINDING/KINETIC ENERGY SCALE", checked=FALSE, handler = function(h, ...) {
                     XEnergy<-get("XPSSettings", envir=.GlobalEnv)$General[5] #the fifth element of the first column of XPSSettings
                     XEnergy<-as.numeric(XEnergy)
                     if (svalue(SwitchE)==TRUE){
                        idx1<-findXIndex(unlist(Object[[coreline]]@.Data[[1]]), point.coords$x[1]) #index relative to the upper limit (lower if KE scale) of the X scale
                        idx2<-findXIndex(unlist(Object[[coreline]]@.Data[[1]]), point.coords$x[2]) #index relative to the lower limit (upper if KE scale) of the X scale
                        Object[[coreline]]@.Data[[1]]<<-XEnergy-OldEnergyScale
                        Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][idx1],Object[[coreline]]@.Data[[1]][idx2])
                        point.coords$x<<-XEnergy-point.coords$x
                        Corners$x<<-XEnergy-Corners$x
                        if (Object[[coreline]]@Flags[1]==TRUE) { #original scaale is BE
                           Object[[coreline]]@Flags[1]<<-FALSE   #set KE scale
                           Object[[coreline]]@units[1]<<-"Kinetic Energy [eV]"
                        } else if (Object[[coreline]]@Flags[1]==FALSE) { #original scaale is KE
                           Object[[coreline]]@Flags[1]<<-TRUE   #set BE scale
                           Object[[coreline]]@units[1]<<-"Binding Energy [eV]"
                        }
                     }
                     if (svalue(SwitchE)==FALSE){
                        idx1<-findXIndex(unlist(Object[[coreline]]@.Data[[1]]), point.coords$x[1]) #index relative to the upper limit (lower if KE scale) of the X scale
                        idx2<-findXIndex(unlist(Object[[coreline]]@.Data[[1]]), point.coords$x[2]) #index relative to the lower limit (upper if KE scale) of the X scale
                        Object[[coreline]]@.Data[[1]]<<-OldEnergyScale
                        Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][idx1],Object[[coreline]]@.Data[[1]][idx2])
                        point.coords$x<<-XEnergy-point.coords$x
                        Corners$x<<-XEnergy-Corners$x
                        Object[[coreline]]@Flags[1]<<-OldFlag
                        Object[[coreline]]@units[1]<<-OldUnits
                     }
                     draw.plot()
         }, container=gframe23 )

  ## CLOSE button
  gseparator(container = Egroup2) # separator
  gframe24 <- gframe(text = " Help ", container = Egroup2)
  glabel(" Set the region edges with the cursors", container=gframe24)

  addSpring(Egroup2)
  gseparator(container = Egroup2)

  gbutton("SAVE", expand=FALSE, handler = function(h, ...){
              Object[[coreline]]@.Data[[1]]<<-OldEnergyScale
              Object[[coreline]]@Flags[1]<<-OldFlag
              Object[[coreline]]@units[1]<<-OldUnits
              assign(activeFName, Object, envir = .GlobalEnv)
              reset.boundaries()
              plot(Object)
              XPSSaveRetrieveBkp("save")
         }, container = Egroup2 )


  gbutton("SAVE & EXIT", expand=FALSE, handler = function(h, ...){
              Object[[coreline]]@.Data[[1]]<<-OldEnergyScale
              Object[[coreline]]@Flags[1]<<-OldFlag
              Object[[coreline]]@units[1]<<-OldUnits
              assign(activeFName, Object, envir = .GlobalEnv)
              reset.boundaries()
              setGraphicsEventHandlers(which = dev.cur(), onMouseUP = 1)  #stops the Graphics.Event.Handler
              WhileExit <- -1
              dispose(Ewindow)
              plot(Object)
              XPSSaveRetrieveBkp("save")
         }, container = Egroup2 )

  gbutton("EXIT", expand=FALSE, handler = function(h, ...){
              WhileExit <- -1
              setGraphicsEventHandlers(which = dev.cur(), onMouseUP = 1)  #stops the Graphics.Event.Handler
              dispose(Ewindow)
              plot(Object)
              XPSSaveRetrieveBkp("save")
         }, container = Egroup2 )

  ## status bar
  statbar <- gstatusbar("status", container = Ewindow)


  visible(Ewindow) <- TRUE
  tkfocus(force=Ewindow$widget) #force the focus on the widget

#----- Markers at Coreline extremes -----
  draw.plot()

#----- Set interrupts on graphic window -----
    WhileExit <- 1
    while(WhileExit > 0){
         if (as.character(tkwinfo("exists", Ewindow$widget)) == "1"){ #widget not improperly closed with X
             Focus <- tkfocus()
             Focus <- as.character(Focus)  #length(Focus) > 0 only when MainWindow has focus
         }
         if(length(Focus) == 0){  #focus is on the graphic window
             eventEnv <- getGraphicsEvent(prompt = "",  #get the interrupt when the mouse buttons are pressed
              		                        onMouseUp = mouseBtnUp)       #on mouse button pressed whichButton() function called
         }
    }
}
