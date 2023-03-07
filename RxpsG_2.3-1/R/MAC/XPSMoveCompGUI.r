#   Manual Move Components using handlers applied to the graphic window
#   FUNCTION CALL:  XPSMoveCompGUI(c2.pxt,CoreLine=2)

#'Function to modify component position and intensity in a fit
#'
#'Provides a userfriendly interface change position and intensity of each
#'individual fitting component of a selected XPSCoreline. Changes are saved in the
#'.GlobalEnv main software memory
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSMoveComp()
#'}
#'
#'@export
#'


XPSMoveComp <- function() {


   my.coords <- function(xx, yy){     #giving cursor position in pixels here compute cursor coordinates
      xx <- grconvertX(xx, "ndc", "user")
      yy <- grconvertY(yy, "ndc", "user")

      Xlim1 <- min(Object@RegionToFit[[1]])   #control over the possible X, Y range
      Xlim2 <- max(Object@RegionToFit[[1]])
      Ylim1 <- min(Object@RegionToFit[[2]])
      Ylim2 <- max(Object@RegionToFit[[2]])
      if (xx < Xlim1 ) {xx <- Xlim1}
      if (xx > Xlim2 ) {xx <- Xlim2}
      if (yy < Ylim1 ) {yy <- Ylim1}
      if (yy > Ylim2 ) {yy <- Ylim2}

      Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
      yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
      coords <- c(xx, yy, yy_BasLin)
      return(coords)
   }
   
#MouseUp handler allows to directly rise the focus on X11() and calls mouseBTN
#In windows right button activates X11() options => there is NO control on buttons
#Only mouse coordinates are passed to mouseBtnUp(). Then only mouseBtnUp function
#will be called, all functions MUST be activated with left mouse button
   mouseBtnUp <- function(buttons, x, y) {
     coords  <<-  my.coords(x, y)
	    yy <- coords[2]
	    xx <- coords[1]
	    
     if (Object@Flags[1]) { #Binding energy set
        if (xx > parusr[1] || xx < parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() } #if you click ouside XY box of the plot do noting
     } else {
        Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
        if (xx < parusr[1] || xx > parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() }
     }

     if (SetZoom==FALSE) { #left button works only when SET ZOOM REGION inactive
         do.move()
         XPSquantify(XPSSample)
         refresh <<- FALSE
     } else if (SetZoom == TRUE) { #selection of the zooming area
      	  point.coords$x[point.index] <<- coords[1]   #abscissa
      	  point.coords$y[point.index] <<- coords[2]   #ordinate
      	  if (point.index == 1) {
             point.index <<- 2    #to modify the second edge of the selected area
             Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
             Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         } else if (point.index==2) {
             Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
             Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
             point.index <<- 3
         } else if (point.index==3) {
             D<-vector("numeric", 4)
             Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
             for (ii in 1:4) {
                 D[ii] <- ((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
                 if(D[ii] <= Dmin){
                    Dmin <- D[ii]
                    idx <- ii
                 }
             }
             if (idx == 1){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx == 2){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             } else if (idx == 3){
                Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx == 4){
                Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
                Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             }
             if (Object@Flags[1]) { #Binding energy set
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             } else {
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             }
         }
     }
     draw.plot()
     tkfocus(force=MCWindow$widget)
     Focus <<- as.character(tkfocus())
     return(1)
  }


  StopHandler <- function(buttons, x, y){
     setGraphicsEventHandlers(onMouseUp = 1)
     setGraphicsEventHandlers(onMouseDown = 1)
     cat("\n OK thanks.")
     return(1)
  }


  LoadCoreLine<-function(){
      XPSSample <<- get(activeFName, envir=.GlobalEnv)     #load the XPSSample data from main memory
      SpectName <<- get("activeSpectName", envir=.GlobalEnv)    #get active Spectrum index
      SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
      Object <<- XPSSample[[SpectIndx]]
      Xlimits <<- range(Object@RegionToFit$x)
      Ylimits <<- range(Object@RegionToFit$y)
      ComponentList <<- names(slot(Object,"Components"))
      if (length(ComponentList)==0) {
          gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
          return()
      }
      delete(MCFrame2, FitComp)    #selecting a new core line needs a new gradio/checkbox

      if (length(ComponentList) > 1){    #gradio works with at least 2 items Less than 2 items gcheckbox will be used
          FitComp <<- gradio(ComponentList, selected=1, handler = function(h,...){   #gradio handler has to be redefined
#                   refresh <<- TRUE
#                   draw.plot()   #replot spectrum without marker
                   FComp <- svalue(FitComp)
                   FComp <- as.numeric(unlist(strsplit(FComp, split="C")))[2]  #index of the selected component
                   xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   FuncName <- Object@Components[[FComp]]@funcName
                   yy <- Object@Components[[FComp]]@param[1,1] #component height h
                   yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                   Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                   yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh <<- FALSE #now plot also the component marker
                   draw.plot()   #replot all
               },  container = MCFrame3)

          LL <- length(ComponentList)
          NCol <- ceiling(LL/5)   #gradio will be split in solumns of 5 elements
          for(ii in 1:LL){
              tkpack.forget(FitComp$widgets[[ii]])  # unparent widgets (uses library call)
          }
          for(kk in 1:NCol){
              NN <- (kk-1)*5
              for (ii in 1:5) {
                   if((ii+NN) > LL) {break}
                   FitComplyt[ii,kk] <<- FitComp$widgets[[(ii+NN)]]
              }
          }
      } else {
          FitComp <<- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){ #gradio handler has to be redefined
                   refresh<<-TRUE
                   draw.plot()   #replot spectrum without marker
                   FComp <- svalue(FitComp)
                   FComp <- as.numeric(unlist(strsplit(FComp, split="C")))[2]
                   xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   FuncName <- Object@Components[[FComp]]@funcName
                   yy <- Object@Components[[FComp]]@param[1,1] #component height h
                   yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                   Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                   yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh <<- FALSE #now plot also the component marker
                   draw.plot()   #plot all
              },  container = MCFrame3)
      }
      xx <- Object@Components[[1]]@param[2,1] #component position mu
      yy <- Object@Components[[1]]@param[1,1] #component height h
      Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
      yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
      coords[1] <<- xx
      coords[2] <<- yy
      refresh <<- FALSE #now plot the component marker
      draw.plot()   #replot spectrum and marker of selected fit component
  }

  do.move <- function(...) {
     FComp <- svalue(FitComp)
     if (length(FComp)==0) {
         gmessage(msg="Select Component Please", title="WARNING", icon = "warning")
     } else {
        FComp <- as.numeric(unlist(strsplit(FComp, split="C")))[2]   #index of the selected component
        xx <- coords[1]
        yy <- coords[2]  #Component max value with baseline
        zz <- coords[3]  #Component max value without baseline
        FitFunct <- Object@Components[[FComp]]@funcName
        newh <- GetHvalue(Object, FComp, FitFunct, zz)  #Get value computes the Component value given the fit parameters and the Ymax value

        #range limits for mu
        varmu <- getParam(Object@Components[[FComp]],variable="mu")
        minmu <- varmu$start-varmu$min
        maxmu <- varmu$max-varmu$start
        newmu <- c(xx, xx-minmu, xx+maxmu)
        #range limits for h
        varh <- getParam(Object@Components[[FComp]],variable="h")
        minh <- varh$start-varh$min
        if (is.infinite(varh$max)) {
            maxh <- varh$start*10
        } else {
            maxh <- varh$max-varh$start
        }

        if (maxh > 0) {
            newh <- c(newh, 0, newh*5)    # No constraints on h
        }
        if (maxh==0){
            newh <- c(newh, newh, newh)   # h is fixed
        }
        if (maxh<0){
            newh <- c(newh, 0, newh*5)    # maxh cannot be <0: => force newh to correct values
        }
        if (varh$start <0) {
            newh <- c(0, 0, 1e5)   #set a positive value for an hypotheic fit
        }
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="mu", value=newmu)
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="h", value=newh)
        Object@Components[[FComp]] <<- Ycomponent(Object@Components[[FComp]], x=Object@RegionToFit$x, y=Object@Baseline$y) #eomputes the Y value and add baseline
# Fit computed addind fit components with the modified ones
        tmp <- sapply(Object@Components, function(z) matrix(data=z@ycoor))
        Object@Fit$y <<- ( colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y))

        Object <<- sortComponents(Object)
#if component order changed then re-number them
        LL <- length(Object@Components) #N. fit components
        for (ii in 1:LL){
           if (xx == Object@Components[[ii]]@param["mu",1]) { #compare marker position with component positions
             idx <- ii
             break()
           }
        }
        svalue(FitComp) <- paste("C", idx, sep="")  #update component gradio
        XPSSample[[SpectIndx]] <<- Object
     }
  }


  draw.plot <- function(...) {
     if (point.index==1 && refresh==FALSE) {  #point.index==1 when moving mcomponent
        plot(Object, xlim=Xlimits, ylim=Ylimits)
        points(x=coords[1], y=coords[2], col=2, cex=1.2, lwd=2, pch=1)  # if refresh==FALSE plot spectrum with component marker
     } else if (SetZoom == TRUE){   #set zoom area corners
	       if (point.index <= 2) {  #define zoom area corners
 	         plot(Object)
           points(point.coords, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
  	     } else if (point.index > 2){  #plot zoom area corners
 	         plot(Object, xlim=Xlimits, ylim=Ylimits)
           points(Corners, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
           rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
        }
     }
     parusr <<- par('usr')
     svalue(StatusBar) <- sprintf(paste("x =",round(coords[1],1), " y =",round(coords[2]), sep=" "))
  }

  reset.plot <- function(h, ...) {
       point.coords$x <<- range(Object@RegionToFit$x) #set original X range
       point.coords$y <<- range(Object@RegionToFit$y) #set original Y range
       Object@Boundaries <<- point.coords
       Xlimits <<- point.coords$x
       Ylimits <<- sort(point.coords$y, decreasing=FALSE)
       Corners <<- point.coords
       parusr <<- par("usr")
       draw.plot()
  }

  refresh.plot <- function(...) {
     plot(Object)
  }

  reset.vars <- function(){
     XPSSample <<- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     SpectName <<- get("activeSpectName", envir=.GlobalEnv)
     SpectList <<- XPSSpectList(activeFName)
     SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
     Object <<- XPSSample[[SpectIndx]]
     ComponentList <<- names(slot(Object,"Components"))
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <<- range(Object@.Data[1])
        Ylimits <<- range(Object@.Data[2])
        NoFit <<- TRUE
        return()
     }

     FComp <<- svalue(FitComp)
     FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
     FComp <<- FComp[2]
     coords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     point.coords <<- list(x=NA, y=NA)
     xx <<- Object@Components[[FComp]]@param[2,1] #component position mu
     Rng <- range(Object@RegionToFit[[1]])
     if (xx < Rng[1]) {xx <- Rng[1]}
     if (xx > Rng[2]) {xx <- Rng[2]}

     yy <<- Object@Components[[FComp]]@param[1,1] #component height h
     Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
     Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
     yy <<- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx

     point.coords$x <<- range(Object@RegionToFit$x) #ordinata secondo estremo del survey
     point.coords$y <<- range(Object@RegionToFit$y) #ordinata secondo estremo del survey
     Xlimits <<- range(Object@RegionToFit$x)
     Ylimits <<- range(Object@RegionToFit$y)
     Object@Boundaries$x <<- Xlimits
     Object@Boundaries$y <<- Ylimits
     coords[1] <<- xx
     coords[2] <<- yy
     Corners <<- point.coords
     point.index <<- 1

     refresh <<- TRUE
     SetZoom <<- FALSE
     NoFit <<- FALSE
  }



# --- Variables ---
     XPSSample <- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     SpectName <- get("activeSpectName", envir=.GlobalEnv)
     SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
     Object <- XPSSample[[SpectIndx]]
     ComponentList <-names(slot(Object,"Components"))
     FNameList <- XPSFNameList()
     SpectList <- XPSSpectList(activeFName)
     FComp <- NULL

     coords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     point.coords <- list(x=NA, y=NA)
     xx <- NULL
     yy <- NULL
     Corners <- point.coords
     point.index <- 1
     parusr <- NULL #user graphic parameters

     refresh <- TRUE
     SetZoom <- FALSE
     NoFit <- FALSE
     Focus <- NULL
     WhileExit <- NULL

#Coreline boundaries
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <- range(Object@.Data[1])
        Ylimits <- range(Object@.Data[2])
        NoFit <- TRUE
     } else {
        LL<-length(Object@.Data[[1]])
        point.coords$x <- range(Object@RegionToFit$x) #set the X window extension == x range
        point.coords$y <- range(Object@RegionToFit$y) #set the Y window extension == y range
        Xlimits <- range(Object@RegionToFit$x)
        Ylimits <- range(Object@RegionToFit$y)
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
     }


#--- Widget definition ---
     MCWindow <- gwindow("XPS MOVE COMPONENT", visible = FALSE)
     addHandlerDestroy(MCWindow, handler=function(h, ...){
                         WhileExit <<- -1    #stops the while loop if MainWindow unproperly closed with X
                         setGraphicsEventHandlers(onMouseMove = 1) #blocks the mouseHandler
                         setGraphicsEventHandlers(onMouseUp = 1) #blocks the mouseHandler
                         setGraphicsEventHandlers(onMouseDown = 1) #blocks the mouseHandler
                      })
     visible(MCWindow) <- TRUE

     MCGroup <- ggroup(spacing = 5, container=MCWindow)

#--- Selection Group ---
     SelectGroup1 <- ggroup(horizontal=FALSE, spacing = 5, container=MCGroup)

     MCFrame1 <- gframe(text = " XPS SAMPLE ", container = SelectGroup1)
     XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                               ActiveFName <- svalue(XPS.Sample)
                               assign("activeFName", ActiveFName, envir=.GlobalEnv)
                               reset.vars()
                               if (NoFit == TRUE) { return() }
                               delete(MCFrame2, CLobj)
                               CoreLinelyt <<- glayout(spacing=1, container=MCFrame2)
                               SpectList <<- XPSSpectList(ActiveFName)
                               Indx <- grep(SpectName, SpectList)    #set the CoreLine Indx
                               CLobj <<- gradio(SpectList, selected=Indx, horizontal=FALSE, handler=function(h,...){
                                                    XPS.CL <- svalue(CLobj)
                                                    XPS.CL <- unlist(strsplit(XPS.CL, "\\."))   #drop "NUMber." in component name
                                                    Indx <- as.integer(XPS.CL[1])
                                                    SpectName <- XPS.CL[2]
                                                    assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                                                    assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                                                    LoadCoreLine()
                                                }, container = MCFrame2)
                               LL <- length(SpectList)
                               NCol <- ceiling(LL/5)   #gradio will be split in Ncol columns of 5 elements
                               for(ii in 1:LL){
                                   tkpack.forget(CLobj$widgets[[ii]])  # unparent widgets (uses library call)
                               }
                               for(kk in 1:NCol){
                                   NN <- (kk-1)*5
                                   for (ii in 1:5) {
                                        if((ii+NN) > LL) {break}
                                        CoreLinelyt[ii,kk] <<- CLobj$widgets[[(ii+NN)]]
                                   }
                               }
                               LoadCoreLine()
                               refresh <<- FALSE #now plot also the component marker
                               draw.plot()   
                           }, container = MCFrame1)
     svalue(XPS.Sample) <- activeFName

     SelectGroup2 <- ggroup(horizontal=TRUE, spacing = 5, container=SelectGroup1)
     MCFrame2 <- gframe(text = "CORELINES", horizontal=TRUE, spacing=10, container = SelectGroup2)
     CoreLinelyt <- glayout(spacing=1, container=MCFrame2)
     if (length(SpectList) > 1){    #gradio works with at least 2 items Less than 2 items gcheckbox will be used
         CLobj <- gradio(SpectList, selected=-1, horizontal=FALSE, handler=function(h,...){
                               XPS.CL <- svalue(CLobj)
                               XPS.CL <- unlist(strsplit(XPS.CL, "\\."))   #drop "NUMber." in component name
                               Indx <- as.integer(XPS.CL[1])
                               SpectName <- XPS.CL[2]
                               assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                               assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                               LoadCoreLine()
                           }, container = MCFrame2)
              LL <- length(SpectList)
              NCol <- ceiling(LL/5)   #gradio will be split in Ncol columns of 5 elements
              for(ii in 1:LL){
                  tkpack.forget(CLobj$widgets[[ii]])  # unparent widgets (uses library call)
              }
              for(kk in 1:NCol){
                  NN <- (kk-1)*5
                  for (ii in 1:5) {
                       if((ii+NN) > LL) {break}
                       CoreLinelyt[ii,kk] <- CLobj$widgets[[(ii+NN)]]
                  }
              }
     } else {
         CLobj <- gcombobox(SpectList, selected=-1, horizontal=FALSE, handler=function(h,...){
                               XPS.CL <- svalue(CLobj)
                               XPS.CL <- unlist(strsplit(XPS.CL, "\\."))   #drop "NUMber." in component name
                               Indx <- as.integer(XPS.CL[1])
                               SpectName <- XPS.CL[2]
                               assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                               assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                               LoadCoreLine()
                           }, container = MCFrame2)
     }

     MCFrame3 <- gframe(text = "FIT COMPONENTS", horizontal=TRUE, spacing=10, container = SelectGroup2)
     FitComplyt <- glayout(spacing=1, container=MCFrame3)
     if (length(ComponentList) > 1){    #gradio works with at least 2 items
         #a one column gradio is created to select a single fit component
         FitComp <- gradio(ComponentList, horizontal=FALSE, selected=1, handler = function(h,...){
                               refresh <<- TRUE    #cancel previous selections
                               draw.plot()   #plot spectrum without marker
                               FComp <- svalue(FitComp)
                               FComp <- as.numeric(unlist(strsplit(FComp, split="C")))[2]   #index selected component
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               FuncName <- Object@Components[[FComp]]@funcName
                               yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh <<- FALSE #now plot also the component marker
                               draw.plot()   #replot spectrum and marker
                           },  container = MCFrame3)
         #the gradio single column is now split in subcolumns of 5 elements each
                     LL <- length(ComponentList)
                     NCol <- ceiling(LL/5)   #gradio will be split in solumns of 5 elements
                     for(ii in 1:LL){
                         tkpack.forget(FitComp$widgets[[ii]])  # unparent widgets (uses library call)
                     }
                     for(kk in 1:NCol){
                         NN <- (kk-1)*5
                         for(ii in 1:5) {
                             if((ii+NN) > LL) {break}
                                 FitComplyt[ii,kk] <- FitComp$widgets[[(ii+NN)]]
                         }
                     }
     } else {
         FitComp <- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){
                               refresh <<- TRUE    #cancel previous component markers
                               draw.plot()   #plot spectrum only
                               FComp <- svalue(FitComp)
                               FComp <- as.numeric(unlist(strsplit(FComp, split="C")))[2]   #index selected compoent
                               FComp <- FComp[2]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh <<- FALSE #now plot spectrum + component marker
                               draw.plot()
                           },  container = MCFrame2)
     }

     MCFrame4 <- gframe(text = "OPTIONS", horizontal=FALSE, spacing=5, container = SelectGroup1)
     Buttlyt <- glayout(spacing=1, container=MCFrame4)
     Buttlyt[1,1] <- LBFitbutton <- gbutton("      FIT Lev.Marq.     ", handler=function(h,...){
                               FComp <- svalue(FitComp)
                               Object <<- XPSFitLM(Object)
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx #coords of marker of the first fit component
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh <<- FALSE  #now plot also the component marker
                               draw.plot(Object)
                           }, container = Buttlyt)

     Buttlyt[1,2] <- MFFitbutton<-gbutton("       FIT Modfit         ", handler=function(h,...){
                               FComp <- svalue(FitComp)
                               Object <<- XPSModFit(Object)
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh <<- FALSE #now plot also the component marker
                               draw.plot(Object)
                           }, container = Buttlyt)

     Buttlyt[2,1] <- ZRbutton <- gbutton("      SET ZOOM REGION      ", handler = function(h, ...){
                               CompCoords <<- coords   #save the of position component_marker
                               point.coords <<- NULL   #point.coords contain the X, Y data ranges
                               enabled(CLobj) <- FALSE
                               enabled(LBFitbutton) <- FALSE
                               enabled(MFFitbutton) <- FALSE
                               enabled(RSTbutton) <- FALSE
                               row1<-" => Left click to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                               row2<-"\n => Click near corners to adjust Zoom Region Dimensions"
                               row3<-"\n => When Zoom Region OK, press MAKE ZOOM"
                               msg<-paste(row1, row2, row3, sep="")
                               gmessage( msg, icon="warning")
                               SetZoom <<- TRUE
                           }, container = Buttlyt)

     Buttlyt[2,2] <- MZbutton<-gbutton("          MAKE ZOOM          ", handler = function(h, ...){
                               if (Object@Flags[1]) { #Binding energy set
                                  point.coords$x <- sort(point.coords$x, decreasing=TRUE)
                               } else {               #Kinetic energy set
                                  point.coords$x<-sort(point.coords$x, decreasing=FALSE)
                               }
                               Xlimits <<- point.coords$x
                               Ylimits <<- sort(point.coords$y, decreasing=FALSE)
 	                             slot(Object,"Boundaries") <<- point.coords
 	                             point.index <<- 1
 	                             coords <<- CompCoords #restore of position component_marker
 	                             refresh <<- FALSE
                               SetZoom <<- FALSE
                               draw.plot()
                               enabled(CLobj) <- TRUE
                               enabled(LBFitbutton) <- TRUE
                               enabled(MFFitbutton) <- TRUE
                               enabled(RSTbutton) <- TRUE
                           }, container = Buttlyt)

     Buttlyt[3,1] <- RSTbutton<-gbutton("         RESET PLOT        ", handler = function(h, ...) {
                               SetZoom <<- FALSE
                               refresh <<- FALSE
  	                            point.index <<- 1
  	                            reset.plot()
                           }, container = Buttlyt)

     
     Buttlyt[3,2] <- gbutton("        RE-LOAD DATA       ", handler=function(h,...){
                               LoadCoreLine()
                           }, container = Buttlyt)

     Buttlyt[4,1] <- gbutton("           SAVE            ", handler=function(h,...){
#    With button SAVE the Component parameters are updated and are now available for FiTConstraints
                               Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                               XPSSample[[Indx]] <<- Object
                               assign(activeFName, XPSSample, envir = .GlobalEnv)
                               plot(XPSSample[[Indx]])
                               XPSSaveRetrieveBkp("save")
                           }, container = Buttlyt)

     Buttlyt[4,2] <- gbutton("    EXIT      ", handler=function(h,...){
                               tcl("update", "idletasks")
                               setGraphicsEventHandlers(onMouseUp = 1)
                               setGraphicsEventHandlers(onMouseDown = 1)
                               WhileExit <<- -1
                               Focus <<- as.character(tkfocus())
                               XPSSaveRetrieveBkp("save")
                               dispose(MCWindow)
                           }, container = Buttlyt)

     StatusBar <- gstatusbar("status", container = MCWindow)

     enabled(CLobj) <- TRUE
     enabled(LBFitbutton) <- TRUE
     enabled(MFFitbutton) <- TRUE
     enabled(RSTbutton) <- TRUE

#--- Plot spectrum & components ---
     if (NoFit==FALSE){
        coords[1] <- Object@Components[[1]]@param[2,1] #component position mu
        coords[2] <- Object@Components[[1]]@param[1,1] #component1 height h
        FuncName <- Object@Components[[1]]@funcName
        coords[2] <- coords[2]/GetHvalue(Object,1, FuncName, 1)
        Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
        Xindx <- which(Object@RegionToFit[[1]]>coords[1]-Estep/2 & Object@RegionToFit[[1]]<coords[1]+Estep/2) #indice del vettore X corrispondente alla posizione della componente
        coords[2] <- coords[2]+Object@Baseline$y[Xindx]
        refresh <- FALSE
        draw.plot()
        refresh <- TRUE
     }

     tcl("update", "idletasks")

#cat("\n 1111")
#scan(n=1, quiet=TRUE)


     setGraphicsEventHandlers(prompt = " ")  #set the Event Environment where to save the handler of the graphic window
     eventEnv <- getGraphicsEventEnv(which = dev.cur())

#--- Set interrupts on graphic window -----
     WhileExit <- 1
     while(WhileExit > 0){  #widget not improperly closed
         if (as.character(tkwinfo("exists", MCWindow$widget)) == "1"){ #widget not improperly closed with X
             Focus <- tkfocus()
             Focus <- as.character(Focus)  #length(Focus) > 0 only when MainWindow has focus
         } else {
             setGraphicsEventHandlers(onMouseUp = 1) #blocks the mouseHandler
             setGraphicsEventHandlers(onMouseDown = 1) #blocks the mouseHandler
             break
         }
         LL <- length(Focus)
         if(LL == 0){  #focus is on the graphic window
            getGraphicsEvent(prompt = "Press Left Mouse Button",  #get the interrupt when the mouse buttons are pressed
                             onMouseDown = StopHandler,
              		             onMouseUp = mouseBtnUp) #on mouse button pressed whichButton() function called
         }
     }

}
