instPckgs<-function(){

   cat("\n INSTALLING THE LIBRARIES NEEDED FOR RxpsG ")
   cat("\n\n The installation will take a few seconds")
   cat("\n\n If local personal library folder NOT present,\n answer YES to create the folder and start installation! ")
   cat("\n Please also check if your system allows the creation of the new folder")
   cat("\n\n\n")

   install.packages("baseline")
   install.packages("digest")
   install.packages("lattice")
   install.packages("latticeExtra")
   install.packages("limSolve")
   install.packages("memoise")
   install.packages("minpack.lm")
   install.packages("NORMT3")
   install.packages("rootSolve")
   install.packages("signal")
   install.packages("SparseM")
   install.packages("tkrplot")
   install.packages("wavelets")
   install.packages("FME")
   install.packages("gWidgets2")
   install.packages("gWidgets2tcltk")

   cat("\n INSTALLATION of libraries FINISHED! ")

   cat("\n\n ==> NOW system ready for RxpsG installation")
   cat("\n\n 1) go to the GitHub - URL  https://github.com/GSperanza/Tar.Gz")
   cat("\n 2) Download the latest version of RxpsG.tar.gz in a local folder on your PC")
   cat("\n 3) Rstudio menu -> Tools -> Install Packages -> Package Archive File (.tar.gz)")
   cat("\n 4) Control the successful installation of the package or if something is still lacking")
   cat("\n\n Thank you for using RxpsG. Good work")

}
