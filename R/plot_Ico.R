#' plot_Ico
#'
#' Plot Ico class image. Set the matrix containing the marks coordinates in the scars argument, the function returns them on the image.
#' @param image.ico Ico.object: Ico class image
#' @param xpos numeric: window x coordinates 
#' @param ypos numeric: window xycoordinates
#' @return Ico.object: plotted class Ico image 
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @examples
#' \dontrun{
#' # A. brevirostris case-study
#' #load image
#' data("C_el_pic")
#' #plot image
#' plot_Ico(C_el_pic)
#' # C. elaphus eostephanoceros case-study
#' #load image
#' data("C_el_pic")
#' #plot image
#' plot_Ico(C_el_pic)
#' }
#' @export


 plot_Ico<-function(image.ico,xpos=820,ypos=0){
  width<-image.ico$xlim
  height<-image.ico$ylim
  windows(width=(image.ico$xlim[2]-image.ico$xlim[1]),
          height = (image.ico$ylim[2]-image.ico$ylim[1]),xpos=xpos,ypos=ypos,
          rescale="fit")

  plot(NA,xlim=c(image.ico$xlim[1],image.ico$xlim[2]),
       ylim=c(image.ico$ylim[1],image.ico$ylim[2]),axes=F)
  rasterImage(image.ico$image, image.ico$res[2], image.ico$res[1], 0, 0)
}

