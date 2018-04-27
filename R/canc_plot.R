#' canc_plot
#'
#' Plot the scars stored in the object big_matrix associated to an image of class Ico.
#' @param image.ico Ico.object: Ico class image
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export
canc_plot<-function (image.ico, big_matrix) {
  plot_Ico(image.ico$image, xpos = 0, ypos = 0)
  rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
       image.ico$work_area[[3]], image.ico$work_area[[4]], border = "red", 
       lwd = 2)
  if (length(big_matrix) != 0) {
    for (i in 1:length(big_matrix)) {
      points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                            2), 2], pch = 4, col = 2, cex = 0.5, lwd = 0.5/10)
      points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                            2), 2], col = 2, lwd = 1, type = "l")
      points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                            4), 2], pch = 4, col = 2, cex = 0.5, lwd = 0.5/10)
      points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                            4), 2], col = 2, lwd = 1, type = "l")
    }
  }
}