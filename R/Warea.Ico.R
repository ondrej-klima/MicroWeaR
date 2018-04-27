#' Warea.Ico
#'
#' Select a working area of a class Ico image through an interactive plot. The operator selects the center of the working area and its dimensions.
#' @param image.ico Ico.object: class Ico image
#' @param sizes numeric vector: side of the working area (in micron)
#' @return Ico.object: class Ico image with the selected working area
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export
Warea.Ico<-function (image.ico, sizes = c(200, 400, 600)) {
  x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, xlab = "", 
       ylab = "")
  points(rep(1, 5), seq(7.5, 1, length = 5), pch = 15, col = c(4, 
                                                               3, 3, 3, 2), cex = 3)
  text(rep(1, 5), seq(7.5, 1, length = 5), paste(" ", c("x1", 
                                                        paste(sizes[1], "x", sizes[1]), paste(sizes[2], "x", 
                                                                                              sizes[2]), paste(sizes[3], "x", sizes[3]), "select")), 
       pos = 4, cex = 2)
  plot_Ico(image.ico, xpos = 0, ypos = 0)
  print("Select the centroid of the working area")
  fix <- NULL
  fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                 bg = "red")
  points(fix$x, fix$y, cex = 2, pch = 3, lwd = 2.5, col = "red")
  dev.set(dev.prev())
  print("Insert desidered side lenght of the working area")
  fixs <- NULL
  fixs <- locator(n = 1, type = "n", col = "red", cex = 8, 
                  pch = 3, bg = "red")
  sel <- which.min(abs(seq(7.5, 1, length = 5) - fixs[[2]]))
  points(1, seq(7.5, 1, length = 5)[sel], pch = 4, col = 1, 
         cex = 3, lwd = 2)
  hside <- 0
  if (sel %in% c(2, 3, 4)) {
    hside <- sizes[sel - 1]/2
  }
  if (sel == 5) {
    intervallo <- NULL
    cat(paste("Insert value of the desidered size"), "\n")
    intervallo <- as.numeric(readLines(n = 1))
    hside <- intervallo/2
  }
  dev.set(dev.next())
  if (sel != 1) {
    work_coo <- list(xleft = fix$x - hside, ybottom = fix$y - 
                       hside, xright = fix$x + hside, ytop = fix$y + hside)
  }
  if (sel == 1) {
    work_coo <- list(xleft = image.ico$xlim[1], ybottom = image.ico$ylim[1], 
                     xright = image.ico$xlim[2], ytop = image.ico$ylim[2])
  }
  rect(xleft = work_coo[[1]], ybottom = work_coo[[2]], xright = work_coo[[3]], 
       ytop = work_coo[[4]], border = "red", lwd = 2)
  out <- list(image = image.ico, work_area = work_coo)
  return(out)
}