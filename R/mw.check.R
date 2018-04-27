#' mw.check
#'
#' Check through an interactive multi-plot the classification provided by the autom_class function. Before runnin output.Ico with the a posteriori classification, remember to run again the cross.parallel function with the updated scars classifications.
#' @param image.ico Ico.object: class Ico image
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @param type_traces character vector: classified scars types
#' @param vector numeric: which scars needs to be rechecked
#' @return Ico.object: class Ico image
#' @return "priori" character vector: classified scars before running the check function
#' @return "posteriori" character vector: classified scars after running the check function
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export
mw.check<-function (image.ico, big_matrix, type_traces, vector=NULL) 
{
  types <- c("Fi.Scr", "Co.Scr", "Sm.Pit", "Lg.Pit")
  types_traces_o <- type_traces
  graphics.off()
  if(is.null(vector)==TRUE){vector<-1:(length(big_matrix))}
  for (i in vector) {
    graphics.off()
    plot_Ico(image.ico$image, xpos = 0, ypos = 0)
    rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
         image.ico$work_area[[3]], image.ico$work_area[[4]], 
         border = "red", lwd = 2)
    points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                          2), 2], pch = 4, col = 2, cex = 1, lwd = 2)
    points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                          2), 2], col = 2, lwd = 1, type = "l")
    points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                          4), 2], pch = 4, col = 2, cex = 1, lwd = 2)
    points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                          4), 2], col = 2, lwd = 1, type = "l")
    x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
    plot(NA, xlim = c(0, 20), ylim = c(-5, 10), axes = F, 
         xlab = "", ylab = "")
    points(c(0, 7), c(9, 9), type = "l", lty = 3, lwd = 2)
    rect(0, 6, 7, 7, lty = 2)
    points(3.5, 4, pch = 1, lty = 2, cex = 3)
    points(3.5, 1, pch = 1, lty = 2, cex = 5)
    text(rep(15, 4), c(9, 6.5, 4, 1), labels = types)
    points(3.5, -5, pch = 15, lty = 2, cex = 4, col = "blue")
    text(15, -5, labels = "zoom (+/-)")
    dev.set(dev.prev())
    x11(xpos = 1100, ypos = 540, width = 4.5, height = 2.5)
    plot(NA, xlim = c(0, 3.5), ylim = c(0, 1.5), axes = F, 
         xlab = "", ylab = "")
    text(1.5, 1, labels = type_traces[i], cex = 4, col = "red")
    dev.set(dev.prev())
    fix <- NULL
    fix <- locator(n = 1, type = "n", col = "red", cex = 8, 
                   pch = 3, bg = "red")
    sel <- which.min(abs(fix$y - c(9, 7, 4, 1, -3)))
    if (sel == 5) {
      dev.set(dev.prev())
      zm()
      dev.set(dev.next())
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 8, 
                     pch = 3, bg = "red")
      sel <- which.min(abs(fix$y - c(9, 7, 4, 1, -3)))
    }
    if (sel %in% c(1:4)) {
      type_t <- types[sel]
      types_traces_o[i] <- type_t
      dev.set(dev.prev())
      dev.set(dev.prev())
      dev.off(which = dev.cur())
    }
    if (sel == 5) {
      dev.set(dev.prev())
      zm()
      dev.set(dev.next())
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 8, 
                     pch = 3, bg = "red")
      sel <- which.min(abs(fix$y - c(9, 7, 4, 1, -3)))
    }
  }
  graphics.off()
  out <- list(priori = type_traces, posteriori = types_traces_o)
  return(out)
}
