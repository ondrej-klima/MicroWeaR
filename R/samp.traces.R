#' samp.traces
#'
#' This function allows to record the marks in an Ico class image through an interactive plot.
#' @param image.ico Ico.object: Ico class
#' @param cexp numeric: symbol size
#' @param lwdp numeric: line width
#' @return matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export

samp.traces<-function (image.ico, cexp = 0.5, lwdp = cexp/10) {
  graphics.off()
  plot_Ico(image.ico$image, xpos = 0, ypos = 0)
  rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
       image.ico$work_area[[3]], image.ico$work_area[[4]], border = "red", 
       lwd = 2)
  x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, xlab = "", 
       ylab = "")
  points(rep(1, 5)[-c(2, 4)], seq(7.5, 1, length = 5)[-c(2, 
                                                         4)], pch = c(-9658, 4, 15), col = c(1, 2, 4), cex = 3, 
         lwd = 5)
  text(rep(1, 5)[-c(2, 4)], seq(7.5, 1, length = 5)[-c(2, 4)], 
       paste(" ", c("start", "stop", "zoom (+/-)")), pos = 4, 
       cex = 1.5, col = c(1, 1, 1))
  j <- 0
  fix <- NULL
  fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                 bg = "red")
  sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
  if (sel == 1) {
    j <- j + 1
    dev.off()
    x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
    plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
         xlab = "", ylab = "")
    points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                               19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
           lwd = 5)
    text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                           c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
         cex = 1.5)
    dev.set(dev.prev())
  }
  if (sel == 4) {
    j <- j + 1
    dev.off()
    dev.set(dev.prev())
    x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
    plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
         xlab = "", ylab = "")
    points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                               19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
           lwd = 5)
    text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                           c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
         cex = 1.5)
    dev.set(dev.prev())
    zm()
  }
  j <- 1
  sel <- 1
  big_matrix <- list()
  while (sel != 3) {
    print(j)
    print(length(big_matrix))
    fix_n_x <- NULL
    fix_n_y <- NULL
    for (i in 1:4) {
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 0.1, 
                     pch = 4, bg = "red")
      points(fix$x, fix$y, pch = 4, col = 2, cex = cexp, 
             lwd = lwdp)
      fix_n_x <- c(fix_n_x, fix$x)
      fix_n_y <- c(fix_n_y, fix$y)
      if (i == 2) {
        points(fix_n_x[c(1, 2)], fix_n_y[c(1, 2)], col = 2, 
               lwd = lwdp, type = "l")
      }
      if (i == 4) {
        points(fix_n_x[c(3, 4)], fix_n_y[c(3, 4)], col = 2, 
               lwd = lwdp, type = "l")
      }
      coo_i <- cbind(fix_n_x, fix_n_y)
    }
    big_matrix[[j]] <- coo_i
    dev.set(dev.prev())
    fix <- NULL
    fix <- locator(n = 1, type = "n", col = "red", cex = 8, 
                   pch = 3, bg = "red")
    sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
    if (sel == 1) {
      j <- j + 1
      dev.set(dev.prev())
    }
    if (sel == 2) {
      j <- j
      dev.set(dev.prev())
      graphics.off()
      x11(xpos = 1200, ypos = 0, width = 3.5, height = 5)
      plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
           xlab = "", ylab = "")
      points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], 
             pch = c(17, 19, 4, 15), col = c(3, "orange", 
                                             2, 4), cex = 3, lwd = 5)
      text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], 
           paste(" ", c("next", "cancel", "stop", "zoom (+/-)")), 
           pos = 4, cex = 1.5)
      big_matrix <- big_matrix[-length(big_matrix)]
      canc_plot(image.ico, big_matrix)
    }
    if (sel == 4) {
      j <- j + 1
      dev.set(dev.prev())
      zm()
    }
  }
  return(big_matrix)
}