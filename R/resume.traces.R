#' resume.traces
#'
#' Utilities to export plots and to remove traces
#' @param image.ico Ico.object: class Ico image
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @param sounds logical: if FALSE sounds are silences
#' @param delete.traces numeric vector: which scars needs to be deleted
#' @param delay numeric: specify seconds after that all devices will be closed
#' @param delete.traces.int logical: set TRUE to delete traces by using an interactive way
#' @return big_matrix:  a matrix with stored coordinates
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export
resume.traces<-function(image.ico, big_matrix,sounds=FALSE,
                         delete.traces=c(1,2,3),
                        delete.traces.int=FALSE,delay=3){
  
if(is.null(delete.traces)==FALSE & delete.traces.int==FALSE){
big_matrix<-big_matrix[-(delete.traces)]}

  
if(sounds == FALSE){
  options(locatorBell = FALSE)
}
if(sounds == TRUE){
  options(locatorBell = TRUE)
}

{
plot_Ico(image.ico$image, xpos = 0, ypos = 0)
rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
     image.ico$work_area[[3]], image.ico$work_area[[4]], 
     border = "red", lwd = 2)
for(i in 1:length(big_matrix)){
  points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                        2), 2], pch = 4, col = 2, cex = 1, lwd = 2)
  points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                        2), 2], col = 2, lwd = 1, type = "l")
  points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                        4), 2], pch = 4, col = 2, cex = 1, lwd = 2)
  points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                        4), 2], col = 2, lwd = 1, type = "l")
}

means_t<-lapply(big_matrix,colMeans)
means<-matrix(unlist(means_t),ncol=2,byrow = TRUE)
# points(means,col="yellow",pch=4)
text(means,col="yellow",labels=1:length(big_matrix))


x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
     xlab = "", ylab = "")
points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], 
       pch = c(-128190,-10162, -10006, -128270), col = c(2, "orange", 1, 4), 
       cex = 3, lwd = 5)
text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], 
     paste(" ",c("save", "quit", "delete", "zoom (+/-)")), pos = 4, 
     cex = 1.5)
}

# ops<-0
repeat{
# ops<-ops+1
j=0
fix <- NULL
fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
               bg = "red")
sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
if (sel == 4) {
  dev.set(dev.prev())
  dev.off()
  canc_plot(image.ico,NULL)
  zm()
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
  text(means,col="yellow",labels=1:length(big_matrix))
  dev.set(dev.next())
}
if (sel == 3) {

dev.set(dev.prev())

fix.t<-NULL
fix.t <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
               bg = "red")
# seled.t<-which.min(sqrt(rowSums((unlist(fix.t)-means)^2)))

seled.t<-mcNNindex(as.matrix(means),t(as.matrix(unlist(fix.t))),k=3)[1]

cat(seled.t,"\n")
big_matrix<-big_matrix[-seled.t]
dev.off()
{
  plot_Ico(image.ico$image, xpos = 0, ypos = 0)
  rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
       image.ico$work_area[[3]], image.ico$work_area[[4]], 
       border = "red", lwd = 2)
  for(i in 1:length(big_matrix)){
    points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                          2), 2], pch = 4, col = 2, cex = 1, lwd = 2)
    points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                          2), 2], col = 2, lwd = 1, type = "l")
    points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                          4), 2], pch = 4, col = 2, cex = 1, lwd = 2)
    points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                          4), 2], col = 2, lwd = 1, type = "l")
  }
  
  means_t<-lapply(big_matrix,colMeans)
  means<-matrix(unlist(means_t),ncol=2,byrow = TRUE)
  # points(means,col="yellow",pch=4)
  text(means,col="yellow",labels=1:length(big_matrix))
}
dev.set(dev.next())

}
if (sel == 1) {
dev.set(dev.prev())
savePlot(filename = "Rplot",
type = "tif",
device = dev.cur(),
restoreConsole = TRUE)
dev.set(dev.next())
}
if (sel == 2) {
Sys.sleep(delay)
replicate(length(dev.list()), dev.off()) 
break()
}
if(is.null(delete.traces)==FALSE & delete.traces.int==FALSE){
  big_matrix<-big_matrix[-(delete.traces)]}
}

return(big_matrix)
}


