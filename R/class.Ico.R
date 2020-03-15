#' class.Ico
#'
#' Convert an image in an Ico class object. Formats currently supported: .jpeg, .png and .tiff. Limited to greyscale images.
#' @param path character: path of an image file
#' @param image.type character: image format file ("jpg", "png", "tiff")
#' @return image matrix: class Ico image of the loaded file
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export

class.Ico<-function(path,image.type=c("jpg","png","tiff")){
  
  if(image.type=="jpg"){
    image<-readJPEG(path,native=F)
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
        image<-image[,,1]
      }}
    image<-apply(image,1,rev)
    image<-apply(image,1,rev)
  }
  
  if(image.type=="png"){
    image<-readPNG(path,native=F)
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
        image<-image[,,1]
      }}
    image<-apply(image,1,rev)
    image<-apply(image,1,rev)
  }
  
  if(image.type=="tiff"){
    image<-readTIFF(path,native=F)
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
        image<-image[,,1]
      }}
    image<-apply(image,1,rev)
    image<-apply(image,1,rev)
  }
  
  res<-dim(image)[1:2]
  output<-list("image"=image,"res"=res,"unit"="pixel","area"=min(res)/2,"zoom"=res/2,
               "xlim"=c(0,res[2]),
               "ylim"=c(0,res[1]),"scale_factor"=1)
  
  class(output) <- "Ico"
  return(output)
}
