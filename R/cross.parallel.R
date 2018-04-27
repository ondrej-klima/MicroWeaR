#' cross.parallel
#'
#' Detect pairs of scratches which are parallel and/or that cross each other.
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @param image.ico Ico.object: class Ico image
#' @param Type character: scars type
#' @return numeric: matrix with number of pairs of parallel and/or scratches that cross each other
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @examples
#' \dontrun{
#' # A. brevirostris case-study
#' #load sampled scars
#' data("A_br_sam")
#' #load working area
#' data("A_br_war")
#' class<-autom_class(A_br_sam,A_br_war$image)
#' criss_cross=cross.parallel(A_br_sam,A_br_war$image,class$Type)
#' criss_cross
#' # C. elaphus eostephanoceros case-study
#' #load sampled scars
#' data("C_el_sam")
#' #load working area
#' data("C_el_war")
#' class<-autom_class(C_el_sam,C_el_war$image)
#' criss_cross=cross.parallel(C_el_sam,C_el_war$image,class$Type)
#' criss_cross
#' }
#' @export


 cross.parallel<-function(big_matrix,image.ico,Type){

 slopes<-NULL
  Scratch_list<-big_matrix[which(substr(Type,4,6)=="Scr")]
  if(length(Scratch_list)>1){
    combinazioni<-combn(length(Scratch_list), 2)
    cross_paral<-cbind(t(combinazioni),NA,NA)
    colnames(cross_paral)<-c("scratch_1","scratch_2","cross","paral")

    for(i in 1:ncol(combinazioni)){
      A<-Scratch_list[[combinazioni[,i][1]]][1,]
      B<-Scratch_list[[combinazioni[,i][1]]][4,]

      if(A==B){
        if(which(A==B)==1){
          A[1]=A[1]*1.0001}
      }

      if(A==B){
        if(which(A==B)==2){
          B[1]=B[1]*1.0001}
      }
      slope<-(A[2]-B[2])/(A[1]-B[1])
      slopes<-c(slopes,slope)
      y<--1
      x<--slope
      b<-A[2]-A[1]*slope
      C<-Scratch_list[[combinazioni[,i][2]]][1,]
      D<-Scratch_list[[combinazioni[,i][2]]][4,]
      if(C==D){
        if(which(C==D)==1){
          C[1]<-C[1]*1.0001}
      }

      if(C==D){
        if(which(C==D)==2){
          D[1]<-D[1]*1.0001}
      }

      slope2<-(C[2]-D[2])/(C[1]-D[1])
      slopes<-c(slopes,slope2)
      y2<--1
      x2<--slope2
      b2<-C[2]-C[1]*slope2

      coeffMatr<-matrix(c(x,1,x2,1),nrow=2,ncol=2,byrow=TRUE)
      RhsMatr<-matrix(c(b,b2),nrow=2,ncol=1,byrow=TRUE)
      Inverse<-solve(coeffMatr)
      Result<-Inverse %*% RhsMatr
      Result_x<-Result[1]
      Result_y<-Result[2]

      cross<-NULL
      paral<-NULL

      A<-Scratch_list[[combinazioni[,i][1]]][1,]
      B<-Scratch_list[[combinazioni[,i][1]]][4,]

      range_x_1<-range(Scratch_list[[combinazioni[,i][1]]][,1])
      range_y_1<-range(Scratch_list[[combinazioni[,i][1]]][,2])
      range_x_2<-range(Scratch_list[[combinazioni[,i][2]]][,1])
      range_y_2<-range(Scratch_list[[combinazioni[,i][2]]][,2])

      if(((Result_x>min(range_x_1)&Result_x<max(range_x_1))&
          (Result_x>min(range_x_2)&Result_x<max(range_x_2))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1)))==TRUE){
        cross<-"YES"
      }

      if(((Result_x>min(range_x_1)&Result_x<max(range_x_1))&
          (Result_x>min(range_x_2)&Result_x<max(range_x_2))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1)))==FALSE){
        cross<-"NO"
      }

      pos_A<-nn2(Scratch_list[[combinazioni[,i][1]]],t(as.matrix(c(Result_x,Result_y),ncol=2)))$nn.idx[1]
      pos_B<-nn2(Scratch_list[[combinazioni[,i][2]]],t(as.matrix(c(Result_x,Result_y),ncol=2)))$nn.idx[1]

      dist_A<-sqrt(sum((Scratch_list[[combinazioni[,i][1]]][pos_A,]-c(Result_x,Result_y))^2))
      dist_B<-sqrt(sum((Scratch_list[[combinazioni[,i][2]]][pos_B,]-c(Result_x,Result_y))^2))

      #threshold<-(image.ico$work_area$xright-image.ico$work_area$xleft)*ThF

      if((min(dist_A,dist_B)<((image.ico$area*2)*2))==FALSE){
        paral<-"YES"}
      if((min(dist_A,dist_B)>((image.ico$area*2)*2))==FALSE){
        paral<-"NO"}

      cross_paral[i,3]<-cross
      cross_paral[i,4]<-paral
    }
  }


  slopes_matrix<-matrix(slopes,ncol=2,byrow = T)
  m2<-slopes_matrix[,2]
  m1<-slopes_matrix[,1]
  angle<-round((atan(abs((m2-m1)/(1+m1*m2))))*180/pi,2)
  cross_paral<-cbind(cross_paral,angle)
  return(cross_paral)
}
