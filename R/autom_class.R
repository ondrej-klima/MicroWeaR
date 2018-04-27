 #' autom_class
 #'
 #' This function classifies into different categories the recorded marks using the samp.traces function.
 #' @param big_matrix matrix: a matrix with stored the coordinates (4) of the microwears
 #' @param image.ico Ico.object: class Ico image
 #' @param Pit_Scr numeric: parameter to discriminate between scratches and pits. If the ratio length/windth is minor or equal to the set value (4 for example) a scar is classified as pit; on the contrary as scratch
 #' @param Sm.Lg_Pit numeric: parameter to classify a pit as small or large width based (in micron)
 #' @param Fi.Co_Scr numeric: parameter to classify a scratch as course or fine width based (in micron)
 #' @param cexp numeric: size of points
 #' @return Type character vector: types of the classified scars
 #' @return Matrix matrix: length and width (in micron) of each scar
 #' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
 #' @examples
 #' \dontrun{
 #' # A. brevirostris case-study
 #' #load sampled scars
 #' data("A_br_sam")
 #' #load working area
 #' data("A_br_war")
 #' class<-autom_class(A_br_sam,A_br_war$image)
 #' # C. elaphus eostephanoceros case-study
 #' #load sampled scars
 #' data("C_el_sam")
 #' #load working area
 #' data("C_el_war")
 #' class<-autom_class(C_el_sam,C_el_war$image)
 #' }
 #' @export
autom_class<-function(big_matrix,image.ico,Pit_Scr=4,Sm.Lg_Pit=8,Fi.Co_Scr=3,cexp=max(image.ico$res)/10){

  Type<-rep(NA,length(big_matrix))

  vett_1_temp<-NULL
  vett_2_temp<-NULL
  for(i in 1:length(big_matrix)){
    vett_1_temp[i]<-sqrt(sum((big_matrix[[i]][1,]-big_matrix[[i]][2,])^2))
    vett_2_temp[i]<-sqrt(sum((big_matrix[[i]][3,]-big_matrix[[i]][4,])^2))
  }

  Type[which((vett_1_temp + vett_2_temp)==0)]<-"Sm.Pit"

  vett_1<-vett_1_temp
  vett_2<-vett_2_temp
  vett_1[which(vett_2_temp>vett_1_temp)]<-vett_2_temp[which(vett_2_temp>vett_1_temp)]
  vett_2[which(vett_2_temp>vett_1_temp)]<-vett_1_temp[which(vett_2_temp>vett_1_temp)]

  ratio<-vett_1/vett_2

  Type[which(ratio<=Pit_Scr)]<-"Pit"
  Type[which(ratio>Pit_Scr)]<-"Scratch"

  Type[which(Type=="Pit")[which(vett_1[Type=="Pit"]<=Sm.Lg_Pit)]]<-"Sm.Pit"
  Type[which(Type=="Pit")[which(vett_1[Type=="Pit"]>Sm.Lg_Pit)]]<-"Lg.Pit"

  Type[which(Type=="Scratch")[which(vett_2[Type=="Scratch"]<=Fi.Co_Scr)]]<-"Fi.Scr"
  Type[which(Type=="Scratch")[which(vett_2[Type=="Scratch"]>Fi.Co_Scr)]]<-"Co.Scr"

  plot_Ico(image.ico,xpos = 0, ypos = 0)

  col_points<-"yellow"
  if("Sm.Pit"%in%Type){
    for(i in 1:length(which(Type=="Sm.Pit"))){
      points(big_matrix[[which(Type=="Sm.Pit")[i]]],pch=19,cex=cexp/2,col="red")
      text(x=apply(big_matrix[[which(Type=="Sm.Pit")[i]]],2,mean)[1],
           y=apply(big_matrix[[which(Type=="Sm.Pit")[i]]],2,mean)[2],
      labels<-which(Type=="Sm.Pit")[i],pos=3,cex=cexp,col=col_points)
    }}

  if("Lg.Pit"%in%Type){
    for(i in 1:length(which(Type=="Lg.Pit"))){
      points(big_matrix[[which(Type=="Lg.Pit")[i]]][1:2,],pch=19,cex=cexp/2,col="red")
      points(big_matrix[[which(Type=="Lg.Pit")[i]]][3:4,],pch=19,cex=cexp/2,col="red")
      text(x=apply(big_matrix[[which(Type=="Lg.Pit")[i]]],2,mean)[1],
           y=apply(big_matrix[[which(Type=="Lg.Pit")[i]]],2,mean)[2],labels=which(Type=="Lg.Pit")[i],pos=3,cex=cexp,col=col_points)
    }}


  if("Fi.Scr"%in%Type){
    for(i in 1:length(which(Type=="Fi.Scr"))){
      points(big_matrix[[which(Type=="Fi.Scr")[i]]][c(1,2),],type="l",pch=4,cex=cexp/2,col="red",lty=2,lwd=2)
      points(big_matrix[[which(Type=="Fi.Scr")[i]]][c(3,4),],type="l",pch=4,cex=cexp/2,col="red",lty=2,lwd=2)
      bary<-apply(big_matrix[[which(Type=="Fi.Scr")[i]]],2,mean)
      text(t(as.matrix(bary)),labels=which(Type=="Fi.Scr")[i],pos=3,cex=cexp,lwd = 2,col=col_points)
    }}

  if("Co.Scr"%in%Type){
    for(i in 1:length(which(Type=="Co.Scr"))){
      points(big_matrix[[which(Type=="Co.Scr")[i]]][1:2,],type="l",pch=4,cex=cexp/2,col="red",lty=2,lwd=2)
      points(big_matrix[[which(Type=="Co.Scr")[i]]][3:4,],type="l",pch=4,cex=cexp/2,col="red",lty=2,lwd=2)
      bary<-apply(big_matrix[[which(Type=="Co.Scr")[i]]],2,mean)
      text(t(as.matrix(bary)),labels=which(Type=="Co.Scr")[i],pos=3,cex=cexp,lwd = 2,col=col_points)
    }}

  if(length(Type)>1){
    matrix_meas<-round(cbind(vett_1,vett_2),2)
    colnames(matrix_meas)<-c("length","width")
    rownames(matrix_meas)<-Type
    out<-list("Type"=Type,"Matrix"=matrix_meas)
    return(out)
    }

  if(length(Type)==1){
    return(Type)}
  }
