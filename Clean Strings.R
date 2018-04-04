Limpiar.cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",x)
  if(espacios){
    x<-gsub("\\W\\s","",iconv(x,to="ASCII//TRANSLIT"))
    } else {
    x<-gsub("\\W","",iconv(x,to="ASCII//TRANSLIT"))
    }
}
