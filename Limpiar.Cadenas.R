Limpiar.Cadenas <-function(x, espacios=T){
  require("stringi")
  x<-gsub("\\.", "_",tolower(gsub("\\W","",x)))
  x<-gsub("([\\W])\\1+","\\1",stri_trans_general(x,id="Latin-ASCII"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
        x
    }
}
