# Carga en un solo dataset todos los archivos de cierta extension dentro de una caperta especificada

Cargar.Datos<-function (carpeta, extension="csv", exhaustivo=F, n_ultimos=0, ordenado=T, ausentes, separador=","){
    
    require("data.table")
    
    if(ordenado) file.list <- paste0(folder,"/",list.files(folder, pattern = paste0("+.",extension), recursive = F))
    else file.list <- sort(paste0(folder,"/",list.files(folder, pattern = paste0("+.",extension), recursive = F)))
    m=length(file.list)
    n=ifelse((n_ultimos>=m| n_ultimos<1),1, m-(n_ultimos-1))
    
    file.list<-file.list[n:length(file.list)]
    
    print("Importando:: ")
    print(file.list)
    Union <- do.call("rbind",
                     lapply(file.list, FUN = function(file) {
                         fread(file, sep=separador,
                               na.strings = ausentes,
                               col.names = names(fread(file.list[1], nrows = 1))
                         )}
                     )
    )
    return(Union)
}
