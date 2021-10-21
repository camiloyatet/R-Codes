ConvertirFecha <- function(x, origen='1899-12-30'){
  options(warn=-1)
  x_trans <- as.Date(ifelse(substr(x,5,5)=="/",  as.character(as.Date(x, "%Y/%m/%d")),
                            ifelse(substr(x,5,5)=="-",  as.character(as.Date(x, "%Y-%m-%d")),
                                   ifelse(substr(x,3,3)=="/",  as.character(as.Date(x, "%d/%m/%Y")),
                                          ifelse(!is.na(as.numeric(x)), as.character(as.Date(as.numeric(x), origin = origen)),
                                                 as.Date(NA))))))
  options(warn=0)
}
