RecodificarTop <- function(data, var_recode, var_top, fun_Top, n=10, lab_recodificar = "Otros"){
  
  # data = base de datos a recodificar
  # var_recode = Variable que se quiere recodificar
  # var_top = variable por la cual se hace el top
  # fun_Top = funcion de agregacion: sum, mean, n_distinct, median, n
  # n = Numero de categorias a recodificar
  # lab_recodificar = Etiqueta de los elementos fuera del top
  
  require(dplyr)
  require(lazyeval)
  datos = data
  
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n())
  } else{
    aux1 <- datos %>% 
      group_by_at(var_recode) %>% 
      summarise_(Var= interp(paste0(fun_Top, "(var, na.rm = T)"), var = as.name(var_top)))
  }
  
  aux1 <- aux1 %>% 
    arrange(desc(Var)) %>% 
    top_n(n, Var) %>% 
    select(1) %>% 
    unlist() %>% 
    as.character()
  
  print(aux1)
  
  data <- datos %>% 
    mutate(!!var_recode := !!parse_expr(interp(paste0("factor(ifelse(var %in% aux1, var,","'", 
                                                      lab_recodificar, "'), levels = c('", lab_recodificar, "', rev(aux1)), 
                                                      ordered = T)" ), var = as.name(var_recode)
                                               )
                                        )
           )
  
  data
  
  return(data)
}
