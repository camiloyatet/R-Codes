# Paquetes ----
#' Cargar e Instalar Paquetes Requeridos
#'
#' Esta función verifica si los paquetes de R especificados están instalados. 
#' Si algún paquete no está instalado, lo instala. Posteriormente, carga todos los paquetes solicitados.
#'
#' @param pkg Un vector de caracteres con los nombres de los paquetes que se desean verificar, instalar (si es necesario) y cargar.
#' @return Un vector lógico que indica si cada paquete fue cargado con éxito (\code{TRUE} si se cargó, \code{FALSE} en caso contrario).
#' @examples
#' Loadpkg(c("ggplot2", "dplyr"))  # Instala y carga ggplot2 y dplyr si es necesario
#' @export
Loadpkg <- function(pkg) {
  # Identificar los paquetes que no están instalados
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  # Instalar paquetes faltantes
  if (length(new.pkg) > 0) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  
  # Cargar los paquetes requeridos y devolver vector lógico de éxito/fracaso
  sapply(pkg, require, character.only = TRUE)
}


# Cadenas de Caracteres ----

#' Limpiar y Normalizar Nombres
#'
#' Esta función limpia y normaliza una cadena de texto eliminando espacios repetidos y 
#' convirtiendo todo a mayúsculas. Se asegura de que los espacios múltiples sean reducidos a uno solo 
#' y elimina espacios en blanco al principio y al final de la cadena.
#'
#' @param s Un vector de caracteres con los nombres o cadenas de texto a limpiar.
#' @return Un vector de caracteres con las cadenas normalizadas, sin espacios en blanco duplicados y en mayúsculas.
#' @examples
#' LimpiarNombres("  Juan    Pérez   ")  # Devuelve "JUAN PÉREZ"
#' @export
LimpiarNombres <- function(s) {
  # Quitar espacios extra, reducir múltiples espacios a uno, y convertir a mayúsculas
  x <- trimws(str_to_upper(gsub("([\\s])\\1+", "\\1", s, perl = TRUE)))
  
  return(x)
}

#' Limpiar y Normalizar una Cadena de Texto
#'
#' Esta función limpia y normaliza una cadena o un vector de texto aplicando diferentes transformaciones,
#' tales como la eliminación de espacios, números, caracteres especiales y acentos, según se especifique.
#'
#' @param x Un vector de caracteres con las cadenas de texto a limpiar.
#' @param rem_espacios Un valor lógico (TRUE o FALSE) que indica si se deben eliminar todos los espacios en blanco. 
#' El valor predeterminado es \code{FALSE}.
#' @param rem_numeros Un valor lógico que indica si se deben eliminar los números. 
#' El valor predeterminado es \code{TRUE}.
#' @param rem_caresp Un valor lógico que indica si se deben eliminar los caracteres especiales (no alfanuméricos). 
#' El valor predeterminado es \code{TRUE}.
#' @param rem_acentos Un valor lógico que indica si se deben eliminar los acentos. 
#' El valor predeterminado es \code{TRUE}.
#' @return Un vector de caracteres con las cadenas de texto limpias, después de aplicar las transformaciones especificadas.
#' @examples
#' LimpiarCadena(c("¡Hola, mundo!", "Texto con números 123 y acentos áéíóú."))  
#' # Devuelve: "HOLA MUNDO" "TEXTO CON NUMEROS Y ACENTOS AEIOU"
#' @export
LimpiarCadena <- function(x, rem_espacios = FALSE, rem_numeros = TRUE, rem_caresp = TRUE, rem_acentos = TRUE) {
  
  # Convertir a mayúsculas y eliminar espacios repetidos
  x <- trimws(str_to_upper(gsub("([\\s])\\1+", "\\1", x, perl = TRUE)))
  
  # Eliminar espacios si se indica
  if (rem_espacios) {
    x <- gsub("\\s", "", x)
  }
  
  # Eliminar números si se indica
  if (rem_numeros) {
    x <- gsub("\\d", "", x)
  }
  
  # Eliminar caracteres especiales si se indica
  if (rem_caresp) {
    x <- gsub("[^[:alnum:][:space:]]", "", x)
  }
  
  # Eliminar acentos si se indica
  if (rem_acentos) {
    x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  }
  
  return(x)
}


#' Unir Múltiples Cadenas de Texto
#'
#' Esta función une múltiples cadenas de texto en una sola cadena, con la opción de omitir valores \code{NA}. 
#' Es similar a \code{paste}, pero permite controlar la eliminación de valores ausentes.
#'
#' @param ... Varios vectores de caracteres que se desean unir.
#' @param sep Separador entre las cadenas. El valor predeterminado es un espacio en blanco (\code{" "}).
#' @param collapse Un valor opcional para definir un separador entre los elementos resultantes, 
#' similar a \code{paste}'s \code{collapse}. El valor predeterminado es \code{NULL}.
#' @param na.rm Un valor lógico que indica si los valores \code{NA} deben ser eliminados. 
#' El valor predeterminado es \code{FALSE}, lo que incluye los \code{NA} en el resultado.
#' @return Una cadena de texto resultante de la unión de las cadenas especificadas.
#' @examples
#' UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
#' # Devuelve "Hola-Mundo"
#' UnirCadenas("Hola", NA, "Mundo", sep = " ", na.rm = FALSE)
#' # Devuelve "Hola NA Mundo"
#' @export
UnirCadenas <- function(..., sep = " ", collapse = NULL, na.rm = FALSE) {
  
  # Función auxiliar para unir, omitiendo NA y espacios en blanco si na.rm es TRUE
  paste.na <- function(x, sep) {
    x <- gsub("^\\s+|\\s+$", "", x)  # Eliminar espacios en los extremos
    ret <- paste(na.omit(x), collapse = sep)  # Omitir NA y unir
    if (ret == "") return(NA)  # Si el resultado está vacío, retornar NA
    return(ret)
  }
  
  if (!na.rm) {
    # Usar paste normal si no se omiten los NA
    return(paste(..., sep = sep, collapse = collapse))
  } else {
    # Si na.rm es TRUE, creamos un data.frame para unir fila por fila, evitando NA
    df <- data.frame(..., stringsAsFactors = FALSE)
    ret <- apply(df, 1, function(row) paste.na(row, sep))
    
    # Si collapse es NULL, devolver vector; si no, aplicar collapse
    if (is.null(collapse)) {
      return(ret)
    } else {
      return(paste.na(ret, sep = collapse))
    }
  }
}

# HTML ----

#' Insertar Saltos de Línea HTML
#'
#' Esta función genera una cadena con un número especificado de saltos de línea en formato HTML (\code{<br/>}).
#'
#' @param n Un valor entero que indica el número de saltos de línea a generar. El valor predeterminado es \code{1}.
#' @return Una cadena de texto que contiene \code{<br/>} repetido \code{n} veces.
#' @examples
#' Saltos(3)  # Devuelve "<br/><br/><br/>"
#' @export
Saltos <- function(n = 1) {
  strrep('<br/>', n)
}

#' Insertar Espacios en HTML
#'
#' Esta función genera una cadena con un número especificado de espacios en formato HTML (\code{&emsp;}).
#'
#' @param n Un valor entero que indica el número de espacios a generar. El valor predeterminado es \code{1}.
#' @return Una cadena de texto que contiene \code{&emsp;} repetido \code{n} veces.
#' @examples
#' Espacios(4)  # Devuelve "&emsp;&emsp;&emsp;&emsp;"
#' @export
Espacios <- function(n = 1) {
  strrep('&emsp;', n)
}

#' Crear una Etiqueta Obligatoria con un Asterisco
#'
#' Esta función genera una etiqueta HTML que incluye el texto de la etiqueta y un asterisco (`*`)
#' para indicar que el campo es obligatorio. El asterisco es presentado con una clase CSS llamada
#' `mandatory_star` para facilitar el estilo visual.
#'
#' @param label Texto de la etiqueta que se desea mostrar.
#' @return Un objeto `tagList` que representa la etiqueta HTML con el asterisco visual.
#' 
#' @examples
#' # Crear una etiqueta obligatoria
#' labelObligatorio("Nombre del Usuario")
#'
#' @export
labelObligatorio <- function(label) {
  # Retorna una lista de etiquetas HTML que incluye el texto y un asterisco para marcar como obligatorio
  tagList(
    label,  # El texto de la etiqueta
    span("*", class = "mandatory_star")  # Asterisco con clase CSS para estilo
  )
}



# Números ----
#' Reemplazar Valores NaN, Inf y -Inf por 0
#'
#' Esta función reemplaza los valores `NaN`, `Inf` y `-Inf` en un vector numérico por 0.
#'
#' @param x Un vector numérico en el que se buscarán los valores `NaN`, `Inf` y `-Inf`.
#' @return Un nuevo vector numérico con los valores `NaN`, `Inf` y `-Inf` reemplazados por 0.
#' @examples
#' SiError_0(c(1, NaN, 2, Inf, -Inf))  # Devuelve: 1 0 2 0 0
#' @export
SiError_0 <- function(x) {
  # Reemplazar valores NaN, Inf y -Inf por 0 usando is.finite para la comprobación
  x[!is.finite(x)] <- 0
  return(x)
}

#' Calcular Variación Porcentual
#'
#' Esta función calcula la variación porcentual entre un número inicial y uno final. 
#' La variación porcentual se calcula de acuerdo con las siguientes reglas:
#'  - Si ambos valores son 0, la variación es 0.
#'  - Si el valor inicial es 0 y el final es distinto de 0, la variación es 0.
#'  - Si ambos valores tienen el mismo signo (o son ambos positivos o ambos negativos), se calcula como \code{(fin - ini) / ini}.
#'  - Si los valores tienen signos opuestos, la variación se calcula como \code{(fin - ini) / abs(ini)}.
#'
#' @param ini El número inicial (denominador).
#' @param fin El número final (numerador).
#' @return Un valor numérico que representa la variación porcentual.
#' @examples
#' Variacion(10, 15)   # Devuelve 0.5 (50% de aumento)
#' Variacion(0, 10)    # Devuelve 0 (por convención en este caso)
#' Variacion(-10, 5)   # Devuelve 1.5 (150% de aumento, en valor absoluto del denominador)
#' @export
Variacion <- function(ini, fin) {
  # Verificar casos especiales para evitar divisiones innecesarias
  if (ini == 0 & fin == 0) {
    return(0)
  } else if (ini == 0) {
    return(0)  # Si el inicial es 0, la variación es 0 por convención
  } else if (ini * fin >= 0) {
    # Si ambos números tienen el mismo signo, calcular la variación normal
    return((fin - ini) / ini)
  } else {
    # Si los números tienen signos opuestos, calcular la variación sobre el valor absoluto del inicial
    return((fin - ini) / abs(ini))
  }
}

#' Calcular la Moda de un Vector
#'
#' Esta función calcula la moda de un vector numérico o categórico. La moda es el valor que aparece con mayor frecuencia en el vector.
#' Si hay más de una moda, la función devuelve solo una de ellas.
#'
#' @param x El vector numérico o categórico del cual se desea calcular la moda.
#' @param na.rm Un valor lógico que indica si se deben excluir los valores `NA` en el cálculo de la moda. 
#' El valor predeterminado es `TRUE`.
#' @return El valor de la moda del vector, es decir, el valor que aparece con mayor frecuencia. Si hay más de una moda, devuelve el primero encontrado.
#' @examples
#' Moda(c(1, 2, 2, 3, 4))  # Devuelve 2
#' Moda(c("a", "b", "b", "c", "c", "c"))  # Devuelve "c"
#' Moda(c(1, 1, 2, 2, 3), na.rm = TRUE)  # Devuelve 1
#' @export
Moda <- function(x, na.rm = TRUE) {
  # Eliminar valores NA si na.rm es TRUE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Calcular los valores únicos y determinar la frecuencia máxima
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


# Fechas ----

#' Obtener el Primer Día de una Unidad de Tiempo en una Fecha
#'
#' Esta función calcula el primer día de la unidad temporal especificada (mes, año, etc.) de una fecha dada.
#' Si no se especifica, el valor predeterminado es calcular el primer día del mes.
#'
#' @param x La fecha de la cual se desea obtener el primer día. Puede ser un objeto de clase \code{Date} o \code{POSIXt}.
#' @param uni La unidad de tiempo que se desea usar para el cálculo. Puede ser "month", "year", entre otras. El valor predeterminado es "month".
#' @return Un objeto de clase \code{Date} que representa el primer día de la unidad temporal de la fecha dada.
#' @import lubridate
#' @examples
#' PrimerDia("2023-10-15")  # Devuelve "2023-10-01"
#' PrimerDia(as.Date("2023-05-22"))  # Devuelve "2023-05-01"
#' PrimerDia("2023-10-15", uni = "year")  # Devuelve "2023-01-01"
#' @export
PrimerDia <- function(x, uni = "month") {
  # Verificar si x es una fecha válida y convertir a tipo Date
  require(lubridate)
  x <- lubridate::floor_date(as.Date(x), unit = uni)
  return(x)
}

#' Convertir una Fecha en un Formato de Texto Personalizado
#'
#' Esta función convierte una fecha en un formato de texto personalizado, permitiendo seleccionar qué partes de la fecha mostrar (día, mes, año).
#'
#' @param x Un vector de fechas que se desea convertir.
#' @param dia Un valor lógico que indica si se debe incluir el día (predeterminado: TRUE).
#' @param dia_nombre Un valor lógico que indica si se debe incluir el nombre completo del día (predeterminado: TRUE).
#' @param dia_nom_abr Un valor lógico que indica si se debe incluir el nombre abreviado del día (predeterminado: TRUE).
#' @param mes Un valor lógico que indica si se debe incluir el mes (predeterminado: TRUE).
#' @param mes_abr Un valor lógico que indica si se debe incluir el nombre abreviado del mes (predeterminado: TRUE).
#' @param anho Un valor lógico que indica si se debe incluir el año (predeterminado: TRUE).
#' @param anho_abr Un valor lógico que indica si se debe incluir el año abreviado (predeterminado: TRUE).
#' @param sep_texto Un valor lógico que indica si se debe incluir un separador de texto entre las partes de la fecha (predeterminado: TRUE).
#' @return Un vector de cadenas de texto que representa las fechas en el formato especificado.
#' @import lubridate
#' @import stringr
#' @examples
#' FechaTexto(as.Date(c("2023-10-15", "2022-05-22")))
#' FechaTexto(as.Date(c("2023-10-15", "2022-05-22")), dia_nombre = FALSE)
#' @export
FechaTexto <- function(x, dia = TRUE, dia_nombre = TRUE, dia_nom_abr = TRUE, 
                       mes = TRUE, mes_abr = TRUE, anho = TRUE, 
                       anho_abr = TRUE, sep_texto = TRUE) {
  
  # Requiere las librerías necesarias
  require(lubridate)
  require(stringr)
  
  # Definición de los días de la semana
  dia_l <- c('Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo')
  dia_c <- c('lun', 'mar', 'mié', 'jue', 'vie', 'sáb', 'dom')
  
  # Definición de los meses
  mes_l <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 
             'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  mes_c <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
  
  # Convertir x a formato de fecha en caso que no lo sea
  x <- as.Date(x)
  
  # Día de la semana
  dn <- ifelse(dia_nombre, 
               ifelse(dia_nom_abr, dia_c[wday(x, week_start = 1)], dia_l[wday(x, week_start = 1)]), 
               NA)
  
  # Mes
  m <- ifelse(mes, 
              ifelse(mes_abr, mes_c[month(x)], mes_l[month(x)]), 
              NA)
  
  # Año
  y <- ifelse(anho, 
              ifelse(anho_abr, format(x, "%y"), format(x, "%Y")), 
              NA)
  
  # Día
  d <- ifelse(dia, day(x), NA)
  
  # Construcción del resultado
  res <- paste(dn, paste(m, y, sep = ifelse(sep_texto, " de ", "")), sep = ifelse(sep_texto, ", ", ""))
  
  # Retornar el resultado
  return(res)
}



#' Calcular Edad en Años Entre Dos Fechas
#'
#' Esta función calcula la edad en años completos entre dos fechas, considerando los años bisiestos.
#'
#' @param from La fecha de nacimiento (fecha inicial).
#' @param to La fecha actual (fecha final).
#' @return La edad en años entre las dos fechas.
#' @import lubridate
#' @examples
#' EdadCumplida(as.Date("1990-05-25"), Sys.Date())  # Devuelve la edad actual
#' EdadCumplida(as.Date("1985-10-10"), as.Date("2023-01-01"))  # Devuelve 37
#' @export
EdadCumplida <- function(from, to) {
  # Convertir a objetos de clase Date
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Calcular la diferencia en años
  age <- as.numeric(difftime(to, from, units = "days")) / 365.25
  
  # Redondear a la cantidad entera de años
  return(floor(age))
}



# Manejo de Datos ----

#' Recodificación de Categorías Menos Frecuentes
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode El nombre de la variable que se desea recodificar.
#' @param var_top El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param n El número máximo de categorías principales a conservar (predeterminado: 10).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @import dplyr forcats rlang
#' @export
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, nom_var, lab_recodificar = "OTROS"){
  
  require(rlang)
  require(forcats)
  datos = data
  
  # Calcula las frecuencias o las estadísticas según la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var = n(),
                Pct = Var/unique(Tot))
  } else {
    aux1 <- datos %>% 
      mutate(Tot = !!parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var = !!parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                Pct = Var/unique(Tot))
  }
  
  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 %>%
    arrange(desc(Var)) %>%
    mutate(Seq = row_number(),
           !!nom_var := ifelse(Seq <= n, as.character(!!parse_expr(var_recode)), lab_recodificar)
           ) %>%
    select(all_of(var_recode), all_of(nom_var))
  
  # Recodifica las categorías en el dataset original
  data <- datos %>%
    left_join(aux2, by = var_recode) %>% 
    mutate(!!nom_var := factor(!!sym(nom_var), levels = c(unique(aux2[[nom_var]])), ordered = TRUE),
           !!nom_var := fct_relevel(!!sym(nom_var), lab_recodificar, after = Inf))  # Asegura que 'lab_recodificar' esté al final
  
  return(data)
}


#' Recodificación de Categorías Menos Frecuentes (Relativa)
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor relativo o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode El nombre de la variable que se desea recodificar.
#' @param var_top El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param pct_min El porcentaje mínimo necesario para considerar una categoría principal (predeterminado: 0.05).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @import dplyr forcats rlang
#' @export
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, nom_var, lab_recodificar = "OTROS") {
  
  datos = data
  
  # Calcula las frecuencias o estadísticas relativas de acuerdo a la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var = n(),
                Pct = Var / unique(Tot))
  } else {
    aux1 <- datos %>% 
      mutate(Tot = !!parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var = !!parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                Pct = Var / unique(Tot))
  }
  
  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 %>% 
    arrange(desc(Var)) %>% 
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Pct > pct_min, as.character(", 
                                            var_recode, "), '", 
                                            lab_recodificar, "')"))) %>% 
    select(all_of(var_recode), all_of(nom_var))
  
  # Recodifica las categorías en el dataset original
  data <- datos %>%
    left_join(aux2, by = var_recode) %>%
    mutate(!!nom_var := factor(!!sym(nom_var), levels = c(unique(aux2[[nom_var]])), ordered = TRUE),
           !!nom_var := fct_relevel(!!sym(nom_var), lab_recodificar, after = Inf))  # Asegura que 'lab_recodificar' esté al final
  
  return(data)
}


# Elementos Gráficos ----

#' Crear Línea Vertical en un Gráfico
#'
#' Esta función genera una línea vertical que puede ser utilizada en gráficos interactivos,
#' como los creados con la librería \code{plotly}. La línea se dibuja en una posición especificada
#' sobre el eje x y se puede personalizar su color.
#'
#' @param x Posición en el eje x donde se desea trazar la línea (por defecto: 0).
#' @param color Color de la línea (por defecto: "red").
#' @return Una lista con las propiedades necesarias para dibujar la línea vertical en un gráfico.
#' @examples
#' vline(2)  # Traza una línea vertical en x = 2 con color rojo.
#' vline(3, "blue")  # Traza una línea vertical en x = 3 con color azul.
#' @export
vline <- function(x = 0, color = "red") {
  # Definir las propiedades de la línea vertical
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",  # Usa "paper" para que la línea cubra todo el gráfico
    x0 = x, 
    x1 = x, 
    line = list(color = color)  # Asigna el color a la línea
  )
}

#' Crear Línea Horizontal en un Gráfico
#'
#' Esta función genera una línea horizontal que puede ser utilizada en gráficos interactivos,
#' como los creados con la librería \code{plotly}. La línea se dibuja en una posición especificada
#' sobre el eje y y se puede personalizar su color.
#'
#' @param y Posición en el eje y donde se desea trazar la línea (por defecto: 0).
#' @param color Color de la línea (por defecto: "#ff3a21").
#' @return Una lista con las propiedades necesarias para dibujar la línea horizontal en un gráfico.
#' @examples
#' hline(3)  # Traza una línea horizontal en y = 3 con color #ff3a21.
#' hline(5, "green")  # Traza una línea horizontal en y = 5 con color verde.
#' @export
hline <- function(y = 0, color = "#ff3a21") {
  # Definir las propiedades de la línea horizontal
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",  # Usa "paper" para que la línea cubra todo el gráfico
    y0 = y, 
    y1 = y, 
    line = list(color = color)  # Asigna el color a la línea
  )
}

#' Generar Colores para Gráficos según Valores
#'
#' Esta función asigna colores a un conjunto de valores, con una excepción específica
#' para los valores que contienen la cadena "RACAFE & CIA S C A", que siempre se asigna un color particular 
#' (firebrick). Para los otros valores, se asignan colores graduados desde el gris claro
#' hasta el gris oscuro, dependiendo de la cantidad de valores.
#'
#' @param input_values Un vector de valores para los cuales se desea asignar colores.
#'                     Los valores que contengan la cadena "RACAFE & CIA S C A" serán asignados
#'                     el color 'firebrick'. El resto de los valores recibirá colores graduales de gris.
#' @return Un vector de colores asignados a cada valor en el vector de entrada.
#'         Los valores que coincidan con "RACAFE & CIA S C A" (usando `grepl`) tendrán el color 'firebrick',
#'         y el resto de los valores se asignarán colores graduales de gris (desde gris claro a gris oscuro).
#' 
#' @examples
#' # Asignar colores a un vector de valores
#' ColoresRacafe(c("RACAFE & CIA S C A", "Otro Valor", "Valor Importante"))
#'
#' @import grDevices
#' @export
ColoresRacafe <- function(input_values) {
  
  # Verificar si la entrada es un vector
  if (!is.vector(input_values)) {
    stop("El parámetro 'input_values' debe ser un vector.")
  }
  
  # Calcular la cantidad de valores en el vector
  num_valores <- length(input_values)
  
  # Crear una paleta de colores desde gris claro (gray90) a gris oscuro (gray20)
  pal <- colorRampPalette(c("gray90", "gray20"))
  
  # Asignar los colores graduales a los valores (de acuerdo con el número de valores)
  cols <- pal(num_valores)
  
  # Usar grepl para buscar coincidencias parciales con la cadena "RACAFE"
  # y asignar 'firebrick' a los valores coincidentes, el resto recibirá colores graduales
  colores_asignados <- ifelse(grepl("RACAFE", input_values, ignore.case = T), 'firebrick', cols)
  
  # Retornar los colores asignados
  return(colores_asignados)
}


#' Generar Colores Graduales entre Verde y Azul
#'
#' Esta función genera una paleta de colores graduales que van desde el color verde 
#' "forestgreen" hasta el color azul oscuro "royalblue4", según el número de valores que se pasen como argumento.
#'
#' @param value Un vector de valores para los cuales se desea generar una paleta de colores. 
#'              La cantidad de valores determinará cuántos colores se generarán en la paleta.
#' @return Un vector de colores graduales entre verde y azul, con una longitud igual al número de valores de entrada.
#' 
#' @examples
#' # Generar una paleta de colores para 5 valores
#' ColoresGreenBlue(c("A", "B", "C", "D", "E"))
#'
#' @import grDevices
#' @export
ColoresGreenBlue <- function(value) {
  
  # Verificar si la entrada es un vector
  if (!is.vector(value)) {
    stop("El parámetro 'value' debe ser un vector.")
  }
  
  # Calcular la cantidad de valores
  num_valores <- length(value)
  
  # Crear una paleta de colores desde verde "forestgreen" hasta azul oscuro "royalblue4"
  pal <- colorRampPalette(c("forestgreen", "royalblue4"))
  
  # Asignar los colores graduales a los valores
  cols <- pal(num_valores)
  
  # Retornar los colores asignados
  return(cols)
}


# Formatos ----

#' Definir Formato para Números
#'
#' Esta función asigna un formato específico para los números en función de un parámetro de formato. Los formatos disponibles son:
#' "coma" (números con comas como separadores de miles), "numero" (números con dos decimales), 
#' "dinero" (números con símbolo de dólar y comas como separadores de miles), "miles" (números en miles con símbolo de dólar), 
#' y "porcentaje" (números expresados como porcentaje con dos decimales).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles.
#'   - "miles": Números en miles con símbolo "$" y comas como separadores de miles.
#'   - "porcentaje": Números expresados como porcentaje (multiplicados por 100 y con "%" al final).
#' @return Un objeto de tipo función que formatea los números según el formato especificado.
#' 
#' @examples
#' # Formato de número con comas
#' DefinirFormato("coma")(1234567.89)
#' 
#' # Formato de dinero
#' DefinirFormato("dinero")(1234567.89)
#'
#' @import scales
#' @export
DefinirFormato <- function(formato) {
  
  require(scales)
  
  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    label_number(accuracy = 1, scale = 1, suffix = "", prefix = "", big.mark = ",")
  } else if (formato == "numero") {
    label_number(accuracy = 0.01, scale = 1, suffix = "", prefix = "", big.mark = ",")
  } else if (formato == "dinero") {
    label_number(accuracy = 1, scale = 1, suffix = "", prefix = "$", big.mark = ",")
  } else if (formato == "miles") {
    label_number(accuracy = 1, scale = 1 / 1000, suffix = "", prefix = "$", big.mark = ",")
  } else if (formato == "porcentaje") {
    label_number(accuracy = 0.01, scale = 100, suffix = "%", prefix = "", big.mark = ",")
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'miles', 'porcentaje'.")
  }
  
  # Retornar el formato correspondiente
  return(formato)
}

#' Definir Formato de Visualización de Números en D3.js
#'
#' Esta función define el formato de visualización de números en una gráfica utilizando la biblioteca D3.js.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles), 
#' "numero" (números con dos decimales), "dinero" (números con símbolo de dólar y comas como separadores de miles),
#' y "porcentaje" (números expresados como porcentaje con dos decimales).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles, sin decimales.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles, sin decimales.
#'   - "porcentaje": Números expresados como porcentaje con dos decimales.
#' @return Un string que representa el formato para visualizar los números en una gráfica utilizando D3.js.
#' 
#' @examples
#' # Formato con comas
#' FormatoD3("coma")
#' 
#' # Formato de dinero
#' FormatoD3("dinero")
#' 
#' @export
FormatoD3 <- function(formato) {
  
  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    ",.0f"   # Números con comas y sin decimales
  } else if (formato == "numero") {
    ",.2f"   # Números con dos decimales
  } else if (formato == "dinero") {
    "$,.0f"  # Números con símbolo de dólar, con comas como separadores de miles y sin decimales
  } else if (formato == "porcentaje") {
    ",.2%"   # Números expresados como porcentaje con dos decimales
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
  
  # Retornar el formato correspondiente
  return(formato)
}

#' Definir Formato de Visualización de Números en JavaScript
#'
#' Esta función define el formato de visualización de números para ser usado en JavaScript.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles),
#' "numero" (números con dos decimales), "dinero" (números con símbolo de dólar y separadores de miles, sin decimales),
#' y "porcentaje" (números expresados como porcentaje con un decimal).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles y sin decimales.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$", comas como separadores de miles y sin decimales.
#'   - "porcentaje": Números expresados como porcentaje con un decimal.
#' @return Un string que representa el formato correspondiente para usar en JavaScript para la visualización de números.
#' 
#' @examples
#' # Formato con comas
#' FormatoJS("coma")
#' 
#' # Formato de dinero
#' FormatoJS("dinero")
#' 
#' @export
FormatoJS <- function(formato) {
  
  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    'function(d){return d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'  # Números con comas y sin decimales
  } else if (formato == "numero") {
    'function(d){return d.toFixed(2)}'  # Números con dos decimales
  } else if (formato == "dinero") {
    'function(d){return "$" + d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'  # Números con símbolo de dólar y separadores de miles
  } else if (formato == "porcentaje") {
    'function(d){return (d*100).toFixed(1) + "%"}'  # Números expresados como porcentaje con un decimal
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
  
  # Retornar el formato correspondiente
  return(formato)
}

#' Definir el Formato de Números para Handsontable (HOT)
#'
#' Esta función asigna un formato de número adecuado para ser usado en Handsontable, una biblioteca JavaScript.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles), "numero" (números con dos decimales),
#' "dinero" (números con símbolo de dólar y separadores de miles), y "porcentaje" (números expresados como porcentaje).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles.
#'   - "porcentaje": Números expresados como porcentaje con dos decimales.
#' @return Un string que representa el formato correspondiente para usar en Handsontable (HOT).
#' 
#' @examples
#' # Formato con comas
#' FormatoHOT("coma")
#' 
#' # Formato de dinero
#' FormatoHOT("dinero")
#' 
#' @export
FormatoHOT <- function(formato) {
  
  # Asignar el formato adecuado según el tipo especificado
  formato <- if (formato == "coma") {
    "0,0"  # Números con comas como separadores de miles
  } else if (formato == "numero") {
    "0,0.00"  # Números con dos decimales
  } else if (formato == "dinero") {
    "$0,0.00"  # Números con símbolo de dólar y separadores de miles
  } else if (formato == "porcentaje") {
    "%0,0.00"  # Números expresados como porcentaje con dos decimales
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
  
  # Retornar el formato correspondiente
  return(formato)
}


#' Formatear un Número con Opciones de Estilo y Colores
#'
#' Esta función permite formatear un número de acuerdo con un formato específico, 
#' con opciones para aplicar negrita y cambiar el color en función de un umbral 
#' o meta. Se puede utilizar para resaltar visualmente los números en gráficos o tablas.
#'
#' @param x El número o vector de números a formatear.
#' @param formato El formato deseado para el número. Opciones incluyen:
#'   "coma" (con separadores de miles), "numero", "dinero", "miles", "porcentaje".
#' @param negrita Un valor lógico (`TRUE` o `FALSE`) que indica si el número debe aparecer en negrita (por defecto es `TRUE`).
#' @param color El color en formato hexadecimal (por defecto es "#000000"). Si no se proporciona `meta`, este color será usado.
#' @param meta Un valor o vector numérico que representa el umbral o meta para cambiar el color. (opcional)
#' @param prop Un valor lógico (`TRUE` o `FALSE`) que indica si se debe usar una escala de colores proporcional al valor de `x` con respecto a `meta` (por defecto es `TRUE`).
#' @return Un número formateado en HTML con opciones de estilo (negrita, color) y el formato numérico especificado.
#'
#' @examples
#' # Formatear un número en formato "dinero" con color proporcional
#' FormatearNumero(2500, "dinero", negrita = TRUE, meta = 2000)
#'
#' # Formatear un número en formato "porcentaje" sin negrita y con color específico
#' FormatearNumero(0.75, "porcentaje", negrita = FALSE, color = "#00FF00")
#' 
#' @import RColorBrewer
#' @import tidyverse
#' @import shiny
#' @export
FormatearNumero <- function(x, formato, negrita = TRUE, color = "#000000", meta = NA, prop = TRUE) {
  
  # Definir formato numérico usando la función DefinirFormato
  form <- DefinirFormato(formato)
  
  # Definir un rango de meta para la aplicación de colores
  meta2 <- c(-Inf, meta, Inf)
  
  # Crear una paleta de colores basada en el valor de 'prop' (proporcional o no)
  pal <- ifelse(prop,
                colorRampPalette(c("#CB4335", "orange", "#138D75")),
                colorRampPalette(c("#138D75", "orange", "#CB4335"))
  )
  
  # Establecer el número de colores de la paleta
  n <- length(meta) + 1
  colors <- pal(n)
  
  # Determinar el color basado en la relación del número con la meta
  col <- ifelse(is.null(meta), color, colors[sum(!x < meta2)])
  
  # Formatear el número en HTML con el color y negrita si corresponde
  res <- ifelse(negrita, 
                paste0("<span style='font-weight: bold;color:", col, "'>", form(x), "</span>") %>% HTML,
                paste0("<span style='color:", col, "'>", form(x), "</span>") %>% HTML)
  
  return(res)
}

#' Formatear Texto con Opciones de Estilo
#'
#' Esta función permite formatear un texto con diversas opciones de estilo, como 
#' resaltar en negrita, cambiar el color, ajustar el tamaño de la fuente, alinear el texto 
#' y transformar el texto a mayúsculas, minúsculas o capitalización de la primera letra.
#'
#' @param x El texto a formatear.
#' @param negrita Un valor lógico (`TRUE` o `FALSE`) que indica si el texto debe aparecer en negrita (por defecto es `TRUE`).
#' @param color El color en formato hexadecimal (por defecto es "#000000").
#' @param tamano_pct El tamaño de la fuente en porcentaje (por defecto es 1, es decir, tamaño normal).
#' @param alineacion La alineación del texto. Puede ser "left", "center" o "right" (por defecto es "left").
#' @param transform La transformación del texto. Opciones: "none", "capitalize", "uppercase", "lowercase" (por defecto es "capitalize").
#' 
#' @return El texto formateado en HTML con las opciones seleccionadas.
#'
#' @examples
#' # Formatear texto en negrita, color azul y tamaño 120%
#' FormatearTexto("Texto formateado", negrita = TRUE, color = "#0000FF", tamano_pct = 1.2)
#'
#' # Texto en mayúsculas y alineado al centro
#' FormatearTexto("Texto en mayúsculas", transform = "uppercase", alineacion = "center")
#'
#' @import shiny
#' @export
FormatearTexto <- function(x, negrita = TRUE, color = "#000000", tamano_pct = 1, alineacion = "left", transform = "none") {
  
  # Establecer el estilo de negrita
  neg <- paste0("font-weight:", ifelse(negrita, "bold", "normal"), ";")
  
  # Establecer el color del texto
  col <- paste0("color:", color, ";")
  
  # Establecer el tamaño de la fuente en porcentaje
  tam <- paste0("font-size:", tamano_pct * 100, "%;")
  
  # Establecer la alineación del texto
  ali <- paste0("text-align:", alineacion, ";")
  
  # Establecer la transformación del texto (mayúsculas, minúsculas, etc.)
  tra <- paste0("text-transform:", transform, ";")
  
  # Generar el HTML con las opciones de formato
  res <- paste0("<span style='", neg, col, tam, ali, tra, "'>", x, "</span>") %>% HTML
  
  # Retornar el texto formateado
  return(res)
}

#' Obtener el color para un indicador
#'
#' Esta función devuelve el color correspondiente para un indicador, según si es proporcional o no.
#'
#' @param x El valor del indicador.
#' @param prop Un valor lógico que indica si el indicador es proporcional (TRUE) o no (FALSE). 
#'            El valor predeterminado es TRUE.
#' @return El color correspondiente como una cadena hexadecimal.
#' @examples
#' col_kpi(1, TRUE)  # Indicador proporcional positivo (color verde).
#' col_kpi(-1, FALSE)  # Indicador no proporcional negativo (color rojo).
#' @export
col_kpi <- function(x, prop = TRUE){
  # Definir los colores para los casos proporcionales y no proporcionales
  case_when(
    x == 0 ~ "#000000",  # Color negro si el valor es 0
    prop & x > 0 ~ "#0B5345",  # Verde si es proporcional y positivo
    prop & x < 0 ~ "#943126",  # Rojo si es proporcional y negativo
    !prop & x > 0 ~ "#943126",  # Rojo si no es proporcional y positivo
    !prop & x < 0 ~ "#0B5345"  # Verde si no es proporcional y negativo
  )
}

#' Obtener un carácter que represente la dirección de un indicador
#'
#' Esta función devuelve un carácter que representa el crecimiento o decrecimiento de un indicador numérico.
#'
#' @param x El número del cual se desea obtener el carácter representativo.
#' @return Un carácter: '▲' para valores positivos, '▼' para negativos y '▬' para valores iguales a 0.
#' @examples
#' chr_kpi(1)  # Devuelve "▲" para crecimiento positivo.
#' chr_kpi(-1)  # Devuelve "▼" para decrecimiento negativo.
#' @export
chr_kpi <- function(x){
  case_when(
    x == 0 ~ "▬",  # Neutral si es 0
    x > 0 ~ "▲",   # Flecha hacia arriba si es positivo
    x < 0 ~ "▼"    # Flecha hacia abajo si es negativo
  )
}

# Elementos HTML ----
#' CajaIco - Genera una caja con un ícono y texto en un entorno de Shiny
#'
#' Esta función genera una caja con un ícono y texto en un entorno de Shiny.
#'
#' @param texto El texto que se mostrará en la caja.
#' @param icono El ícono que se mostrará en la caja.
#' @param col_fondo El color de fondo de la caja.
#' @param col_letra (por defecto #17202A) El color del texto.
#' @param col_icono (por defecto #000000) El color del ícono.
#' @return Una caja con el ícono y el texto especificados.
#'
#' @examples
#' CajaIco("Ejemplo de caja con ícono", "fas fa-chart-bar", "#3498DB", "#FFFFFF", "#FFFFFF")
#'
CajaIco <- function(texto, icono, col_fondo, col_letra = "#17202A", col_icono = "#000000") {
  # Cargando la librería colorspace
  require(colorspace)
  
  # Definición de estilos para la caja
  s_caj <- paste0("display:block;background:", col_fondo, ";min-height:120px;height: fit-content;width:100%;border-radius:10px;box-shadow:1px 1px 2px", darken(col_fondo, 0.1), ";")
  
  # Definición de estilos para el ícono
  s_ico <- paste0("position:absolute;display:block;float:left;height:100%;width:100%;text-align:left; font-size:80px;color:", adjust_transparency(col_icono, 0.05), ";background:transparent;z-index:1")
  
  # Definición de estilos para el contenido
  s_con <- "position:absolute;z-index:3;margin:0px;padding:5px 10px;margin-top:20px;margin-left:20px;background:transparent;"
  
  # Definición de estilos para el texto
  s_tex <- paste0("color:", col_letra, ";text-align:left;vertical-align:text-top;font-size:20px;")
  
  # Generación de la caja utilizando las especificaciones de estilo definidas
  column(12,
         div(style = s_caj,
             column(6, style = s_ico, icon(icono)), # Incorporando el ícono a la caja
             column(6, style = s_con,
                    tags$span(style = s_tex, texto)) # Incorporando el texto a la caja
         )
  )
}

#' CajaIma - Genera una caja con una imagen y texto en un entorno de Shiny
#'
#' Esta función genera una caja con una imagen y texto en un entorno de Shiny.
#'
#' @param texto El texto que se mostrará en la caja.
#' @param imagen La ruta de la imagen que se mostrará en la caja.
#' @param col_fondo El color de fondo de la caja.
#' @param col_letra (por defecto #17202A) El color del texto.
#' @param col_icono (por defecto #000000) El color de la imagen.
#' @return Una caja con la imagen y el texto especificados.
#'
#' @examples
#' CajaIma("Ejemplo de caja con imagen", "ruta_de_la_imagen.jpg", "#3498DB", "#FFFFFF", "#FFFFFF")
#'
CajaIma <- function(texto, imagen, col_fondo, col_letra = "#17202A", col_icono = "#000000") {
  # Cargando la librería colorspace
  require(colorspace)
  
  # Definición de estilos para la caja
  s_caj <- paste0("display:block;background:", col_fondo, ";min-height:120px;width:100%;border-radius:10px;box-shadow:1px 1px 2px", darken(col_fondo, 0.1), ";")
  
  # Definición de estilos para la imagen
  s_ima <- "position:absolute;display:block;z-index:1;opacity: 0.10;"
  
  # Definición de estilos para el contenido
  s_con <- "position:absolute;z-index:3;margin:0px;padding:5px 10px;margin-top:20px;margin-left:20px;background:transparent;"
  
  # Definición de estilos para el texto
  s_tex <- paste0("color:", col_letra, ";text-align:left;vertical-align:text-top;font-size:20px;")
  
  # Generación de la caja utilizando las especificaciones de estilo definidas
  column(12,
         div(style = s_caj,
             column(6, style = s_ima, 
                    HTML(paste("<img src=", imagen, "alt='Imagen'>"))
             ),
             column(6, style = s_con,
                    tags$span(style = s_tex, texto)
             )
         )
  )
}
# Gráficas ----
#' Imprimir un Indicador de Tipo Gauge
#'
#' Esta función crea un gráfico de tipo gauge (indicador) usando la librería `plotly`.
#' El indicador muestra un valor numérico dentro de un rango, con colores que varían según el valor,
#' y proporciona un formato personalizado para los números.
#'
#' @param val El valor actual a mostrar en el gauge.
#' @param limites Un vector con dos valores que definen los límites en los cuales el valor cambia de color.
#' @param rango Un vector con dos valores que definen el rango del gauge.
#' @param Directo Un valor lógico (`TRUE` o `FALSE`) que indica si el rango y los límites deben ser utilizados en orden directo o invertido.
#' @param formato El formato deseado para los números, utilizando la función `Formatod3` para personalizar la visualización.
#' 
#' @return Un gráfico interactivo de tipo gauge que muestra el valor dentro de un rango, con formato de número personalizado.
#'
#' @examples
#' # Crear un gauge con valor 75, límites [50, 100], rango [0, 100], y formato de número personalizado
#' ImprimirGauge(75, c(50, 100), c(0, 100), Directo = TRUE, formato = "numero")
#'
#' @import plotly
#' @export
ImprimirGauge <- function(val, limites, rango, Directo, formato) {
  
  # Definir el formato de número usando la función Formatod3
  Form = Formatod3(formato)
  
  # Ajustar los límites y el rango en función de la opción 'Directo'
  limites <- if(Directo) limites else rev(limites)
  rango <- if(Directo) rango else rev(rango)
  
  # Crear el gráfico gauge utilizando plot_ly
  plot_ly(
    value = val,
    title = list(text = ""),
    type = "indicator",
    mode = "gauge+number",
    number = list(valueformat = Form),
    gauge = list(
      axis = list(
        range = list(NULL, max(rango)), 
        tickformat = Form
      ),
      bar = list(color = "black"),
      steps = list(
        list(range = c(rango[1], limites[1]), color = "#45B39D"),
        list(range = c(limites[1], limites[2]), color = "#EB984E"),
        list(range = c(limites[2], rango[2]), color = "#CD6155")
      )
    )
  ) %>% 
    layout(
      autosize = TRUE, 
      margin = list(t = 0, b = 0, l = 0, r = 0),  # Definir márgenes en blanco
      xaxis = list(tickformat = Form, title = ""),
      yaxis = list(tickformat = Form, title = "")
    ) %>% 
    config(displayModeBar = FALSE)  # Ocultar la barra de herramientas de la gráfica
}
#' Imprimir un Gráfico de Tipo Anillo (Pie) con Colores Personalizados
#'
#' Esta función crea un gráfico de tipo anillo (pie chart con agujero) a partir de un conjunto de datos.
#' El gráfico puede mostrar la suma o el número de ocurrencias de una variable, con colores personalizables
#' y etiquetas reordenadas según la variable medida.
#'
#' @param data El conjunto de datos a utilizar para generar el gráfico.
#' @param var_label El nombre de la variable a usar como etiqueta en el gráfico.
#' @param var_medida El nombre de la variable numérica sobre la cual se calcula la suma o el número de ocurrencias.
#' @param funcion La función a aplicar sobre `var_medida`. Puede ser "sum" o "n" para contar ocurrencias.
#' @param fun_color La función de color a usar. Puede ser "ColoresPlots" o "ColoresPlots2".
#' 
#' @return Un gráfico de tipo pie (anillo) con los valores representados y colores personalizados.
#' 
#' @examples
#' # Crear un gráfico de anillo con la variable 'Categoria' y aplicar la función 'sum' sobre la variable 'Valor'.
#' ImprimirAnillo(data, var_label = "Categoria", var_medida = "Valor", funcion = "sum", fun_color = "ColoresPlots")
#'
#' @import dplyr
#' @import plotly
#' @importFrom rlang parse_expr
#' @importFrom forcats fct_reorder
#' @export
ImprimirAnillo <- function(data, var_label, var_medida, funcion = "sum", fun_color) {
  
  # Función auxiliar para definir la visibilidad del texto en el gráfico
  ValoresPlots <- function(value) {
    total = sum(value)
    text <- ifelse(value / total <= 0.05, 'none', 'auto')
    return(text)
  }
  
  # Agrupar los datos y aplicar la función adecuada (n o sum)
  if (funcion == "n") {
    aux1 <- data %>% 
      group_by_at(var_label) %>% 
      summarise(Var = n()) %>% 
      mutate(Lab = !!parse_expr(var_label))
  } else {
    aux1 <- data %>% 
      group_by_at(var_label) %>% 
      summarise(Var = !!parse_expr(paste0(funcion, "(", var_medida, ", na.rm = TRUE)"))) %>% 
      mutate(Lab = !!parse_expr(var_label))
  }
  
  # Ajustar las etiquetas para que se ajusten al ancho y agregar saltos de línea
  aux1$Lab <- sapply(aux1$Lab, FUN = function(x) {paste(strwrap(x, width = 30), collapse = "<br>")})
  
  # Reordenar las etiquetas en función del valor de la variable medida (Var)
  aux1$Lab <- fct_reorder(aux1$Lab, aux1$Var, max)
  cols <- as.character(aux1$Lab)
  # Crear el gráfico usando la función de color seleccionada
  
  if (fun_color == "ColoresRacafe") {
    plot_ly(aux1) %>%
      add_pie(labels = ~Lab, values = ~Var, sort = TRUE,
              type = 'pie', hole = 0.4, textposition = ~ValoresPlots(Var),
              marker = list(line = list(width = 2),
                            colors = ~ColoresRacafe(cols)))  %>%
      layout(margin = list(t = 20, b = 20, l = 20, r = 20),  # Ajustar márgenes
             legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.07, 
                           font = list(size = 9, color = "black"))) %>%
      config(displayModeBar = FALSE)  # Ocultar la barra de herramientas
  } else if (fun_color == "ColoresGreenBlue") {
    plot_ly(aux1) %>%
      add_pie(labels = ~Lab, values = ~Var, sort = TRUE,
              type = 'pie', hole = 0.4, textposition = ~ValoresPlots(Var),
              marker = list(line = list(width = 2),
                            colors = ~ColoresGreenBlue(cols)))  %>%
      layout(margin = list(t = 20, b = 20, l = 20, r = 20),  # Ajustar márgenes
             legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.07, 
                           font = list(size = 9, color = "black"))) %>%
      config(displayModeBar = FALSE)  # Ocultar la barra de herramientas
  }
}

#' Imprimir Serie de Tiempo con o sin Pronóstico
#'
#' Esta función genera un gráfico interactivo con una serie temporal utilizando `plotly`. 
#' Si se activa el parámetro `pronostico`, se realiza un pronóstico utilizando el modelo `prophet`.
#'
#' @param data Dataframe con la información a graficar.
#' @param fecha Nombre de la columna que contiene las fechas. Debe ingresar el nombre en comillas.
#' @param vars Nombres de las columnas a graficar. Debe ingresarlas en un vector de caracteres.
#' @param per_omit Número de observaciones a omitir desde el principio. (Predeterminado: 3).
#' @param pronostico Indicador lógico para realizar el pronóstico con `prophet` (Predeterminado: `FALSE`).
#' @param periodos Número de periodos futuros a predecir (si `pronostico` es `TRUE`).
#' @param IC Intervalo de confianza del pronóstico (Predeterminado: 0.95).
#' @param colores Vector de colores a utilizar en el gráfico. Debe tener el mismo tamaño que `vars`.
#' @param formato Formato para el eje y y los textos (Opciones: "numero").
#' @param titulo Título principal del gráfico.
#' @param titeje Título del eje y del gráfico.
#' 
#' @return Un gráfico interactivo de serie temporal generado por `plotly`. 
#' Si `pronostico` es `TRUE`, también se mostrará el pronóstico con intervalos de confianza.
#' 
#' @import plotly
#' @import prophet
#' @import dplyr
#' @import rlang
#' @import scales
#' @export
#' 
#' @examples
#' # Generar gráfico sin pronóstico
#' ImprimirSerieTiempo(data, fecha = "Fecha", vars = c("Ventas", "Costo"), colores = c("blue", "red"), titulo = "Ventas y Costo", titeje = "Valor")
#' 
#' # Generar gráfico con pronóstico
#' ImprimirSerieTiempo(data, fecha = "Fecha", vars = c("Ventas"), pronostico = TRUE, colores = c("blue"), titulo = "Ventas con Pronóstico", titeje = "Ventas")
ImprimirSerieTiempo <- function(data, fecha, vars, per_omit = 3, pronostico = FALSE, periodos = 6, IC = 0.95, 
                                colores, formato = "numero", titulo, titeje) {
  
  # Requerir librerías necesarias
  require(plotly)
  require(prophet)
  require(rlang)
  require(scales)
  require(dplyr)
  
  # Preprocesamiento: filtrar fechas y omitir las primeras 'per_omit' filas
  aux0 <- data %>%  
    mutate(Fecha = !!parse_expr(fecha)) %>% 
    arrange(Fecha) %>% 
    filter(!is.na(Fecha), row_number() > per_omit)
  
  # Completar fechas faltantes hasta el primer día del mes actual
  aux1 <- aux0 %>% 
    filter(Fecha <= PrimerDia(Sys.Date())) %>% 
    complete(Fecha = seq.Date(as.Date(min(Fecha)), as.Date(max(Fecha)), by = "month")) %>% 
    mutate_at(vars, list(~ifelse(is.na(.), 0, .))) %>% 
    select_at(c("Fecha", vars))
  
  # Asignar nombres de colores a las variables
  names(colores) <- vars
  
  # Definir el formato para los ejes y las etiquetas
  form1 <- DefinirFormato(formato)
  form2 <- FormatoD3(formato)
  
  # Si el parámetro 'pronostico' es verdadero, realizar el pronóstico utilizando prophet
  if (pronostico) {
    
    # Pronóstico para cada variable en 'vars'
    lista <- names(aux1)[-1]
    pron <- do.call("bind_rows", lapply(lista, function(var) {
      
      aux2 <- aux1 %>%
        select(ds = Fecha, y = all_of(var))  # Preparar datos para prophet
      
      # Ajustar modelo prophet
      m <- prophet(aux2, interval.width = IC, weekly.seasonality = FALSE, daily.seasonality = FALSE, yearly.seasonality = TRUE, growth = "linear")
      future <- make_future_dataframe(m, periods = periodos, freq = "month", include_history = TRUE)
      
      forecast <- predict(m, future) %>%
        mutate(Source = var) %>%
        left_join(aux2, by = "ds") %>% 
        select(Fecha = ds, Source, Observado = y, LI = yhat_lower, Pronostico = yhat, LS = yhat_upper)
      
      # Calcular el error MAPE
      aux_mape <- abs((forecast$Observado - forecast$Pronostico) / forecast$Observado)
      mape <- mean(aux_mape[!is.infinite(aux_mape)], na.rm = TRUE)
      print(paste("Variable:", var, "MAPE:", mape))
      
      # Si el MAPE es mayor al 20% o hay menos de 30 puntos, eliminar las predicciones
      if (nrow(aux2) < 30 | mape > 0.2) {
        forecast <- forecast %>%
          mutate_at(c("LI", "Pronostico", "LS"), list(~as.numeric(NA)))
      } else {
        forecast <- forecast %>%
          mutate_at(c("LI", "Pronostico", "LS"), list(~ifelse(!is.na(Observado), as.numeric(NA), .)))
      }
      
      # Unir los datos originales con el pronóstico
      forecast <- forecast %>%
        full_join(aux0 %>% select(Fecha, all_of(var)), by = join_by(Fecha)) %>% 
        select(-Observado) %>% 
        rename(Observado = all_of(var))
      
      return(forecast)
      
    }))
    
    # Crear gráfico de pronóstico con plotly
    plot_ly(pron, x = ~Fecha) %>% 
      add_trace(y = ~Observado, color = ~Source, type = "scatter", mode = "lines+markers",
                marker = list(size = 5), colors = colores,
                hoverinfo = "text", hoverlabel = list(pron = "left"),
                hovertext = paste0("<b>", format(pron$Fecha, "%B del %Y"), "</b>",
                                   "<br>", str_to_sentence(pron$Source), ": ", form1(pron$Observado))) %>% 
      add_ribbons(ymin = ~LI, ymax = ~LS, color = ~Source, showlegend = FALSE, alpha = 0.3,
                  hoverinfo = "text", hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", format(pron$Fecha, "%B del %Y"), "</b>",
                                     "<br>", "Límite inferior:  ", form1(pron$LI),
                                     "<br>", "Estimado:         ", form1(pron$Pronostico),
                                     "<br>", "Límite superior: ", form1(pron$LS))) %>% 
      add_lines(y = ~Pronostico, color = ~Source, type = "scatter", mode = "lines", name = "Forecast",
                line = list(dash = 'dot'), hoverinfo = "none", showlegend = FALSE) %>% 
      layout(title = list(text = titulo, font = list(family = "Arial, sans-serif", size = 16, color = "black")),
             annotations = list(text = text_mape, y = 30, x = min(pron$Fecha), showarrow = FALSE),
             xaxis = list(title = ""),
             yaxis = list(title = titeje, tickformat = form2, visible = TRUE, rangemode = "tozero",
                          title = list(text = "", font = list(family = "Arial, sans-serif", size = 16, color = "black")), 
                          tickfont = list(family = "Arial, sans-serif", size = 14, color = "black")),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.05)) %>% 
      config(locale = "es", displayModeBar = FALSE)
    
  } else {
    
    # Gráfico sin pronóstico, solo con los datos reales
    pron <- aux1 %>% 
      pivot_longer(all_of(vars), names_to = "Source", values_to = "Real")
    
    plot_ly(pron, x = ~Fecha) %>% 
      add_trace(y = ~Real, color = ~Source, type = "scatter", mode = "lines+markers",
                marker = list(size = 5), colors = colores,
                hoverinfo = "text", hoverlabel = list(pron = "left"),
                hovertext = paste0("<b>", format(pron$Fecha, "%B del %Y"), "</b>",
                                   "<br>", str_to_sentence(pron$Source), ": ", form1(pron$Real))) %>% 
      layout(title = list(text = titulo, font = list(family = "Arial, sans-serif", size = 16, color = "black")),
             xaxis = list(title = ""),
             yaxis = list(title = titeje, tickformat = form2, visible = TRUE, rangemode = "tozero",
                          title = list(text = "", font = list(family = "Arial, sans-serif", size = 16, color = "black")), 
                          tickfont = list(family = "Arial, sans-serif", size = 14, color = "black")),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.05)) %>% 
      config(locale = "es", displayModeBar = FALSE)
  }
}
