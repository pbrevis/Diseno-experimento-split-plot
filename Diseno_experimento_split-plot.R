##Cargando paquetes de R
library(agricolae)
library(dplyr)
library(gt)
library(RColorBrewer)
library(viridis)

# Limpiando el ambiente global
rm(list = ls(all = TRUE)) 
graphics.off()

################################################################################
##
## Definir parámetros para el diseño experimental (Split-Plot)
##
################################################################################

mainplot_levels <- c("A1", "A2", "A3", "A4") # tratamientos
subplot_levels <- c("B1", "B2", "B3") # subtratamientos
block_no <- 5 # blocks o repeticiones

# Cuenta número de tratamientos
main_no <- n_distinct(mainplot_levels)

# Creando el diseño
outdesign <- design.split(mainplot_levels, subplot_levels, r= block_no,
                          serie=2,
                          seed=45,
                          kinds ="Super-Duper") # método para randomizar

# creando "libro de campo" con el diseño del exper.
field_book <- outdesign$book 


# Escribir libro de campo en archivo de texto
write.table(field_book,"field_book.txt", row.names=FALSE, sep="\t")

################################################################################
##
## Creando una tabla 'gt' con los resultados del diseño
##
################################################################################

# Seleccionando colores para tabla 'gt'. Un color por tratamiento (main plot)
viridis(main_no)

# Convirtiendo todas las variables del data frame a factor
col_names <- c('plots', 'subplot_levels')


field_book[, col_names] <- lapply(field_book[, col_names], factor)

str(field_book)
  

# Generando la tabla 'gt'

field_book |> # data frame
  
  mutate(block = as.character(block),
         block = paste0('Block: ', block)) |> # Agregando string a variable block
  
  
  gt(groupname_col = 'block') |> # agrupando datos por block
  
  cols_move(columns = splots, after = mainplot_levels) |> # reordenando columnas
  
  
  # Asignando colores a mainplots (un tab_style por block)
  
  tab_style(
    style = list(
      cell_fill(color = "#440154FF"), # color hexadecimal generado con viridis(n)
      cell_text(color = "white")
    ),
    locations = cells_body(columns = c('plots', 'splots'),
                           rows = mainplot_levels == 'A1')
  ) |>  
  
  tab_style(
    style = list(
      cell_fill(color = "#31688EFF"), # color hexadecimal generado con viridis(n)
      cell_text(color = "white")
    ),
    locations = cells_body(columns = c('plots', 'splots'),
                           rows = mainplot_levels == 'A2')
  ) |>
  
  
  tab_style(
    style = list(
      cell_fill(color = "#35B779FF"), # color hexadecimal generado con viridis(n)
      cell_text(color = "white")
    ),
    locations = cells_body(columns = c('plots', 'splots'),
                           rows = mainplot_levels == 'A3')
  ) |>
  
  tab_style(
    style = list(
      cell_fill(color = "#FDE725FF"), # color hexadecimal generado con viridis(n)
      cell_text(color = "white")
    ),
    locations = cells_body(columns = c('plots', 'splots'),
                           rows = mainplot_levels == 'A4')
  ) |>
  
  
  tab_header(title = 'Esquema del Diseño Experimental Split-Plot',
             subtitle = 'con 4 tratamientos, 3 sub-tratamientos y 5 repeticiones'
  ) |> 
  
  opt_stylize(style = 1) |> # seleccionar estilo

  gtsave("tabla_1.html") # guardar tabla como archivo html




