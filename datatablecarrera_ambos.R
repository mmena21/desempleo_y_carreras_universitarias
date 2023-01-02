library(tidyverse)

fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://ine.es/jaxi/files/tpx/es/csv_bd/48895.csv?nocab=1" #-pongo el link de la página del INE
curl::curl_download(my_url, "./pruebas/segunestudios.csv")
segunestudios <- rio::import("./pruebas/segunestudios.csv")

#arreglo los datos quitando las comas
segunestudios <- read_delim("pruebas/segunestudios.csv", 
                            delim = "\t", escape_double = FALSE, 
                            col_types = cols(Total = col_number()),  
                            locale = locale(date_names = "es", decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)
#-Arreglo los nombres
names(segunestudios)
segunestudios <- janitor::clean_names(segunestudios)
zz <- pjpv.curso.R.2022::pjp_valores_unicos(segunestudios)

#-Arreglo función
zz <- segunestudios %>% filter(tasas_de_actividad_empleo_y_paro == "Tasa de paro") %>% filter(titulacion %in% c("Pedagogía", "Educación infantil", "Educación primaria", "Bellas artes", "Conservación y restauración", "Arqueología", "Historia", "Filosofía", "Traducción e interpretación", "Literatura", "Economía",  "Psicología", "Criminología", "Geografía","Periodismo","Finanzas y contabilidad","Administración y empresa","Gestión y administración pública","Derecho","Biología","Bioquímica","Biotecnología","Biomedicina","Química","Ciencias del mar","Geología","Física","Matemáticas","Estadística","Informática","Ingeniería de telecomunicación","Ingeniería mecánica", "Ingeniería aeronáutica","Arquitectura y Urbanismo y paisajismo","Ingeniería geomática, topografía y cartografía","Arquitectura técnica","Veterinaria","Odontología","Medicina","Enfermería","Ingeniería biomédica y de la salud","Óptica y optometría","Fisioterapia"))
zz_1 <- zz %>% group_by(titulacion) %>% arrange(desc(total)) %>% filter(sexo == "Hombres")

colnames(zz_1)[2] <- "Titulación"

colnames(zz_1)[3] <- "Tasa de paro"

colnames(zz_1)[4] <- "Porcentaje"
#Datatable para ambos sexos 
zz_3  <- zz %>% group_by(titulacion) %>% arrange(desc(total)) %>% filter(sexo == "Ambos sexos")

colnames(zz_3)[2] <- "Titulación"

colnames(zz_3)[3] <- "Tasa de paro"

colnames(zz_3)[4] <- "Porcentaje"

library(DT)
datatable(zz_3)