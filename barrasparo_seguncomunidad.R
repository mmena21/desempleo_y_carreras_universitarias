library(tidyverse)
#descargamos la tabla del INE con las tasas de paro según comunidad y sexo
fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4966.csv?nocab=1" #-pongo el link de la página del INE
curl::curl_download(my_url, "./pruebas/face.csv")
face <- rio::import("./pruebas/face.csv")
#- Importamos la tabla al Global
face <- rio::import("./pruebas/face.csv") #- rio::import() funciona!! 
str(face) #- PERO realmente funciona??
#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(face) #- están bien pero ... aún así quiero recordar janitor::clean_names()
face <- janitor::clean_names(face) 
names(face)

#- Pb con la tasa de paro (en la v. total) tiene , en el decimal
face <- face %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(face)
face <- face %>% mutate(total = as.numeric(total)) #- un NA en Melilla 2016
#- OK, ya (casi) tenemos los datos correctamente leídos/interpretados
face <- face %>% rename(tasa_paro = total)
face <- face %>% rename(CCAA = comunidades_y_ciudades_autonomas)
#- 2) La v "CCAA" tiene el código y el nombre
# ademas, en provincias estan las 16 CC.AA y el "Total nacional"
face <- face %>% 
  mutate(CCAA = ifelse(CCAA == "Total Nacional",
                       paste("00", CCAA),
                       CCAA))
face <- face %>%  
  tidyr::separate(CCAA, sep = " ", 
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge") 

#gráfico barras ordenado según comunidad
df <- face %>% filter(edad == "De 25 a 54 años")%>% arrange(desc(tasa_paro))%>% group_by(ine_ccaa.n) %>% select(sexo, ine_ccaa.n,edad, periodo, tasa_paro) %>% filter(periodo == 2019)%>% filter(sexo == "Ambos sexos")%>%filter(ine_ccaa.n != "Total Nacional")
max_total <- max(df$tasa_paro) 
p2 <- ggplot(df, aes(x = tasa_paro, y = reorder(ine_ccaa.n, tasa_paro, FUN = desc))) +  
  geom_bar(stat = "identity" , fill = "#FFC311", position = "dodge", color ="white", alpha = 0.5, size = 0.5) + 
  labs(title = "Tasa de paro en las diferentes Comunidades", caption = "Fuente: INE",x="Tasa de paro año 2019", y="") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_excel_new()
p2
