library(tidyverse)
library(sf)
library(tmap)
#descargamos la tabla del INE con las tasas de paro según comunidad y sexo
fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4966.csv?nocab=1" #-pongo el link de la página del INE
curl::curl_download(my_url, "./pruebas/map.csv")
map <- rio::import("./pruebas/map.csv")
#- Importamos la tabla al Global
map <- rio::import("./pruebas/map.csv") #- rio::import() funciona!! 
str(map) #- PERO realmente funciona??
#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(map) #- están bien pero ... aún así quiero recordar janitor::clean_names()
map <- janitor::clean_names(map) 
names(map)

#- Pb con la tasa de paro (en la v. total) tiene , en el decimal
map <- map %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(map)
map <- map %>% mutate(total = as.numeric(total)) #- un NA en Melilla 2016
#- OK, ya (casi) tenemos los datos correctamente leídos/interpretados
map <- map %>% rename(tasa_paro = total)
map <- map %>% rename(CCAA = comunidades_y_ciudades_autonomas)
#- 2) La v "CCAA" tiene el código y el nombre
# ademas, en provincias estan las 16 CC.AA y el "Total nacional"
map <- map %>% 
  mutate(CCAA = ifelse(CCAA == "Total Nacional",
                       paste("00", CCAA),
                       CCAA))
map <- map %>%  
  tidyr::separate(CCAA, sep = " ", 
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge") 
#- vamos a hacer ahora una coropleta con la tasa de paro de cada CA
df_ccaa <- map %>%
  filter(edad == "Total") %>% 
  filter(ine_ccaa.n != "Total Nacional") %>% 
  filter(sexo == "Ambos sexos") %>% 
  select(-c(edad, sexo)) 

#- me quedo con 2019 que es el año en el que estamos analizando todo
df_ccaa_2019 <- df_ccaa %>% filter(periodo == 2019)

#- cargo geometrías de provincias
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(df_geo_prov, max.plot = 1)

#- podemos ver q la última columna de df_geo_prov tiene las "geometrías"
names(df_geo_prov)
head(df_geo_prov)

#- me quedo con las vv. q me interesan
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)
names(df_geo_prov)

#- podemos "agregar" geometrías
df_geo_ccaa <- df_geo_prov %>% 
  group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
plot(df_geo_ccaa, max.plot = 1)
names(df_geo_ccaa)

# df_geo_esp <- df_geo_ccaa %>% group_by(1) %>% summarise()
# plot(df_geo_esp, max.plot = 1)


#- junto geometría (df_geo_ccaa) con datos INE (df_ccaa_2021)
#- las geometrías a la izquierda
df_ok <- left_join(df_geo_ccaa, df_ccaa_2019, by = c("ine_ccaa" = "ine_ccaa"))
names(df_ok)
#mapa con números rosa
#- para ello calculo centroides (!!!)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))
names(df_geo_ccaa)
#- vuelvo a juntar datos EPA con geometrías (q ahora incorporan los centroides)
df_ok <- left_join(df_geo_ccaa, df_ccaa_2019, by = c("ine_ccaa" = "ine_ccaa"))

p <- ggplot() + 
  geom_sf(data = df_ok, 
          aes(geometry = geometry), fill = "pink", 
          color = "black", size = 0.09) +
  geom_text(data = df_ok, aes(x = X, y = Y, label = tasa_paro),
            color = "black",  
            check_overlap = TRUE, size = 3)

p

#- luego ya hay que tunearlo (un poco)
p + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Tasa de desempleo (%)", 
       subtitle = "(año 2019)", 
       caption = "Datos provenientes del INE")


# Seleccionar la comunidad autónoma que deseas señalar
df_ok_seleccionada <- df_ok %>%
  filter(ine_ccaa == "10")

# Crear el gráfico con la comunidad autónoma seleccionada en rojo
p <- ggplot() + 
  geom_sf(data = df_ok, 
          aes(geometry = geometry), fill = "pink", 
          color = "black", size = 0.09) +
  geom_sf(data = df_ok_seleccionada, 
          aes(geometry = geometry), fill = "red", 
          color = "black", size = 0.09) +
  geom_text(data = df_ok, aes(x = X, y = Y, label = tasa_paro),
            color = "black",  
            check_overlap = TRUE, size = 3)

p

#- luego ya hay que tunearlo (un poco)
p + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Tasa de desempleo (%)", 
       subtitle = "(año 2019)", 
       caption = "Datos provenientes del INE")

