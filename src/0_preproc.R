library(tidyverse)
library(lubridate)

data_repo <- read_csv(gsheet::gsheet2text("https://docs.google.com/spreadsheets/d/1TUTag7Majqhn5noRLLMUJ6SFJ0Phwlo-Oc1T59uSZCE/edit#gid=0"
                         , format='csv'),
              skip=1)


data_dipu <- read_csv(gsheet::gsheet2text("https://docs.google.com/spreadsheets/d/1SU7w_mLgjO3EidpA6KZitGpD4-ZQYyWow34NlB5g-Uc/edit#gid=531185343"
                                           , format='csv'),
                       skip=1)  %>%
        select(-X1) %>%
        rename( nombre = Nombre,
                genero = Genero,
                provincia = Distrito,
                bloque = Bloque,
                poroteo = AbortoLegal,
                fuente_poroteo = FuenteAbortoLegal,
                fecha_nac = `Fecha de nacimiento`,
                votacion_final0 = `Voto final`)  %>%
        mutate(nombre =  trimws(iconv(nombre, to='ASCII//TRANSLIT'))) %>%
        mutate(nombre = trimws(str_extract(str_to_lower(nombre), '([A-Za-z]+),\\s+([A-Za-z]+)'))) %>%
        mutate(nombre = case_when(
                nombre == 'guevara, francisco' ~ 'olivera, alejandro',
                nombre == 'carnaghi, guillermo' ~ 'carnaghi, oscar',
                nombre == 'carrizo, maria' ~ 'carrizo, soledad',
                nombre == 'gutierrez, ramiro' ~ 'gutierrez, carlos',
                nombre == 'joury, maria' ~ 'joury, mercedes',
                nombre == 'lopez, maria' ~ 'lopez, jimena',
                nombre == 'marziotta, gisela' ~ 'marziotta, maria',
                nombre == 'sand, nancy' ~ 'giorasi, nancy',
                nombre == 'musac, juan' ~ 'martin, juan',
                nombre =='tailhade, rodolfo' ~ 'tailhade, luis',
                nombre == 'vallejos, fernanda' ~ 'vallejos, maria',
                nombre == 'vessvessian, paola' ~ 'vessvessian, marcela',
                nombre == 'yutrovic, carolina' ~ 'yutrovic, ines',
                nombre == 'zottos, andres' ~ 'zottos, miguel',
                TRUE ~ nombre),
               apellido = trimws(str_extract(str_to_lower(nombre), '([A-Za-z]+)')),
               bloque = str_to_lower(trimws(iconv(bloque, to='ASCII//TRANSLIT'))),
               provincia = trimws(str_to_lower(iconv(provincia, to='ASCII//TRANSLIT')))) 

        
        
votacion_final <-  read_csv(gsheet::gsheet2text("https://doc.google.com/spreadsheets/d/1aeCNZ-yv_WioLSK4-pK0Rp7LdxJlMJ7FotQegR_1nuo/edit#gid=48207088"
                                           , format='csv')) %>%
        rename(votacion_final1 = `¿CÓMO VOTÓ?`,
               nombre = DIPUTADO,
               bloque = BLOQUE,
               provincia = PROVINCIA) %>%
        mutate(nombre =  trimws(iconv(nombre, to='ASCII//TRANSLIT'))) %>%
        mutate(nombre = trimws(str_extract(str_to_lower(nombre), '([A-Za-z]+),\\s+([A-Za-z]+)')),
               apellido = trimws(str_extract(str_to_lower(nombre), '([A-Za-z]+)')),
               bloque = str_to_lower(trimws(iconv(bloque, to='ASCII//TRANSLIT'))),
               provincia = trimws(str_to_lower(iconv(provincia, to='ASCII//TRANSLIT'))),
               provincia = trimws(str_replace(provincia, 'c.a.b.a.', 'ciudad autonoma de buenos aires'))
        )


final <- votacion_final %>%
                left_join( data_dipu %>% select(-bloque) )


final <- final %>%
        mutate(poroteo = str_to_lower(poroteo),
               votacion_final1 = str_to_lower(votacion_final1),
               votacion_final0 = str_to_lower(votacion_final1),
               fecha_nac = dmy(fecha_nac),
               edad = year(today()) - year(fecha_nac),
        )

votacion_final %>%
        group_by(votacion_final1) %>%
        count()

final %>%
        drop_na(votacion_final1) %>%
        arrange(nombre) %>%
        distinct(nombre, provincia, .keep_all = TRUE) %>%
        group_by(votacion_final1) %>%
        count()

write_csv(final, './data/datos_finales.csv')
