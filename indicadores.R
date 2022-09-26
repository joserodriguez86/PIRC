pacman::p_load(tidyverse, haven, xlsx, sf, ggthemes)

rm(list = ls())
load("bases/base_hogar_imputados.RData")
# base_hogar <- read_sav("bases/base_hogar.sav")
base_componentes <- read_sav("bases/base_componentes.sav", encoding = "latin1")

# Arreglos base -------------------
base_componentes <- base_componentes %>% 
  # filter(ENTREVISTADO == 1) %>% 
  select(CUEST, NUM, M2.3, M2.4, M2.5, M2.13, ENTREVISTADO)

base_componentes <- base_componentes %>% 
  left_join(base_hogar, by = "CUEST")

base_hogar <- base_componentes %>% 
  filter(ENTREVISTADO == 1)

base_hogar <- base_hogar %>% 
  mutate(rama = case_when(CAES_letra %in% c("A", "B") ~ "Primarias + extractivas",
                          CAES_letra %in% c("C", "D", "E") ~ "Manufactura + energia-agua-gas",
                          CAES_letra %in% c("F") ~ "Construcción",
                          CAES_letra %in% c("G", "I") ~ "Comercio + hotelería",
                          CAES_letra %in% c("H") ~ "Transporte",
                          CAES_letra %in% c("O") & CAES_num != 8402 ~ "Adm. pública",
                          CAES_letra %in% c("O") & CAES_num == 8402 ~ "FF.AA y policía",
                          CAES_letra %in% c("J", "K", "L", "M", "N", "P", "Q", "R",
                                            "S", "T", "U") ~ "Servicios",
                          TRUE ~ NA_character_),
         sexo_f = factor(SEXO, labels = c("Varón", "Mujer")),
         edad_grupo = car::recode(as.numeric(M2.4), "18:25='18-25'; 
                                  26:44='26-44'; 
                                  45:65='45-65';
                                  66:100='66-'"),
         transferencia = ifelse(M5.2_1 == 1 | M5.2_2 == 1 | M5.2_3 == 1 | M5.2_OTRO != '', 1, 0),
         despido = ifelse(M5.4A_1 == 1, 1, 0),
         suspensiones1 = ifelse(M5.4A_2 == 1, 1, 0),
         suspensiones2 = ifelse(M5.4A_3 == 1, 1, 0),
         suspensiones3 = ifelse(M5.4A_4 == 1, 1, 0),
         cambio_actividad = ifelse(M5.4A_5 == 1, 1, 0),
         reduccion_salario = ifelse(M5.4A_7 == 1, 1, 0),
         teletrabajo = case_when(M5.4A_6 == 1 | M5.5A_6 == 1 ~ 1, 
                                 TRUE ~ 0),
         cierre = ifelse(M5.5A_1 == 1, 1, 0),
         rama2 = case_when(M4.13B == 1 | M4.13B == 2 ~ "Primarias + extractivas",
                           M4.13B == 3 | M4.13B == 4 ~ "Manufactura + energia-agua-gas",
                           M4.13B == 5 ~ "Construcción",
                           M4.13B == 6 | M4.13B == 7 ~ "Comercio + hotelería",
                           M4.13B == 8 ~ "Transporte",
                           M4.13B == 11 ~ "Adm. pública",
                           M4.13B == 9 | M4.13B == 10 | M4.13B >= 12 ~ "Servicios",
                           TRUE ~ NA_character_),
         rama2020 = ifelse(M4.10B1 == 1 | is.na(M4.10B1), rama, rama2))



## Preparación de mapa--------------
argentina <- st_read("C:/Users/Jose/OneDrive/Otros/proyectos_R/mapas/Codgeo_Pais_x_dpto_con_datos/argentina_n.shp")
argentina <- argentina %>%
  filter(!link %in% c("94028"))

provincias <- st_read("C:/Users/Jose/OneDrive/Otros/proyectos_R/mapas/Codgeo_Pais_x_prov_datos/provincias_n.shp")

partidos <- c("06028", "06035", "06091", "06134", "06252", "06260", "06270", "06274", "06364", "06371",
              "06408", "06410", "06412", "06427", "06434", "06490", "06515", "06525", "06539", "06560",
              "06568", "06638", "06648", "06658", "06749", "06756", "06760", "06778", "06805", "06840",
              "06861")

argentina$link <- as.character(argentina$link)
argentina$codpcia <- as.numeric(argentina$codpcia)

argentina <- argentina %>% 
  mutate(REGION = case_when(codpcia == 2 | (link %in% partidos) ~ "GBA",
                            codpcia == 10 | codpcia == 38 | codpcia == 46 | codpcia == 66 | 
                              codpcia == 90 | codpcia == 86 ~ "NOA",
                            codpcia == 18 | codpcia == 22 | codpcia == 54 | codpcia == 34 ~ "NEA",
                            codpcia == 50 | codpcia == 70 | codpcia == 74 ~ "CUYO",
                            (codpcia == 6 & !(link %in% partidos)) | codpcia == 42 ~ "PAMPEANA",
                            codpcia == 30 | codpcia == 14 | codpcia == 82 ~ "CENTRO", 
                            codpcia == 58 | codpcia == 62 | codpcia == 26 | codpcia == 78 |
                              codpcia == 94 ~ "PATAGONICA",
                            TRUE ~ NA_character_)) %>% 
  select(REGION, link, codpcia, geometry)

# argentina_agrup <- argentina %>% 
#   group_by(REGION) %>%
#   summarise(geometry = st_union(geometry)) %>% 
#   ungroup()

theme_map_pirc <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Armado EGP 2021 (encuestado) --------------------------------------

## Separo dígitos del CNO
base_hogar$cno12 <- ifelse(nchar(base_hogar$CNO_encuestado) > 4, str_sub(base_hogar$CNO_encuestado, 1, 2), str_sub(base_hogar$CNO_encuestado, 1, 1))
base_hogar$cno3 <- ifelse(nchar(base_hogar$CNO_encuestado) > 4, str_sub(base_hogar$CNO_encuestado, 3, 3), str_sub(base_hogar$CNO_encuestado, 2, 2))
base_hogar$cno4 <- ifelse(nchar(base_hogar$CNO_encuestado) > 4, str_sub(base_hogar$CNO_encuestado, 4, 4), str_sub(base_hogar$CNO_encuestado, 3, 3))
base_hogar$cno5 <- ifelse(nchar(base_hogar$CNO_encuestado) > 4, str_sub(base_hogar$CNO_encuestado, 5, 5), str_sub(base_hogar$CNO_encuestado, 4, 4))

base_hogar$cno12 <- as.numeric(base_hogar$cno12)
base_hogar$cno3 <- as.numeric(base_hogar$cno3)
base_hogar$cno4 <- as.numeric(base_hogar$cno4)
base_hogar$cno5 <- as.numeric(base_hogar$cno5)


## Actividades agrícolas
base_hogar <- base_hogar %>%
  mutate(rama_r = case_when(CAES_num > 0 & CAES_num <= 300 ~ 1,
                          CAES_num >= 500 & CAES_num < 9999 ~ 0))


## Categoría de ocupación (no distingue ocupaciones directivas)
base_hogar <- base_hogar %>%
  mutate(categoria = case_when(M3.5 == 1 ~ 1,
                               M3.5 == 2 | cno3 == 1 ~ 2,
                               (M3.5 >= 3 & M3.5 <= 5) & (cno3 == 2) ~ 3,
                               (M3.5 >= 3 & M3.5 <= 5) &
                                 (cno3 == 3 | cno3 == 0 | cno3 == 9 | is.na(cno3)) ~ 4))


## Tamaño de actividad
base_hogar <- base_hogar %>%
  mutate(tamano = case_when(M3.6 >= 1 & M3.6 < 3 ~ 1,
                                 M3.6 >= 3 ~ 2,
                                 TRUE ~ NA_real_))

## EGP
base_hogar <- base_hogar %>%
  mutate(egp = case_when( #Directivos y patrones

    cno12 >= 0 & cno12 <= 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 == 1 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & rama_r == 1 ~ 7,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & rama_r == 1 ~ 7,
    (cno12 == 6 | cno12 == 7) & cno5 != 1 & is.na(tamano) ~ 1,
    cno12 == 3 ~ 1,
    cno12 == 4 ~ 2,
    cno12 > 7 & categoria == 1 & tamano == 2 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 == 1 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & rama_r == 1 ~ 7,


    #Cuenta propia
    categoria == 2 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | (cno12 >= 60 & cno12 <= 81)
                                  | cno12 >= 90) ~ 1,
    categoria == 2 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54)
                                  | cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 2 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 43) | cno12 == 45 | cno12 == 51
                                  | (cno12 >= 60 & cno12 <= 71) | cno12 == 81 | cno12 == 91
                                  | cno12 == 99) ~ 2,
    categoria == 2 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 30) | cno12 == 36 | cno12 == 44 |
                                    cno12 == 46 | cno12 == 47 | cno12 == 50 |
                                    (cno12 >= 52 & cno12 <= 54) | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 10 & cno12 <= 30) | cno12 == 32 |
                                                  (cno12 >= 34 & cno12 <= 47) |
                                                  (cno12 >= 50 & cno12 <= 54) | cno12 == 58
                                                | cno12 == 64 | cno12 >= 70) ~ 6,
    categoria == 2 & (cno12 == 31 | cno12 == 33 | cno12 == 48 | cno12 == 49 |
                        (cno12 >= 55 & cno12 <= 57)) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 7,


    #Asalariados jefes
    categoria == 3 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 3 & cno5 == 1 & (cno12 == 10 | (cno12 >= 30 & cno12 <= 32) | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 3 & cno5 == 2 & ((cno12 >= 10 & cno12 <= 32) | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 49) | (cno12 >= 50 & cno12 <= 53) |
                                    (cno12 >= 60 & cno12 <= 71) | cno12 == 81 |
                                    cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 3 & cno5 == 2 & (cno12 == 33 | cno12 == 36 |
                                    (cno12 >= 56 & cno12 <= 58) | cno12 == 72 | cno12 == 80 |
                                    cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 3 & cno5 == 2 & (cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 30 | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 47 | cno12 == 48 |
                                    cno12 == 81 | cno12 == 91) ~ 2,
    categoria == 3 & cno5 == 3 & (cno12 == 32 | cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 33 & cno12 <=36) | cno12 == 40 | cno12 == 44 | cno12 == 45 |
                                    (cno12 >= 49 & cno12 <= 53) | (cno12 >= 55 & cno12 <= 58) |
                                    cno12 == 64 | (cno12 >= 70 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 8,
    categoria == 3 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 64) ~ 11,


    #Asalariados directos
    categoria == 4 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 4 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 4 & cno5 == 1 & cno12 == 31 ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 45) | cno12 == 50 | cno12 == 52 |
                                    (cno12 >= 60 & cno12 <= 71) |
                                    cno12 == 81 | cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 4 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 31) | cno12 == 46 | cno12 == 54) ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 36 | (cno12 >= 47 & cno12 <= 49) | cno12 == 51 |
                                    cno12 == 53 | cno12 == 56 | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 4 & cno5 == 2 & (cno12 == 57) ~ 9,
    categoria == 4 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 54 |
                                    cno12 == 81 | cno12 == 91) ~ 3,
    categoria == 4 & cno5 == 3 & (cno12 == 30 | cno12 == 32) ~ 4,
    categoria == 4 & cno5 == 3 & (cno12 >= 48 & cno12 <= 50) ~ 8,
    categoria == 4 & cno5 == 3 & (cno12 == 36 | cno12 == 40 | cno12 == 44 | cno12 == 45 | cno12 == 47
                                  | cno12 == 52 | cno12 == 53 | cno12 == 55 |
                                    cno12 == 57 | (cno12 >= 71 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 9,
    categoria == 4 & cno5 == 3 & (cno12 == 34 | cno12 == 35 | cno12 == 51 | cno12 == 56 |
                                    cno12 == 58 | cno12 == 64 | cno12 == 70) ~ 10,
    categoria == 4 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno5 == 4 & (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 31 | cno12 == 41
                                  | cno12 == 42 | cno12 == 46) ~ 3,
    categoria == 4 & cno5 == 4 & (cno12 == 30) ~ 4,
    categoria == 4 & cno5 == 4 & (cno12 == 32 | (cno12 >= 34 & cno12 <= 40) | cno12 == 44 |
                                    cno12 == 45 | (cno12 >= 47 & cno12 <= 58) |
                                    cno12 == 64 | cno12 >= 70) ~ 10,
    categoria == 4 & cno5 == 4 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno12 == 33 ~ 10
  ))


base_hogar$egp_factor <- factor(base_hogar$egp, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb", "IVc",
                                             "V", "VI", "VIIa", "VIIb"))

base_hogar <- mutate(base_hogar, egp5 = car::recode(base_hogar$egp, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5"))
base_hogar$egp5_factor <- factor(base_hogar$egp5, labels = c("I+II", "III", "IV", "V+VI", "VII"))

## Variación Solís y Boado (2016)
base_hogar <- base_hogar %>%
  mutate(egp_mod = case_when(categoria == 2 & cno5 == 4 & rama_r == 0 ~ 10,
                             categoria == 2 & cno5 == 4 & rama_r == 1 ~ 11,
                             categoria == 2 & cno12 == 33 ~ 10,
                             TRUE ~ egp))

base_hogar$egp_mod_factor <- factor(base_hogar$egp_mod, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb",
                                                                   "IVc", "V", "VI", "VIIa", "VIIb"))

base_hogar <- mutate(base_hogar, egp5_mod = car::recode(base_hogar$egp_mod, "1:2=1; 3:4=2; 5:7=3; 8:9=4;
                                                        c(10,11)=5"))
base_hogar$egp5_mod_factor <- factor(base_hogar$egp5_mod,
                                     labels = c("Clase de servicio",
                                                "Trabajadores no manuales rutinarios",
                                                "Pequeños propietarios e independientes",
                                                "Clase trabajadora calificada",
                                                "Clase trabajadora no calificada"))

# Armado EGP 2021 (PSH) --------------------------------------

## Separo dígitos del CNO
base_hogar$cno12 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 1, 2), str_sub(base_hogar$CNO_PSH, 1, 1))
base_hogar$cno3 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 3, 3), str_sub(base_hogar$CNO_PSH, 2, 2))
base_hogar$cno4 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 4, 4), str_sub(base_hogar$CNO_PSH, 3, 3))
base_hogar$cno5 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 5, 5), str_sub(base_hogar$CNO_PSH, 4, 4))

base_hogar$cno12 <- as.numeric(base_hogar$cno12)
base_hogar$cno3 <- as.numeric(base_hogar$cno3)
base_hogar$cno4 <- as.numeric(base_hogar$cno4)
base_hogar$cno5 <- as.numeric(base_hogar$cno5)


## Actividades agrícolas
base_hogar <- base_hogar %>%
  mutate(rama_r = ifelse(M12.4 == 1, 1, 0))


## Categoría de ocupación (no distingue ocupaciones directivas)
base_hogar <- base_hogar %>%
  mutate(categoria = case_when(M3.5 == 1 ~ 1,
                               M3.5 == 2 | cno3 == 1 ~ 2,
                               (M3.5 >= 3 & M3.5 <= 4) & (cno3 == 2) ~ 3,
                               (M3.5 >= 3 & M3.5 <= 4) &
                                 (cno3 == 3 | cno3 == 0 | cno3 == 9 | is.na(cno3)) ~ 4))


## Tamaño de actividad
base_hogar <- base_hogar %>%
  mutate(tamano = case_when(M12.6 >= 1 & M12.6 < 3 ~ 1,
                            M12.6 >= 3 ~ 2,
                            TRUE ~ NA_real_))

## EGP
base_hogar <- base_hogar %>%
  mutate(egp = case_when( #Directivos y patrones
    
    cno12 >= 0 & cno12 <= 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 == 1 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & rama_r == 1 ~ 7,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & rama_r == 1 ~ 7,
    (cno12 == 6 | cno12 == 7) & cno5 != 1 & is.na(tamano) ~ 1,
    cno12 == 3 ~ 1,
    cno12 == 4 ~ 2,
    cno12 > 7 & categoria == 1 & tamano == 2 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 == 1 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & rama_r == 1 ~ 7,
    
    
    #Cuenta propia
    categoria == 2 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | (cno12 >= 60 & cno12 <= 81)
                                  | cno12 >= 90) ~ 1,
    categoria == 2 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54)
                                  | cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 2 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 43) | cno12 == 45 | cno12 == 51
                                  | (cno12 >= 60 & cno12 <= 71) | cno12 == 81 | cno12 == 91
                                  | cno12 == 99) ~ 2,
    categoria == 2 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 30) | cno12 == 36 | cno12 == 44 |
                                    cno12 == 46 | cno12 == 47 | cno12 == 50 |
                                    (cno12 >= 52 & cno12 <= 54) | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 10 & cno12 <= 30) | cno12 == 32 |
                                                  (cno12 >= 34 & cno12 <= 47) |
                                                  (cno12 >= 50 & cno12 <= 54) | cno12 == 58
                                                | cno12 == 64 | cno12 >= 70) ~ 6,
    categoria == 2 & (cno12 == 31 | cno12 == 33 | cno12 == 48 | cno12 == 49 |
                        (cno12 >= 55 & cno12 <= 57)) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 7,
    
    
    #Asalariados jefes
    categoria == 3 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 3 & cno5 == 1 & (cno12 == 10 | (cno12 >= 30 & cno12 <= 32) | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 3 & cno5 == 2 & ((cno12 >= 10 & cno12 <= 32) | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 49) | (cno12 >= 50 & cno12 <= 53) |
                                    (cno12 >= 60 & cno12 <= 71) | cno12 == 81 |
                                    cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 3 & cno5 == 2 & (cno12 == 33 | cno12 == 36 |
                                    (cno12 >= 56 & cno12 <= 58) | cno12 == 72 | cno12 == 80 |
                                    cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 3 & cno5 == 2 & (cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 30 | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 47 | cno12 == 48 |
                                    cno12 == 81 | cno12 == 91) ~ 2,
    categoria == 3 & cno5 == 3 & (cno12 == 32 | cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 33 & cno12 <=36) | cno12 == 40 | cno12 == 44 | cno12 == 45 |
                                    (cno12 >= 49 & cno12 <= 53) | (cno12 >= 55 & cno12 <= 58) |
                                    cno12 == 64 | (cno12 >= 70 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 8,
    categoria == 3 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 64) ~ 11,
    
    
    #Asalariados directos
    categoria == 4 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 4 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 4 & cno5 == 1 & cno12 == 31 ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 45) | cno12 == 50 | cno12 == 52 |
                                    (cno12 >= 60 & cno12 <= 71) |
                                    cno12 == 81 | cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 4 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 31) | cno12 == 46 | cno12 == 54) ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 36 | (cno12 >= 47 & cno12 <= 49) | cno12 == 51 |
                                    cno12 == 53 | cno12 == 56 | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 4 & cno5 == 2 & (cno12 == 57) ~ 9,
    categoria == 4 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 54 |
                                    cno12 == 81 | cno12 == 91) ~ 3,
    categoria == 4 & cno5 == 3 & (cno12 == 30 | cno12 == 32) ~ 4,
    categoria == 4 & cno5 == 3 & (cno12 >= 48 & cno12 <= 50) ~ 8,
    categoria == 4 & cno5 == 3 & (cno12 == 36 | cno12 == 40 | cno12 == 44 | cno12 == 45 | cno12 == 47
                                  | cno12 == 52 | cno12 == 53 | cno12 == 55 |
                                    cno12 == 57 | (cno12 >= 71 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 9,
    categoria == 4 & cno5 == 3 & (cno12 == 34 | cno12 == 35 | cno12 == 51 | cno12 == 56 |
                                    cno12 == 58 | cno12 == 64 | cno12 == 70) ~ 10,
    categoria == 4 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno5 == 4 & (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 31 | cno12 == 41
                                  | cno12 == 42 | cno12 == 46) ~ 3,
    categoria == 4 & cno5 == 4 & (cno12 == 30) ~ 4,
    categoria == 4 & cno5 == 4 & (cno12 == 32 | (cno12 >= 34 & cno12 <= 40) | cno12 == 44 |
                                    cno12 == 45 | (cno12 >= 47 & cno12 <= 58) |
                                    cno12 == 64 | cno12 >= 70) ~ 10,
    categoria == 4 & cno5 == 4 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno12 == 33 ~ 10
  ))


base_hogar$egp_factor <- factor(base_hogar$egp, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb", "IVc",
                                                           "V", "VI", "VIIa", "VIIb"))

base_hogar <- mutate(base_hogar, egp5 = car::recode(base_hogar$egp, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5"))
base_hogar$egp5_factor <- factor(base_hogar$egp5, labels = c("I+II", "III", "IV", "V+VI", "VII"))

## Variación Solís y Boado (2016)
base_hogar <- base_hogar %>%
  mutate(egp_mod_psh = case_when(categoria == 2 & cno5 == 4 & rama_r == 0 ~ 10,
                             categoria == 2 & cno5 == 4 & rama_r == 1 ~ 11,
                             categoria == 2 & cno12 == 33 ~ 10,
                             TRUE ~ egp))

base_hogar$egp_mod_factor_psh <- factor(base_hogar$egp_mod_psh, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb",
                                                                   "IVc", "V", "VI", "VIIa", "VIIb"))

base_hogar <- mutate(base_hogar, egp5_mod_psh = car::recode(base_hogar$egp_mod, "1:2=1; 3:4=2; 5:7=3; 8:9=4;
                                                        c(10,11)=5"))
base_hogar$egp5_mod_factor_psh <- factor(base_hogar$egp5_mod,
                                     labels = c("Clase de servicio",
                                                "Trabajadores no manuales rutinarios",
                                                "Pequeños propietarios e independientes",
                                                "Clase trabajadora calificada",
                                                "Clase trabajadora no calificada"))


# # Armado EGP 2015 --------------------------------------
# 
# ## Separo dígitos del CNO
# base_hogar$cno12 <- ifelse(nchar(base_hogar$CNO_2015) > 4, str_sub(base_hogar$CNO_2015, 1, 2), str_sub(base_hogar$CNO_2015, 1, 1))
# base_hogar$cno3 <- ifelse(nchar(base_hogar$CNO_2015) > 4, str_sub(base_hogar$CNO_2015, 3, 3), str_sub(base_hogar$CNO_2015, 2, 2))
# base_hogar$cno4 <- ifelse(nchar(base_hogar$CNO_2015) > 4, str_sub(base_hogar$CNO_2015, 4, 4), str_sub(base_hogar$CNO_2015, 3, 3))
# base_hogar$cno5 <- ifelse(nchar(base_hogar$CNO_2015) > 4, str_sub(base_hogar$CNO_2015, 5, 5), str_sub(base_hogar$CNO_2015, 4, 4))
# 
# base_hogar$cno12 <- as.numeric(base_hogar$cno12)
# base_hogar$cno3 <- as.numeric(base_hogar$cno3)
# base_hogar$cno4 <- as.numeric(base_hogar$cno4)
# base_hogar$cno5 <- as.numeric(base_hogar$cno5)
# 
# 
# ## Actividades agrícolas 
# base_hogar <- base_hogar %>% 
#   mutate(rama_r = case_when(M4.13A <= 2 ~ 1,
#                           M4.13A > 2 & M4.13A <= 16 ~ 0))
# 
# 
# ## Categoría de ocupación (no distingue ocupaciones directivas)
# base_hogar <- base_hogar %>% 
#   mutate(categoria = case_when(M4.14A == 1 ~ 1,
#                                M4.14A == 2 | cno3 == 1 ~ 2,
#                                M4.14A == 3 & cno3 == 2 ~ 3,
#                                M4.14A == 3 & (cno3 == 3 | cno3 == 0 | cno3 == 9 | is.na(cno3)) ~ 4))
# 
# 
# ## Tamaño de actividad
# base_hogar <- base_hogar %>% 
#   mutate(tamano = case_when(M4.15A >= 1 & M4.15A < 3 ~ 1,
#                             M4.15A >= 3 ~ 2,
#                             TRUE ~ NA_real_)) 
# 
# ## EGP
# base_hogar <- base_hogar %>% 
#   mutate(egp2015 = case_when( #Directivos y patrones
#     
#     cno12 >= 0 & cno12 <= 2 ~ 1,
#     (cno12 >= 5 & cno12 <= 7) & cno5 == 1 ~ 1,
#     (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 2 ~ 1,
#     (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
#     (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & rama_r == 1 ~ 7,
#     cno12 == 5 & cno5 != 1 & is.na(tamano) & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
#     cno12 == 5 & cno5 != 1 & is.na(tamano) & rama_r == 1 ~ 7, 
#     (cno12 == 6 | cno12 == 7) & cno5 != 1 & is.na(tamano) ~ 1,
#     cno12 == 3 ~ 1,
#     cno12 == 4 ~ 2,
#     cno12 > 7 & categoria == 1 & tamano == 2 ~ 1,
#     cno12 > 7 & categoria == 1 & tamano == 1 & cno5 == 1 ~ 1,
#     cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & (rama_r == 0 | rama_r == "" | is.na(rama_r)) ~ 5,
#     cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & rama_r == 1 ~ 7,
#     
#     
#     #Cuenta propia
#     categoria == 2 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 | 
#                                     (cno12 >= 40 & cno12 <= 44) | (cno12 >= 60 & cno12 <= 81)
#                                   | cno12 >= 90) ~ 1,
#     categoria == 2 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
#                                     (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54)
#                                   | cno12 == 58 | cno12 == 82) ~ 2,
#     categoria == 2 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
#                                     (cno12 >= 40 & cno12 <= 43) | cno12 == 45 | cno12 == 51
#                                   | (cno12 >= 60 & cno12 <= 71) | cno12 == 81 | cno12 == 91
#                                   | cno12 == 99) ~ 2,
#     categoria == 2 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 30) | cno12 == 36 | cno12 == 44 |
#                                     cno12 == 46 | cno12 == 47 | cno12 == 50 | 
#                                     (cno12 >= 52 & cno12 <= 54) | cno12 == 58 | cno12 == 72 |
#                                     cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 6,
#     categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 10 & cno12 <= 30) | cno12 == 32 | 
#                                                   (cno12 >= 34 & cno12 <= 47) | 
#                                                   (cno12 >= 50 & cno12 <= 54) | cno12 == 58 
#                                                 | cno12 == 64 | cno12 >= 70) ~ 6,
#     categoria == 2 & (cno12 == 31 | cno12 == 33 | cno12 == 48 | cno12 == 49 | 
#                         (cno12 >= 55 & cno12 <= 57)) ~ 6,
#     categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 7,
#     
#     
#     #Asalariados jefes
#     categoria == 3 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
#                                     (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
#                                     (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
#     categoria == 3 & cno5 == 1 & (cno12 == 10 | (cno12 >= 30 & cno12 <= 32) | cno12 == 36 |
#                                     (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
#                                     cno12 == 58 | cno12 == 82) ~ 2,
#     categoria == 3 & cno5 == 2 & ((cno12 >= 10 & cno12 <= 32) | cno12 == 34 | cno12 == 35 |
#                                     (cno12 >= 40 & cno12 <= 49) | (cno12 >= 50 & cno12 <= 53) | 
#                                     (cno12 >= 60 & cno12 <= 71) | cno12 == 81 | 
#                                     cno12 == 91 | cno12 == 99) ~ 2,
#     categoria == 3 & cno5 == 2 & (cno12 == 33 | cno12 == 36 | 
#                                     (cno12 >= 56 & cno12 <= 58) | cno12 == 72 | cno12 == 80 |
#                                     cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
#     categoria == 3 & cno5 == 2 & (cno12 == 54) ~ 3,
#     categoria == 3 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 30 | cno12 == 31 | 
#                                     (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 47 | cno12 == 48 | 
#                                     cno12 == 81 | cno12 == 91) ~ 2,
#     categoria == 3 & cno5 == 3 & (cno12 == 32 | cno12 == 54) ~ 3,
#     categoria == 3 & cno5 == 3 & ((cno12 >= 33 & cno12 <=36) | cno12 == 40 | cno12 == 44 | cno12 == 45 |
#                                     (cno12 >= 49 & cno12 <= 53) | (cno12 >= 55 & cno12 <= 58) |
#                                     cno12 == 64 | (cno12 >= 70 & cno12 <= 80) | cno12 == 82 |
#                                     cno12 == 90 | cno12 >= 92) ~ 8,
#     categoria == 3 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 64) ~ 11,
#     
#     
#     #Asalariados directos
#     categoria == 4 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
#                                     (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
#                                     (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
#     categoria == 4 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
#                                     (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
#                                     cno12 == 58 | cno12 == 82) ~ 2,
#     categoria == 4 & cno5 == 1 & cno12 == 31 ~ 3,
#     categoria == 4 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
#                                     (cno12 >= 40 & cno12 <= 45) | cno12 == 50 | cno12 == 52 |
#                                     (cno12 >= 60 & cno12 <= 71) |
#                                     cno12 == 81 | cno12 == 91 | cno12 == 99) ~ 2,
#     categoria == 4 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 31) | cno12 == 46 | cno12 == 54) ~ 3,
#     categoria == 4 & cno5 == 2 & (cno12 == 36 | (cno12 >= 47 & cno12 <= 49) | cno12 == 51 | 
#                                     cno12 == 53 | cno12 == 56 | cno12 == 58 | cno12 == 72 | 
#                                     cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
#     categoria == 4 & cno5 == 2 & (cno12 == 57) ~ 9,
#     categoria == 4 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 31 | 
#                                     (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 54 |
#                                     cno12 == 81 | cno12 == 91) ~ 3,
#     categoria == 4 & cno5 == 3 & (cno12 == 30 | cno12 == 32) ~ 4,
#     categoria == 4 & cno5 == 3 & (cno12 >= 48 & cno12 <= 50) ~ 8,
#     categoria == 4 & cno5 == 3 & (cno12 == 36 | cno12 == 40 | cno12 == 44 | cno12 == 45 | cno12 == 47 
#                                   | cno12 == 52 | cno12 == 53 | cno12 == 55 |
#                                     cno12 == 57 | (cno12 >= 71 & cno12 <= 80) | cno12 == 82 |
#                                     cno12 == 90 | cno12 >= 92) ~ 9,
#     categoria == 4 & cno5 == 3 & (cno12 == 34 | cno12 == 35 | cno12 == 51 | cno12 == 56 |
#                                     cno12 == 58 | cno12 == 64 | cno12 == 70) ~ 10,
#     categoria == 4 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
#     categoria == 4 & cno5 == 4 & (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 31 | cno12 == 41
#                                   | cno12 == 42 | cno12 == 46) ~ 3,
#     categoria == 4 & cno5 == 4 & (cno12 == 30) ~ 4,
#     categoria == 4 & cno5 == 4 & (cno12 == 32 | (cno12 >= 34 & cno12 <= 40) | cno12 == 44 |
#                                     cno12 == 45 | (cno12 >= 47 & cno12 <= 58) |
#                                     cno12 == 64 | cno12 >= 70) ~ 10,
#     categoria == 4 & cno5 == 4 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
#     categoria == 4 & cno12 == 33 ~ 10
#   ))
# 
# 
# base_hogar$egp2015_factor <- factor(base_hogar$egp2015, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb", "IVc",
#                                                            "V", "VI", "VIIa", "VIIb"))
# 
# base_hogar <- mutate(base_hogar, egp2015_5 = car::recode(base_hogar$egp2015, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5"))
# base_hogar$egp2015_5_factor <- factor(base_hogar$egp2015_5, labels = c("I+II", "III", "IV", "V+VI", "VII"))
# 
# ## Variación Solís y Boado (2016)
# base_hogar <- base_hogar %>% 
#   mutate(egp2015_mod = case_when(categoria == 2 & cno5 == 4 & rama_r == 0 ~ 10,
#                              categoria == 2 & cno5 == 4 & rama_r == 1 ~ 11,
#                              categoria == 2 & cno12 == 33 ~ 10,
#                              TRUE ~ egp2015))
# 
# base_hogar$egp2015_mod_factor <- factor(base_hogar$egp2015_mod, labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb",
#                                                                    "IVc", "V", "VI", "VIIa", "VIIb"))
# 
# base_hogar <- mutate(base_hogar, egp2015_5_mod = car::recode(base_hogar$egp2015_mod, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 
#                                                         c(10,11)=5"))
# 
# 
# 
# base_hogar$egp2015_5_mod <- ifelse(base_hogar$M4.10A1 == 1 | is.na(base_hogar$M4.10A1), base_hogar$egp5_mod, base_hogar$egp2015_5_mod)
# 
# base_hogar$egp2015_5_mod_factor <- factor(base_hogar$egp2015_5_mod, 
#                                           labels = c("Clase de servicio", 
#                                                      "Trabajadores no manuales rutinarios",
#                                                      "Pequeños propietarios e independientes", 
#                                                      "Clase trabajadora calificada", 
#                                                      "Clase trabajadora no calificada"))
# 
# 
# ## Trayectorias 2015-2020----------------
# base_hogar <- base_hogar %>% 
#   mutate(trayectoria_clase = case_when(egp2015_5_mod == 1 & egp5_mod == 1 ~ 1,
#                                        egp2015_5_mod == 1 & (egp5_mod >= 2 & egp5_mod <= 3) ~ 2,
#                                        egp2015_5_mod == 1 & (egp5_mod >= 4 & egp5_mod <= 5) ~ 3,
#                                        (egp2015_5_mod >= 2 & egp2015_5_mod <= 3) & egp5_mod == 1 ~ 4,
#                                        (egp2015_5_mod >= 2 & egp2015_5_mod <= 3) & 
#                                          (egp5_mod >= 2 & egp5_mod <= 3) ~ 5,
#                                        (egp2015_5_mod >= 2 & egp2015_5_mod <= 3) & 
#                                          (egp5_mod >= 4 & egp5_mod <= 5) ~ 3,
#                                        (egp2015_5_mod >= 4 & egp2015_5_mod <= 5) & egp5_mod == 1 ~ 6,
#                                        (egp2015_5_mod >= 4 & egp2015_5_mod <= 5) & 
#                                          (egp5_mod >= 2 & egp5_mod <= 3) ~ 7,
#                                        (egp2015_5_mod >= 4 & egp2015_5_mod <= 5) &
#                                          (egp5_mod >= 4 & egp5_mod <= 5) ~ 8),
#          trayectoria_clase_f = factor(trayectoria_clase, labels = c("Herencia clase de servicio",
#                                                                     "Descenso clase intermedia",
#                                                                     "Descenso clase trabajadora",
#                                                                     "Ascenso clase de servicio",
#                                                                     "Herencia clase intermedia",
#                                                                     "Ascenso clase de servicio LD",
#                                                                     "Ascenso clase intermedia",
#                                                                     "Herencia clase trabajadora")))
# 
# 

# Indicadores -------------------

## Mercado de trabajo -----------------------------------------------------

### Mercado de trabajo x región----------------------------------------------

#Asalarización
asalariados <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  group_by(REGION) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "asalarizacion")

asalariados_n <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalarizacion = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "asalarizacion")

asalariados <- as.data.frame(rbind(asalariados, asalariados_tot, asalariados_n))

asalariados %>% 
  filter(REGION != "Total", REGION != "N") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = asalarizacion, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Tasa de \nAsalarización") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/asalarizacion.png", dpi = 300, type = "cairo", width = 6, height = 8)

#Asalarización registrada
asalariados_reg <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  group_by(REGION) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_reg_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "asalariados_reg")

asalariados_reg_n <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados_reg == 1) %>% 
  summarise(asalariados_reg = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "asalariados_reg")

asalariados_reg <- as.data.frame(rbind(asalariados_reg, asalariados_reg_tot, asalariados_reg_n))

asalariados_reg %>% 
  filter(REGION != "Total", REGION != "N") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = asalariados_reg, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Tasa de Asalarización \nregistrada") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/asalarizacion_reg.png", dpi = 300, type = "cairo", width = 6, height = 8)

#Cuentapropismo registrado
indep_reg <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  group_by(REGION) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

indep_reg_tot <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "indep_reg")

indep_reg_tot_n <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(indep_reg == 1) %>% 
  summarise(indep_reg = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "indep_reg")

indep_reg <- as.data.frame(rbind(indep_reg, indep_reg_tot, indep_reg_tot_n))

indep_reg %>% 
  filter(REGION != "Total", REGION != "N") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = indep_reg, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Tasa de cuentapropismo \nregistrado") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar",
                      breaks = seq(25, 55, by = 5))

ggsave(filename = "outputs/indicadores/indep_reg.png", dpi = 300, type = "cairo", width = 6, height = 8)

#Trabajo no registrado
trabajo_noreg <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  group_by(REGION) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2))

trabajo_noreg_tot <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "trabajo_noreg")

trabajo_noreg_n <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(noreg == 1) %>% 
  summarise(trabajo_noreg = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "trabajo_noreg")



trabajo_noreg <- as.data.frame(rbind(trabajo_noreg, trabajo_noreg_tot, trabajo_noreg_n))

trabajo_noreg %>% 
  filter(REGION != "Total", REGION != "N") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = trabajo_noreg, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Tasa de trabajo \nno registrado") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar",
                      breaks = seq(25, 50, by = 5))


ggsave(filename = "outputs/indicadores/trabajo_noreg.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Rama de actividad
rama <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(REGION, rama) %>% 
  tally(POND2R_FIN) %>% 
  group_by(REGION) %>% 
  mutate(porcentaje = round((n/sum(n))*100, digits = 2)) %>% 
  pivot_wider(id_cols = rama, names_from = REGION, values_from = porcentaje)


rama_tot <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(rama) %>% 
  tally(POND2R_FIN) %>% 
  mutate(Total = round((n/sum(n))*100, digits = 2)) %>%
  select(rama, Total) 

rama <- rama %>% 
  left_join(rama_tot, by = "rama")

rama_n <- base_hogar %>%
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(REGION) %>% 
  tally(POND2R_FIN) %>% 
  pivot_wider(names_from = REGION, values_from = n) %>% 
  add_column(rama = "N", .before = "CENTRO") %>% 
  janitor::adorn_totals("col")

rama <- as.data.frame(rbind(rama, rama_n))

#Merge region
trabajo_total_region <- asalariados %>% 
  left_join(asalariados_reg, by = "REGION") %>% 
  left_join(indep_reg, by = "REGION") %>% 
  left_join(trabajo_noreg, by = "REGION") %>% 
  as.data.frame()

write.xlsx(trabajo_total_region, "outputs/indicadores.xlsx", sheetName = "trabajo_region1", row.names = F)
write.xlsx(rama, "outputs/indicadores.xlsx", sheetName = "trabajo_region2", row.names = F, append = T)

### Mercado de trabajo x género -------------------------------------------

#Asalarización
asalariados <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "asalarizacion")

asalariados_tot_n <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalarizacion = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "asalarizacion")

asalariados <- as.data.frame(rbind(asalariados, asalariados_tot, asalariados_tot_n))

#Asalarización registrada
asalariados_reg <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_reg_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "asalariados_reg")

asalariados_reg_tot_n <- base_hogar %>% 
  mutate(asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados_reg == 1) %>% 
  summarise(asalariados_reg = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "asalariados_reg")

asalariados_reg <- as.data.frame(rbind(asalariados_reg, asalariados_reg_tot, asalariados_reg_tot_n))


#Cuentapropismo registrado
indep_reg <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

indep_reg_tot <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "indep_reg")

indep_reg_tot_n <- base_hogar %>% 
  mutate(indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(indep_reg == 1) %>% 
  summarise(indep_reg = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "indep_reg")

indep_reg <- as.data.frame(rbind(indep_reg, indep_reg_tot, indep_reg_tot_n))


#Trabajo no registrado
trabajo_noreg <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2))

trabajo_noreg_tot <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "trabajo_noreg")

trabajo_noreg_tot_n <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(noreg == 1) %>% 
  summarise(trabajo_noreg = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "trabajo_noreg")

trabajo_noreg <- as.data.frame(rbind(trabajo_noreg, trabajo_noreg_tot, trabajo_noreg_tot_n))


# Rama de actividad
rama <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(sexo_f, rama) %>% 
  tally(POND2R_FIN) %>% 
  group_by(sexo_f) %>% 
  mutate(porcentaje = round((n/sum(n))*100, digits = 2)) %>% 
  pivot_wider(id_cols = rama, names_from = sexo_f, values_from = porcentaje)


rama_tot <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(rama) %>% 
  tally(POND2R_FIN) %>% 
  mutate(Total = round((n/sum(n))*100, digits = 2)) %>%
  select(rama, Total) 

rama <- rama %>% 
  left_join(rama_tot, by = "rama")

rama_n <- base_hogar %>%
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(sexo_f) %>% 
  tally(POND2R_FIN) %>% 
  pivot_wider(names_from = sexo_f, values_from = n) %>% 
  add_column(rama = "N", .before = "Varón") %>% 
  janitor::adorn_totals("col")

rama <- as.data.frame(rbind(rama, rama_n))

#Merge sexo
trabajo_total_sexo <- asalariados %>% 
  left_join(asalariados_reg, by = "sexo_f") %>% 
  left_join(indep_reg, by = "sexo_f") %>% 
  left_join(trabajo_noreg, by = "sexo_f") 

write.xlsx(trabajo_total_sexo, "outputs/indicadores.xlsx", sheetName = "trabajo_genero1", row.names = F, append = T)
write.xlsx(rama, "outputs/indicadores.xlsx", sheetName = "trabajo_genero2", row.names = F, append = T)

### Mercado de trabajo x edad -------------------------------------------

#Asalarización
asalariados <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  group_by(edad_grupo) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(M2.12 == 1) %>% 
  summarise(asalarizacion = round(weighted.mean(asalariados, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(edad_grupo = "Total", .before = "asalarizacion")

asalariados_tot_n <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0")) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalarizacion = round(sum(POND2R_FIN))) %>% 
  add_column(edad_grupo = "N", .before = "asalarizacion")

asalariados <- as.data.frame(rbind(asalariados, asalariados_tot, asalariados_tot_n))

#Asalarización registrada
asalariados_reg <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  group_by(edad_grupo) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

asalariados_reg_tot <- base_hogar %>% 
  mutate(asalariados = car::recode(M3.5, "3:5 = 1; 1:2 = 0; 99=0"),
         asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados == 1) %>% 
  summarise(asalariados_reg = round(weighted.mean(asalariados_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(edad_grupo = "Total", .before = "asalariados_reg")

asalariados_reg_tot_n <- base_hogar %>% 
  mutate(asalariados_reg = ifelse(M3.10 == 1, 1, 0)) %>% 
  filter(asalariados_reg == 1) %>% 
  summarise(asalariados_reg = round(sum(POND2R_FIN))) %>% 
  add_column(edad_grupo = "N", .before = "asalariados_reg")

asalariados_reg <- as.data.frame(rbind(asalariados_reg, asalariados_reg_tot, asalariados_reg_tot_n))


#Cuentapropismo registrado
indep_reg <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  group_by(edad_grupo) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2))

indep_reg_tot <- base_hogar %>% 
  mutate(independientes = car::recode(M3.5, "3:5 = 0; 1:2 = 1; 99=0"),
         indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(independientes == 1) %>% 
  summarise(indep_reg = round(weighted.mean(indep_reg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(edad_grupo = "Total", .before = "indep_reg")

indep_reg_tot_n <- base_hogar %>% 
  mutate(indep_reg = ifelse(M3.15 == 3, 1, 0)) %>% 
  filter(indep_reg == 1) %>% 
  summarise(indep_reg = round(sum(POND2R_FIN))) %>% 
  add_column(edad_grupo = "N", .before = "indep_reg")

indep_reg <- as.data.frame(rbind(indep_reg, indep_reg_tot, indep_reg_tot_n))


#Trabajo no registrado
trabajo_noreg <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  group_by(edad_grupo) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2))

trabajo_noreg_tot <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(M2.12 == 1) %>% 
  summarise(trabajo_noreg = round(weighted.mean(noreg, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(edad_grupo = "Total", .before = "trabajo_noreg")

trabajo_noreg_tot_n <- base_hogar %>% 
  mutate(noreg = case_when(M3.10 == 3 | M3.15 <= 2 ~ 1,
                           TRUE ~ 0)) %>% 
  filter(noreg == 1) %>% 
  summarise(trabajo_noreg = round(sum(POND2R_FIN))) %>% 
  add_column(edad_grupo = "N", .before = "trabajo_noreg")

trabajo_noreg <- as.data.frame(rbind(trabajo_noreg, trabajo_noreg_tot, trabajo_noreg_tot_n))


# Rama de actividad
rama <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(edad_grupo, rama) %>% 
  tally(POND2R_FIN) %>% 
  group_by(edad_grupo) %>% 
  mutate(porcentaje = round((n/sum(n))*100, digits = 2)) %>% 
  pivot_wider(id_cols = rama, names_from = edad_grupo, values_from = porcentaje)


rama_tot <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(rama) %>% 
  tally(POND2R_FIN) %>% 
  mutate(Total = round((n/sum(n))*100, digits = 2)) %>%
  select(rama, Total) 

rama <- rama %>% 
  left_join(rama_tot, by = "rama")

rama_n <- base_hogar %>%
  filter(M2.12 == 1 & !is.na(rama)) %>% 
  group_by(edad_grupo) %>% 
  tally(POND2R_FIN) %>% 
  pivot_wider(names_from = edad_grupo, values_from = n) %>% 
  add_column(rama = "N", .before = "18-25") %>% 
  janitor::adorn_totals("col")

rama <- as.data.frame(rbind(rama, rama_n))


#Merge edad
trabajo_total_edad <- asalariados %>% 
  left_join(asalariados_reg, by = "edad_grupo") %>% 
  left_join(indep_reg, by = "edad_grupo") %>% 
  left_join(trabajo_noreg, by = "edad_grupo")

write.xlsx(trabajo_total_edad, "outputs/indicadores.xlsx", sheetName = "trabajo_edad1", row.names = F, append = T)
write.xlsx(rama, "outputs/indicadores.xlsx", sheetName = "trabajo_edad2", row.names = F, append = T)


## Ingresos ----------------------

### Ingresos x region --------------

# ingresos laborales
ingresos_laborales <- base_hogar %>% 
  filter(M2.12 == 1) %>% 
  group_by(REGION) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T)))

ingresos_laborales_tot <- base_hogar %>% 
  filter(M2.12 == 1) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T))) %>% 
  add_column(REGION = "Total", .before = "ing_lab")


ingresos_laborales <- as.data.frame(rbind(ingresos_laborales, ingresos_laborales_tot))

ingresos_laborales %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = ing_lab, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Ingresos laborales \npromedio") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/ingresos_laborales.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Brechas ingresos laborales
brechas_ingresos <- ingresos_laborales %>% 
  mutate(brechas = round(ing_lab / ing_lab[REGION == "Total"], digits = 2))
  

brechas_ingresos %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = brechas, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "Brechas ingresos \nlaborales") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/brechas_ingresos.png", dpi = 300, type = "cairo", width = 6, height = 8)

#Merge region
ingresos_total_region <- ingresos_laborales %>% 
  left_join(brechas_ingresos, by = "REGION") %>% 
  as.data.frame()

write.xlsx(ingresos_total_region, "outputs/indicadores.xlsx", sheetName = "ingresos_region", row.names = F, append = T)


### Ingresos x género --------------
# ingresos laborales
ingresos_laborales <- base_hogar %>% 
  filter(M2.12 == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T)))

ingresos_laborales_tot <- base_hogar %>% 
  filter(M2.12 == 1) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T))) %>% 
  add_column(sexo_f = "Total", .before = "ing_lab")

ingresos_laborales <- as.data.frame(rbind(ingresos_laborales, ingresos_laborales_tot))

write.xlsx(ingresos_laborales, "outputs/indicadores.xlsx", sheetName = "ingresos_sexo", row.names = F, append = T)


### Ingresos x clase --------------
# ingresos laborales
ingresos_laborales <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(egp5_mod_factor)) %>% 
  group_by(egp5_mod_factor) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T)))

ingresos_laborales_tot <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(egp5_mod_factor)) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T))) %>% 
  add_column(egp5_mod_factor = "Total", .before = "ing_lab")

ingresos_laborales <- as.data.frame(rbind(ingresos_laborales, ingresos_laborales_tot))


write.xlsx(ingresos_laborales, "outputs/indicadores.xlsx", sheetName = "ingresos_clases", row.names = F, append = T)


### Ingresos x género x clase --------------
# ingresos laborales
ingresos_laborales <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(egp5_mod_factor)) %>% 
  group_by(egp5_mod_factor, sexo_f) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T))) %>% 
  pivot_wider(id_cols = egp5_mod_factor, names_from = sexo_f, values_from = ing_lab)

ingresos_laborales_tot <- base_hogar %>% 
  filter(M2.12 == 1 & !is.na(egp5_mod_factor)) %>%
  group_by(sexo_f) %>% 
  summarise(ing_lab = round(weighted.mean(ing_lab_imp, POND2R_FIN, na.rm = T))) %>%
  pivot_wider(names_from = sexo_f, values_from = ing_lab) %>% 
  add_column(egp5_mod_factor = "Total", .before = "Varón")

ingresos_laborales <- as.data.frame(rbind(ingresos_laborales, ingresos_laborales_tot)) 

ingresos_laborales <- ingresos_laborales %>% 
  mutate(ratio = round(Varón / Mujer, digits = 2))


write.xlsx(ingresos_laborales, "outputs/indicadores.xlsx", sheetName = "ingresos_clases_sexo", row.names = F, append = T)


## Impacto covid ----------------------

### Impacto x region --------------

# transferencias
transferencia <- base_hogar %>% 
  group_by(REGION) %>% 
  summarise(transferencias_covid = round(weighted.mean(transferencia, POND2R_FIN, na.rm = T)*100, digits = 2))

transferencia_tot <- base_hogar %>% 
  summarise(transferencias_covid = round(weighted.mean(transferencia, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "transferencias_covid")

transferencia_n <- base_hogar %>% 
  filter(transferencia == 1) %>% 
  summarise(transferencias_covid = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "transferencias_covid")


transferencia <- as.data.frame(rbind(transferencia, transferencia_tot, transferencia_n))

transferencia %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = transferencias_covid, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas con \ntransferencias monetarias") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/transferencias.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Despidos
despidos <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2))

despidos_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "despidos")

despidos_n <- base_hogar %>% 
  filter(despido == 1) %>% 
  summarise(despidos = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "despidos")


despidos <- as.data.frame(rbind(despidos, despidos_tot, despidos_n))

despidos %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = despidos, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \ndespedidas en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/despidos.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Suspensiones sin reducción de salario
suspensiones1 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones1_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "suspensiones1")

suspensiones1_n <- base_hogar %>% 
  filter(suspensiones1 == 1) %>% 
  summarise(suspensiones1 = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "suspensiones1")


suspensiones1 <- as.data.frame(rbind(suspensiones1, suspensiones1_tot, suspensiones1_n))

suspensiones1 %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = suspensiones1, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \nsuspendidas sin reducción \nde salario en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/suspensiones1.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Suspensiones con reducción de salario
suspensiones2 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones2_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "suspensiones2")

suspensiones2_n <- base_hogar %>% 
  filter(suspensiones2 == 1) %>% 
  summarise(suspensiones2 = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "suspensiones2")


suspensiones2 <- as.data.frame(rbind(suspensiones2, suspensiones2_tot, suspensiones2_n))

suspensiones2 %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = suspensiones2, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \nsuspendidas con reducción \nde salario en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/suspensiones2.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Suspensiones sin pago de salario
suspensiones3 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones3_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "suspensiones3")

suspensiones3_n <- base_hogar %>% 
  filter(suspensiones3 == 1) %>% 
  summarise(suspensiones3 = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "suspensiones3")


suspensiones3 <- as.data.frame(rbind(suspensiones3, suspensiones3_tot, suspensiones3_n))

suspensiones3 %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = suspensiones3, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \nsuspendidas sin pago \nde salario en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/suspensiones3.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Cambio de actividades
cambio_actividad <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2))

cambio_actividad_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "cambio_actividad")

cambio_actividad_n <- base_hogar %>% 
  filter(cambio_actividad == 1) %>% 
  summarise(cambio_actividad = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "cambio_actividad")


cambio_actividad <- as.data.frame(rbind(cambio_actividad, cambio_actividad_tot, cambio_actividad_n))

cambio_actividad %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = cambio_actividad, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \nque realizaron otras \nactividades en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/cambio_actividad.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Trabajadores que tuvieron modalidad de teletrabajo
teletrabajo <- base_hogar %>% 
  filter(M4.10B == 1) %>% 
  group_by(REGION) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2))

teletrabajo_tot <- base_hogar %>% 
  filter(M4.10B == 1) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "teletrabajo")

teletrabajo_n <- base_hogar %>% 
  filter(teletrabajo == 1) %>% 
  summarise(teletrabajo = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "teletrabajo")


teletrabajo <- as.data.frame(rbind(teletrabajo, teletrabajo_tot, teletrabajo_n))

teletrabajo %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = teletrabajo, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas que tuvo \nteletrabajo en pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/teletrabajo.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Asalariados con reducción de salario
reduccion_salario <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(REGION) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2))

reduccion_salario_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "reduccion_salario")

reduccion_salario_n <- base_hogar %>% 
  filter(reduccion_salario == 1) %>% 
  summarise(reduccion_salario = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "reduccion_salario")


reduccion_salario <- as.data.frame(rbind(reduccion_salario, reduccion_salario_tot, reduccion_salario_n))

reduccion_salario %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = reduccion_salario, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas asalariadas \ncon reducción de salario \nen pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/reduccion_salario.png", dpi = 300, type = "cairo", width = 6, height = 8)


# Cierre de negocios
cierre <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2)) %>% 
  group_by(REGION) %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2))

cierre_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2)) %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "cierre")

cierre_n <- base_hogar %>% 
  filter(cierre == 1) %>% 
  summarise(cierre = round(sum(POND2R_FIN))) %>% 
  add_column(REGION = "N", .before = "cierre")


cierre <- as.data.frame(rbind(cierre, cierre_tot, cierre_n))

cierre %>% 
  filter(REGION != "Total") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = cierre, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% personas que cerraron \nsu negocio o dejaron \nde trabajar en su oficio \nen pandemia") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/cierre.png", dpi = 300, type = "cairo", width = 6, height = 8)

#Merge region
covid_total_region <- transferencia %>% 
  left_join(despidos, by = "REGION") %>% 
  left_join(suspensiones1, by = "REGION") %>% 
  left_join(suspensiones2, by = "REGION") %>% 
  left_join(suspensiones3, by = "REGION") %>% 
  left_join(cambio_actividad, by = "REGION") %>% 
  left_join(teletrabajo, by = "REGION") %>% 
  left_join(reduccion_salario, by = "REGION") %>% 
  left_join(cierre, by = "REGION") %>% 
  as.data.frame()

write.xlsx(covid_total_region, "outputs/indicadores.xlsx", sheetName = "covid_region", row.names = F, append = T)


### Impacto x género --------------

# transferencias
transferencia <- base_hogar %>% 
  group_by(sexo_f) %>% 
  summarise(transferencias_covid = round(weighted.mean(transferencia, POND2R_FIN, na.rm = T)*100, digits = 2))

transferencia_tot <- base_hogar %>% 
  summarise(transferencias_covid = round(weighted.mean(transferencia, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "transferencias_covid")

transferencia_n <- base_hogar %>% 
  filter(transferencia == 1) %>% 
  summarise(transferencias_covid = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "transferencias_covid")


transferencia <- as.data.frame(rbind(transferencia, transferencia_tot, transferencia_n))


# Despidos
despidos <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2))

despidos_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "despidos")

despidos_n <- base_hogar %>% 
  filter(despido == 1) %>% 
  summarise(despidos = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "despidos")


despidos <- as.data.frame(rbind(despidos, despidos_tot, despidos_n))


# Suspensiones sin reducción de salario
suspensiones1 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones1_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "suspensiones1")

suspensiones1_n <- base_hogar %>% 
  filter(suspensiones1 == 1) %>% 
  summarise(suspensiones1 = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "suspensiones1")


suspensiones1 <- as.data.frame(rbind(suspensiones1, suspensiones1_tot, suspensiones1_n))


# Suspensiones con reducción de salario
suspensiones2 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones2_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "suspensiones2")

suspensiones2_n <- base_hogar %>% 
  filter(suspensiones2 == 1) %>% 
  summarise(suspensiones2 = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "suspensiones2")


suspensiones2 <- as.data.frame(rbind(suspensiones2, suspensiones2_tot, suspensiones2_n))


# Suspensiones sin pago de salario
suspensiones3 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones3_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "suspensiones3")

suspensiones3_n <- base_hogar %>% 
  filter(suspensiones3 == 1) %>% 
  summarise(suspensiones3 = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "suspensiones3")


suspensiones3 <- as.data.frame(rbind(suspensiones3, suspensiones3_tot, suspensiones3_n))


# Cambio de actividades
cambio_actividad <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2))

cambio_actividad_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "cambio_actividad")

cambio_actividad_n <- base_hogar %>% 
  filter(cambio_actividad == 1) %>% 
  summarise(cambio_actividad = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "cambio_actividad")


cambio_actividad <- as.data.frame(rbind(cambio_actividad, cambio_actividad_tot, cambio_actividad_n))


# Trabajadores que tuvieron modalidad de teletrabajo
teletrabajo <- base_hogar %>% 
  filter(M4.10B == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2))

teletrabajo_tot <- base_hogar %>% 
  filter(M4.10B == 1) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "teletrabajo")

teletrabajo_n <- base_hogar %>% 
  filter(teletrabajo == 1) %>% 
  summarise(teletrabajo = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "teletrabajo")


teletrabajo <- as.data.frame(rbind(teletrabajo, teletrabajo_tot, teletrabajo_n))


# Asalariados con reducción de salario
reduccion_salario <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  group_by(sexo_f) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2))

reduccion_salario_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "reduccion_salario")

reduccion_salario_n <- base_hogar %>% 
  filter(reduccion_salario == 1) %>% 
  summarise(reduccion_salario = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "reduccion_salario")


reduccion_salario <- as.data.frame(rbind(reduccion_salario, reduccion_salario_tot, reduccion_salario_n))


# Cierre de negocios
cierre <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2)) %>% 
  group_by(sexo_f) %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2))

cierre_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2)) %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(sexo_f = "Total", .before = "cierre")

cierre_n <- base_hogar %>% 
  filter(cierre == 1) %>% 
  summarise(cierre = round(sum(POND2R_FIN))) %>% 
  add_column(sexo_f = "N", .before = "cierre")


cierre <- as.data.frame(rbind(cierre, cierre_tot, cierre_n))

#Merge sexo
covid_total_sexo <- transferencia %>% 
  left_join(despidos, by = "sexo_f") %>% 
  left_join(suspensiones1, by = "sexo_f") %>% 
  left_join(suspensiones2, by = "sexo_f") %>% 
  left_join(suspensiones3, by = "sexo_f") %>% 
  left_join(cambio_actividad, by = "sexo_f") %>% 
  left_join(teletrabajo, by = "sexo_f") %>% 
  left_join(reduccion_salario, by = "sexo_f") %>% 
  left_join(cierre, by = "sexo_f") %>% 
  as.data.frame()

write.xlsx(covid_total_sexo, "outputs/indicadores.xlsx", sheetName = "covid_sexo", row.names = F, append = T)


### Impacto x rama de actividad --------------

# Despidos
despidos <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2))

despidos_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5)& !is.na(rama2020)) %>% 
  summarise(despidos = round(weighted.mean(despido, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "despidos")

despidos_n <- base_hogar %>% 
  filter(despido == 1) %>% 
  summarise(despidos = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "despidos")


despidos <- as.data.frame(rbind(despidos, despidos_tot, despidos_n))


# Suspensiones sin reducción de salario
suspensiones1 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones1_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  summarise(suspensiones1 = round(weighted.mean(suspensiones1, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "suspensiones1")

suspensiones1_n <- base_hogar %>% 
  filter(suspensiones1 == 1) %>% 
  summarise(suspensiones1 = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "suspensiones1")


suspensiones1 <- as.data.frame(rbind(suspensiones1, suspensiones1_tot, suspensiones1_n))


# Suspensiones con reducción de salario
suspensiones2 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones2_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  summarise(suspensiones2 = round(weighted.mean(suspensiones2, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "suspensiones2")

suspensiones2_n <- base_hogar %>% 
  filter(suspensiones2 == 1) %>% 
  summarise(suspensiones2 = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "suspensiones2")


suspensiones2 <- as.data.frame(rbind(suspensiones2, suspensiones2_tot, suspensiones2_n))


# Suspensiones sin pago de salario
suspensiones3 <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2))

suspensiones3_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  summarise(suspensiones3 = round(weighted.mean(suspensiones3, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "suspensiones3")

suspensiones3_n <- base_hogar %>% 
  filter(suspensiones3 == 1) %>% 
  summarise(suspensiones3 = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "suspensiones3")


suspensiones3 <- as.data.frame(rbind(suspensiones3, suspensiones3_tot, suspensiones3_n))


# Cambio de actividades
cambio_actividad <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2))

cambio_actividad_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  summarise(cambio_actividad = round(weighted.mean(cambio_actividad, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "cambio_actividad")

cambio_actividad_n <- base_hogar %>% 
  filter(cambio_actividad == 1) %>% 
  summarise(cambio_actividad = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "cambio_actividad")


cambio_actividad <- as.data.frame(rbind(cambio_actividad, cambio_actividad_tot, cambio_actividad_n))


# Trabajadores que tuvieron modalidad de teletrabajo
teletrabajo <- base_hogar %>% 
  filter(M4.10B == 1 & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2))

teletrabajo_tot <- base_hogar %>% 
  filter(M4.10B == 1 & !is.na(rama2020)) %>% 
  summarise(teletrabajo = round(weighted.mean(teletrabajo, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "teletrabajo")

teletrabajo_n <- base_hogar %>% 
  filter(teletrabajo == 1) %>% 
  summarise(teletrabajo = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "teletrabajo")


teletrabajo <- as.data.frame(rbind(teletrabajo, teletrabajo_tot, teletrabajo_n))


# Asalariados con reducción de salario
reduccion_salario <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  group_by(rama2020) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2))

reduccion_salario_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 3 & M3.5 <= 5) & !is.na(rama2020)) %>% 
  summarise(reduccion_salario = round(weighted.mean(reduccion_salario, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "reduccion_salario")

reduccion_salario_n <- base_hogar %>% 
  filter(reduccion_salario == 1) %>% 
  summarise(reduccion_salario = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "reduccion_salario")


reduccion_salario <- as.data.frame(rbind(reduccion_salario, reduccion_salario_tot, reduccion_salario_n))


# Cierre de negocios
cierre <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2) & !is.na(rama2020) & rama2020 != "Adm. pública") %>% 
  group_by(rama2020) %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2))

cierre_tot <- base_hogar %>% 
  filter(M4.10B == 1 & (M3.5 >= 1 & M3.5 <= 2) & !is.na(rama2020) & rama2020 != "Adm. pública") %>% 
  summarise(cierre = round(weighted.mean(cierre, POND2R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(rama2020 = "Total", .before = "cierre")

cierre_n <- base_hogar %>% 
  filter(cierre == 1) %>% 
  summarise(cierre = round(sum(POND2R_FIN))) %>% 
  add_column(rama2020 = "N", .before = "cierre")


cierre <- as.data.frame(rbind(cierre, cierre_tot, cierre_n))

#Merge rama
covid_total_rama <- despidos %>%  
  left_join(suspensiones1, by = "rama2020") %>% 
  left_join(suspensiones2, by = "rama2020") %>% 
  left_join(suspensiones3, by = "rama2020") %>% 
  left_join(cambio_actividad, by = "rama2020") %>% 
  left_join(teletrabajo, by = "rama2020") %>% 
  left_join(reduccion_salario, by = "rama2020") %>% 
  left_join(cierre, by = "rama2020") %>% 
  as.data.frame()

write.xlsx(covid_total_rama, "outputs/indicadores.xlsx", sheetName = "covid_rama", row.names = F, append = T)


## Tareas de cuidado ----------------------------
base_componentes <- base_componentes %>% 
  group_by(CUEST) %>% 
  mutate(cuidado = ifelse(M15.2 == NUM, 1, 0)) %>%
  ungroup() %>% 
  mutate(cuidado_sexo = case_when(cuidado == 1 & M2.3 == 1 ~ 0,
                                  cuidado == 1 & M2.3 == 2 ~ 1,
                                  TRUE ~ NA_real_),
         cuidado_edad = ifelse(cuidado == 1, M2.4, NA)) %>% 
  group_by(CUEST) %>% 
  mutate(cuidado_sexo_max = max(cuidado_sexo, na.rm = T),
         cuidado_edad_max = max(cuidado_edad, na.rm = T)) %>% 
  ungroup() %>%
  filter(is.finite(cuidado_sexo_max)) %>% 
  mutate(cuidado_sexo_f = factor(cuidado_sexo_max, labels = c("Varón", "Mujer")),
         edad_grupo = car::recode(as.numeric(cuidado_edad_max), "0:18='-18'; 18:25='18-25'; 
                                  26:44='26-44'; 
                                  45:65='45-65';
                                  66:100='66-'")) %>% 
  filter(ENTREVISTADO == 1)


# seleccion <- base_componentes %>%
#   select(CUEST, NUM, M2.3, M2.4, M15.2, cuidado, cuidado_sexo, cuidado_edad, edad_grupo)




### Cuidado x region ------------------
cuidados <- base_componentes %>% 
  group_by(REGION) %>% 
  summarise(cuidados = round(weighted.mean(cuidado_sexo_max, POND1R_FIN, na.rm = T)*100, digits = 2))

cuidados_tot <- base_componentes %>%
  summarise(cuidados = round(weighted.mean(cuidado_sexo_max, POND1R_FIN, na.rm = T)*100, digits = 2)) %>% 
  add_column(REGION = "Total", .before = "cuidados")

cuidados_n <- base_componentes %>% 
  filter(cuidado_sexo_max == 1) %>% 
  summarise(cuidados = round(sum(POND1R_FIN))) %>% 
  add_column(REGION = "N", .before = "cuidados")

cuidados <- as.data.frame(rbind(cuidados, cuidados_tot, cuidados_n))

cuidados %>% 
  filter(REGION != "Total", REGION != "N") %>% 
  merge(argentina, by="REGION") %>% 
  ggplot() +
  geom_sf(aes(group = REGION, fill = cuidados, geometry = geometry), 
          color = NA) +
  geom_sf(data = provincias, fill = NA, color = "white", size = 0.1) +
  theme_map_pirc() +
  labs(fill = "% de hogares con \nmujeres como principales \ncuidadoras y realizadoras \ndel trabajo del hogar") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)) + 
  scale_fill_gradient(low = "#f2cba5", high = "#874356", guide = "colourbar")

ggsave(filename = "outputs/indicadores/cuidados.png", dpi = 300, type = "cairo", width = 6, height = 8)


### Cuidado x edad
cuidados <- base_componentes %>% 
  group_by(edad_grupo, cuidado_sexo_f) %>%
  tally(POND1R_FIN) %>% 
  group_by(edad_grupo) %>% 
  mutate(porcentaje = round(n/sum(n)*100, digits = 2)) %>% 
  pivot_wider(id_cols = edad_grupo, names_from = "cuidado_sexo_f", values_from = "porcentaje")

cuidados_tot <- base_componentes %>% 
  group_by(cuidado_sexo_f) %>% 
  tally(POND1R_FIN) %>% 
  mutate(total = round(n/sum(n)*100, digits = 2)) %>% 
  select(!n) %>% 
  pivot_wider(names_from = "cuidado_sexo_f", values_from = "total") %>% 
  add_column(edad_grupo = "Total", .before = "Varón")

cuidados_tot_n <- base_componentes %>% 
  group_by(cuidado_sexo_f) %>% 
  tally(POND1R_FIN) %>% 
  pivot_wider(names_from = "cuidado_sexo_f", values_from = "n") %>% 
  add_column(edad_grupo = "N", .before = "Varón")

cuidados <- as.data.frame(rbind(cuidados, cuidados_tot, cuidados_tot_n))


write.xlsx(cuidados, "outputs/indicadores.xlsx", sheetName = "cuidados_edad", row.names = F, append = T)


### Cuidado y clase
clase <- base_hogar %>% 
  select(CUEST, egp5_mod_factor_psh)

base_componentes <- base_componentes %>% 
  left_join(clase, by = "CUEST")

cuidados <- base_componentes %>% 
  filter(!is.na(egp5_mod_factor_psh)) %>% 
  group_by(egp5_mod_factor_psh, cuidado_sexo_f) %>%
  tally(POND1R_FIN) %>% 
  group_by(egp5_mod_factor_psh) %>% 
  mutate(porcentaje = round(n/sum(n)*100, digits = 2)) %>% 
  pivot_wider(id_cols = egp5_mod_factor_psh, names_from = "cuidado_sexo_f", values_from = "porcentaje")

cuidados_tot <- base_componentes %>%
  filter(!is.na(egp5_mod_factor_psh)) %>%
  group_by(cuidado_sexo_f) %>% 
  tally(POND1R_FIN) %>% 
  mutate(total = round(n/sum(n)*100, digits = 2)) %>% 
  select(!n) %>% 
  pivot_wider(names_from = "cuidado_sexo_f", values_from = "total") %>% 
  add_column(egp5_mod_factor_psh = "Total", .before = "Varón")

cuidados_tot_n <- base_componentes %>% 
  filter(!is.na(egp5_mod_factor_psh)) %>%
  group_by(cuidado_sexo_f) %>% 
  tally(POND1R_FIN) %>% 
  pivot_wider(names_from = "cuidado_sexo_f", values_from = "n") %>% 
  add_column(egp5_mod_factor_psh = "N", .before = "Varón")

cuidados <- as.data.frame(rbind(cuidados, cuidados_tot, cuidados_tot_n))


write.xlsx(cuidados, "outputs/indicadores.xlsx", sheetName = "cuidados_clase", row.names = F, append = T)
