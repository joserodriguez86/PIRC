# Librerías y cargas de bases---------------------
rm(list=ls())
pacman::p_load(tidyverse, eph, haven, xlsx)

load("bases/base_hogar_imputados.RData")
base_componentes <- read_sav("bases/base_componentes.sav", encoding = "latin1")

base_componentes <- base_componentes %>% 
  select(CUEST, M2.3, M2.4, M2.5, M2.13, ENTREVISTADO)

base_mes <- read.csv("bases/mes.csv", header = T, sep = ";", colClasses=c("CUEST" = "character"), 
                     fileEncoding = "UTF-8-BOM")

canastas <- read.xlsx("bases/canastas_pirc.xlsx", sheetName = "Hoja1")


# Cálculo de pobreza --------------------
## 1) Unión de bases de hogar y componentes-----------------
base_pirc <- base_componentes %>% 
  left_join(base_hogar, by = "CUEST") %>% 
  left_join(base_mes, by = "CUEST")

rm(base_componentes, base_hogar, base_mes)

base_pirc <- base_pirc %>% 
  mutate(region_eph = case_when(PROVINCIA %in% c("Partidos", "CABA"
                                                 ) ~ "GBA",
                                PROVINCIA %in% c("Catamarca", "Jujuy", 
                                                 "La Rioja", "Salta",
                                                 "Santiago",
                                                 "Tucumán", "Tucuman") ~ "NOA",
                                PROVINCIA %in% c("Corrientes", "Chaco", "Misiones",
                                                 "Formosa") ~ "NEA",
                                PROVINCIA %in% c("Mendoza", "San Juan",
                                                 "San Luis") ~ "Cuyo",
                                PROVINCIA %in% c("Buenos Aires", "La Pampa", "Cordoba",
                                                 "Córdoba", "Entre Rios", "Santa Fe") ~ "Pampeana",
                                PROVINCIA %in% c("Rio Negro", "Neuquen", "Chubut", "Santa Cruz",
                                                 "T de Fuego") ~ "Patagonia"))
 
## 2) Variables para determinar la pobreza y la indigencia -------------

###Valores adultos equivalentes-----------
aq <- adulto_equivalente
names(aq)[names(aq) == "CH06"] <- "M2.4"
aq$M2.4[aq$M2.4 == "-1"] <- 0

names(aq)[names(aq) == "CH04"] <- "M2.3"
base_pirc$M2.3[base_pirc$M2.3 == 666666] <- 2

base_pirc$M2.3 <- as.numeric(base_pirc$M2.3)
base_pirc$M2.4 <- as.numeric(base_pirc$M2.4)

base_pirc <- base_pirc %>% 
  left_join(aq, by = c("M2.3", "M2.4")) 

base_pirc <- base_pirc %>% 
  group_by(CUEST) %>% 
  mutate(adequi_sum = sum(adequi)) %>% 
  ungroup()

### Agrego canastas y engel-----
base_pirc <- base_pirc %>% 
  left_join(canastas, by = c("region_eph", "mes"))


## 3) LP y LI------------
base_pirc <- base_pirc %>% 
  mutate(linea_pobreza = adequi_sum*CBA*engel,
         linea_indigencia = adequi_sum*CBA,
         pobreza = case_when(linea_pobreza < itf_imp ~ 0,
                             linea_pobreza >= itf_imp ~ 1),
         indigencia = case_when(linea_indigencia < itf_imp ~ 0,
                             linea_indigencia >= itf_imp ~ 1),
         condicion = factor(pobreza, labels = c("No pobre", "Pobre")))

rm(aq, canastas)

# COBHE -----------------
##Separo dígitos del CNO
base_pirc$cno12 <- ifelse(nchar(base_pirc$CNO_PSH) > 4, str_sub(base_pirc$CNO_PSH, 1, 2), str_sub(base_pirc$CNO_PSH, 1, 1))
base_pirc$cno3 <- ifelse(nchar(base_pirc$CNO_PSH) > 4, str_sub(base_pirc$CNO_PSH, 3, 3), str_sub(base_pirc$CNO_PSH, 2, 2))
base_pirc$cno4 <- ifelse(nchar(base_pirc$CNO_PSH) > 4, str_sub(base_pirc$CNO_PSH, 4, 4), str_sub(base_pirc$CNO_PSH, 3, 3))
base_pirc$cno5 <- ifelse(nchar(base_pirc$CNO_PSH) > 4, str_sub(base_pirc$CNO_PSH, 5, 5), str_sub(base_pirc$CNO_PSH, 4, 4))

base_pirc$cno12 <- as.numeric(base_pirc$cno12)
base_pirc$cno3 <- as.numeric(base_pirc$cno3)
base_pirc$cno4 <- as.numeric(base_pirc$cno4)
base_pirc$cno5 <- as.numeric(base_pirc$cno5)

##Categoría ocupacional
base_pirc <- base_pirc %>% 
  mutate(categoria_psh = case_when(M12.5 == 1 & cno3 == 0 ~ 1,
                                   M12.5 == 2 | cno3 == 1 ~ 2,
                                   (M12.5 == 3 | M12.5 == 4) | (cno3 == 2 | cno3 == 3) ~ 3,
                                   (M12.5 == 3 | M12.5 == 4) & (cno3 == 0) ~ 3,
                                   M12.5 == 99 ~ 99),
         categoria_enc = case_when(M3.5 == 1 & cno3 == 0 ~ 1,
                                   M3.5 == 2 | cno3 == 1 ~ 2,
                                   (M3.5 >= 3 & M3.5 <= 5) | (cno3 == 2 | cno3 == 3) ~ 3,
                                   (M3.5 >= 3 & M3.5 <= 5) & (cno3 == 0) ~ 3,
                                   M3.5 == 99 ~ 99),
         categoria = ifelse(RELACION == 1, categoria_enc, categoria_psh))


## Tamaño del establecimiento
base_pirc <- base_pirc %>% 
  mutate(tamano_psh = case_when(M12.6 >= 1 & M12.6 < 3 ~ 1,
                                M12.6 >= 3 ~ 2,
                                TRUE ~ NA_real_),
         tamano_enc = case_when(M3.6 >= 1 & M3.6 < 3 ~ 1,
                                M3.6 >= 3 ~ 2,
                                TRUE ~ NA_real_),
         tamano = ifelse(RELACION == 1, tamano_enc, tamano_psh))

## Clasificador

base_pirc <- base_pirc %>% 
  mutate(cobhe = case_when(#Clase I: propietarios > 5 y directivos, gerentes, funcionarios de dirección
    (cno12 >= 0 & cno12 <= 4) & tamano == 2 & M2.12 == 1 ~ 1, 
    (cno12 == 6 | cno12 == 7) & tamano == 2 & M2.12 == 1 ~ 1,
    categoria == 1 & tamano == 2 & M2.12 == 1 ~ 1,
    
    #Clase II: propietarios < 5 y directivos, gerentes, funcionarios de dirección  
    cno12 == 5 & M2.12 == 1 ~ 2,
    categoria == 1 & tamano == 1 & M2.12 == 1 ~ 2,
    (cno12 == 6 | cno12 == 7) & tamano == 1 & M2.12 == 1 ~ 2,
    (cno12 >= 0 & cno12 <= 4) & tamano == 1 & M2.12 == 1 ~ 2,
    
    #Clase III: cuenta propias profesionales/calificados
    (cno12 == 32 | cno12 == 35 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & cno5 < 3 & M2.12 == 1 ~ 3,
    
    (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & cno5 <= 3 & M2.12 == 1 ~ 3,
    
    cno12 == 34 & categoria == 2 & cno5 <= 2 & M2.12 == 1 ~ 3,
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 > 1 & M2.12 == 1 ~ 3,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 > 1 & M2.12 == 1 ~ 3,
    
    #Clase IV: trabajadores no manuales > 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    cno12 == 30 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 <= 3 & M2.12 == 1 ~ 4,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 <= 3 & M2.12 == 1 ~ 4,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 <= 2 & M2.12 == 1 ~ 4,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 <= 2 & M2.12 == 1 ~ 4,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 <= 2 & M2.12 == 1 ~ 4,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 <= 3 & M2.12 == 1 ~ 4,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 <= 2 & M2.12 == 1 ~ 4,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 4,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 4,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 4,
    cno12 == 81 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    cno12 == 91 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 4,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 == 1 & M2.12 == 1 ~ 4,
    
    #Clase V: trabajadores manuales > 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 == 4 & M2.12 == 1 ~ 5,
    cno12 == 34 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 5,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 == 4 & M2.12 == 1 ~ 5,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    cno12 == 51 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 5,
    cno12 == 53 & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 5,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 == 4 & M2.12 == 1 ~ 5,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 5,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 2 & M2.12 == 1 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 > 2 & M2.12 == 1 ~ 5,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 2  & M2.12 == 1 ~ 5,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 > 1 & M2.12 == 1 ~ 5,
    
    #Clase VI: trabajadores no manuales < 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    cno12 == 30 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 <= 3 & M2.12 == 1 ~ 6,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 <= 3 & M2.12 == 1 ~ 6,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 <= 2 & M2.12 == 1 ~ 6,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 <= 2 & M2.12 == 1 ~ 6,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 <= 2 & M2.12 == 1 ~ 6,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 <= 3 & M2.12 == 1 ~ 6,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 <= 2 & M2.12 == 1 ~ 6,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 6,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 6,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 & M2.12 == 1 ~ 6,
    cno12 == 81 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    cno12 == 91 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 6,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 == 1 & M2.12 == 1 ~ 6,
    is.na(tamano) & M3.5 == 5 & M2.12 == 1 ~ 7,
    
    #Clase VII: trabajadores manuales < 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 == 4 & M2.12 == 1 ~ 7,
    cno12 == 34 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 7,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 == 4 & M2.12 == 1 ~ 7,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    cno12 == 51 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 7,
    cno12 == 53 & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 7,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 == 4 & M2.12 == 1 ~ 7,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 7,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 1 & M2.12 == 1 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 & M2.12 == 1 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 > 2 & M2.12 == 1 ~ 7,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 1  & M2.12 == 1 ~ 7,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 > 1 & M2.12 == 1 ~ 7,
    cno12 == 55 & M2.12 == 1 ~ 7,
    
    #Clase VIII: Cuenta propia semi-calificados y no calificados
    (cno12 == 10 | cno12 == 32 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & (cno5 == 3 | cno5 == 4) & M2.12 == 1 ~ 8,
    
    (cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & (cno5 == 4) & M2.12 == 1 ~ 8,
    
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 == 1 & M2.12 == 1 ~ 8,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 == 1 & M2.12 == 1 ~ 8,
    cno12 == 36 & categoria == 2 & M2.12 == 1 ~ 8,
    cno12 == 56 & categoria == 2 & M2.12 == 1 ~ 8,
    cno12 == 33 & M2.12 == 1 ~ 8,
    categoria == 2 & cno5 == 4 & M2.12 == 1 ~ 8,
    
    #Clase IX: Desocupados / inactivos
    M2.12 == 2 | (M2.12 == 3 & M2.13 > 1) ~ 9,
    
    #Clase X: Inactivos jubilados
    M2.12 == 3 & M2.13 == 1 ~ 10
  ))

base_pirc$cobhe_f <- factor(base_pirc$cobhe, 
                             labels = c('Propietarios y directivos >5',
                                        'Propietarios y directivos <5',
                                        'Cuenta propia profesionales/calificados',
                                        'Trabajadores no manuales > 5',
                                        'Trabajadores manuales > 5',
                                        'Trabajadores no manuales <5',
                                        'Trabajadores manuales <5',
                                        'Cuenta propia no calificados',
                                        'Desocupados / inactivos',
                                        'Inactivos jubilados'))

# Otras variables ---------------------------------------
base_pirc$sexo_f <- factor(base_pirc$M2.3, labels = c("Varón", "Mujer"))

base_pirc$edad_grupo <- car::recode(base_pirc$M2.4, "18:29=1; 30:39=2; 40:65=3;
                                    66:hi=4; else=NA")

base_pirc$edad_grupo <- factor(base_pirc$edad_grupo, labels = c("18-29", "30-39", "40-65", ">66"))

base_pirc$formal <- case_when(base_pirc$M3.10 == 1 | base_pirc$M3.10 == 2 | base_pirc$M3.15 == 3 ~ 1,
                              TRUE ~ 0)

base_pirc$desocup2 <- as.numeric(car::recode(base_pirc$M4.17, "1=1; 2=0; else=0"))

base_pirc$ife <- ifelse(base_pirc$M5.2_1 == 1, 1, 0)
base_pirc$potenciar <- ifelse(base_pirc$M5.2_2 == 1, 1, 0)
base_pirc$atp <- ifelse(base_pirc$M5.2_3 == 1, 1, 0)

base_pirc$teletrabajo <- case_when(base_pirc$M5.4A_6 == 1 ~ 1,
                                   base_pirc$M5.4B_6 == 1 ~ 1,
                                   TRUE ~ 0)

base_pirc <- base_pirc %>% 
  mutate(covid_cierre_despido = case_when(M5.4A_1 == 1 | M5.4A_4 == 1 | M5.4B_1 == 1 ~ 1,
                                          TRUE ~ 0),
         covid_baja_ingresos = case_when(M5.4A_3 == 1 | M5.4A_7 == 1 | M5.4B_3 == 1 | M5.4B_5 == 1 ~ 1,
                                         TRUE ~ 0),
         covid_cambio = case_when(M5.4A_5 == 1 | M5.4B_2 == 1 | M5.4A_4 == 1 ~ 1,
                                  TRUE ~ 0))

base_pirc <- base_pirc %>% 
  mutate(tray_estado = case_when(M4.10A == 1 & M4.10B == 1 & M2.12 == 1 ~ 1,
                                 M4.10A >= 2 & M4.10B == 1 & M2.12 == 1 ~ 1,
                                 M4.10A == 1 & M4.10B >= 2 & M2.12 == 1 ~ 1,
                                 M4.10A >= 1 & M4.10B >= 2 & M2.12 == 1 ~ 2,
                                 M4.10A == 1 & M4.10B >= 2 & M2.12 >=2 ~ 3,
                                 M4.10A == 1 & M4.10B == 1 & M2.12 >=2 ~ 4,
                                 M4.10A >= 2 & M4.10B >= 2 & M2.12 >=2 ~ 5),
         tray_estado_f = factor(tray_estado, labels = c("Ocupados regulares", "Ocupados recientes",
                                                        "Desoc/inac 2020",
                                                        "Desoc/inac 2021",
                                                        "Desoc/inac regulares")))

# Guarda base --------------
base_pirc <- subset(base_pirc, ENTREVISTADO == 1)

save(base_pirc, file = "bases/base_libro.RData")
