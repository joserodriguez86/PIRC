rm(list = ls())
load("bases/base_hogar_imputados.RData")

pacman::p_load(tidyverse)
options(scipen=999)

# COBHE para base_hogar ------------------

##Separo dígitos del CNO-----
base_hogar$cno12 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 1, 2), str_sub(base_hogar$CNO_PSH, 1, 1))
base_hogar$cno3 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 3, 3), str_sub(base_hogar$CNO_PSH, 2, 2))
base_hogar$cno4 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 4, 4), str_sub(base_hogar$CNO_PSH, 3, 3))
base_hogar$cno5 <- ifelse(nchar(base_hogar$CNO_PSH) > 4, str_sub(base_hogar$CNO_PSH, 5, 5), str_sub(base_hogar$CNO_PSH, 4, 4))

base_hogar$cno12 <- as.numeric(base_hogar$cno12)
base_hogar$cno3 <- as.numeric(base_hogar$cno3)
base_hogar$cno4 <- as.numeric(base_hogar$cno4)
base_hogar$cno5 <- as.numeric(base_hogar$cno5)

##Categoría ocupacional
base_hogar <- base_hogar %>% 
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


## Tamaño del establecimiento------
base_hogar <- base_hogar %>% 
  mutate(tamano_psh = case_when(M12.6 >= 1 & M12.6 < 3 ~ 1,
                            M12.6 >= 3 ~ 2,
                            TRUE ~ NA_real_),
         tamano_enc = case_when(M3.6 >= 1 & M3.6 < 3 ~ 1,
                                M3.6 >= 3 ~ 2,
                                TRUE ~ NA_real_),
         tamano = ifelse(RELACION == 1, tamano_enc, tamano_psh))

## Clasificador --------------

base_hogar <- base_hogar %>% 
  mutate(cobhe = case_when(#Clase I: propietarios > 5 y directivos, gerentes, funcionarios de dirección
    (cno12 >= 0 & cno12 <= 4) & tamano == 2 ~ 1, 
    (cno12 == 6 | cno12 == 7) & tamano == 2 ~ 1,
    categoria == 1 & tamano == 2 ~ 1,
    
    #Clase II: propietarios < 5 y directivos, gerentes, funcionarios de dirección  
    cno12 == 5 ~ 2,
    categoria == 1 & tamano == 1 ~ 2,
    (cno12 == 6 | cno12 == 7) & tamano == 1 ~ 2,
    (cno12 >= 0 & cno12 <= 4) & tamano == 1 ~ 2,
    
    #Clase III: cuenta propias profesionales/calificados
    (cno12 == 32 | cno12 == 35 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & cno5 < 3 ~ 3,
    
    (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & cno5 <= 3 ~ 3,
    
    cno12 == 34 & categoria == 2 & cno5 <= 2 ~ 3,
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
    
    #Clase IV: trabajadores no manuales > 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 30 & categoria == 3 & tamano == 2 ~ 4,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 2 ~ 4,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    cno12 == 81 & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 91 & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 == 1 ~ 4,
    
    #Clase V: trabajadores manuales > 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    cno12 == 34 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 51 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 53 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 2 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 2  ~ 5,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 > 1 ~ 5,
    
    #Clase VI: trabajadores no manuales < 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 30 & categoria == 3 & tamano == 1 ~ 6,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 1 ~ 6,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    cno12 == 81 & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 91 & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 == 1 ~ 6,
    
    #Clase VII: trabajadores manuales < 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    cno12 == 34 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 51 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 53 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 1 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 1  ~ 7,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 > 1 ~ 7,
    cno12 == 55 ~ 7,
    
    #Clase VIII: Cuenta propia semi-calificados y no calificados
    (cno12 == 10 | cno12 == 32 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & (cno5 == 3 | cno5 == 4) ~ 8,
    
    (cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & (cno5 == 4) ~ 8,
    
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
    cno12 == 36 & categoria == 2 ~ 8,
    cno12 == 56 & categoria == 2 ~ 8,
    cno12 == 33 ~ 8,
    categoria == 2 & cno5 == 4 ~ 8,
    
    #Clase IX: Desocupados
    M2.12 == 2 ~ 9,
    
    #Clase X: Inactivos
    M2.12 == 3 ~ 10
  ))

base_hogar$cobhe_f <- factor(base_hogar$cobhe, 
                      labels = c('Propietarios y directivos >5',
                                 'Propietarios y directivos <5',
                                 'Cuenta propia profesionales/calificados',
                                 'Trabajadores no manuales > 5',
                                 'Trabajadores manuales > 5',
                                 'Trabajadores no manuales <5',
                                 'Trabajadores manuales <5',
                                 'Cuenta propia semi-calif y no calificados',
                                 'Desocupados',
                                 'Inactivos'))

# Guardado de la base-----
save(base_hogar, file = "bases/base_hogar_imputados+cobhe.RData")

base_seleccion <- base_hogar %>% 
  select(CUEST, cno12, cobhe)

write.csv2(base_seleccion, "outputs/base_cobhe.csv", row.names = FALSE)
