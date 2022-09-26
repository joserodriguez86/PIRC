rm(list = ls())
pacman::p_load(tidyverse, janitor, xlsx)

#Base y modificaciones de variables --------------------
base_2021 <- read_sav("base_hogar.sav")
load("E:/OneDrive/Otros/proyectos_R/articulo_enes/enes_hog.RData")
base_2014 <- enes_hog
rm(enes_hog)

base_2021$clase_sub <- factor(base_2021$M11.6, labels = c("Clase alta", "Clase media alta", "Clase media", "Clase media baja", "Clase trabajadora", "Clase baja", "Ns/Nc"))

base_2014$clase_sub <- car::recode(base_2014$v260a, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
base_2014$clase_sub2 <- car::recode(base_2014$v261a, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
base_2014$clase_sub <- ifelse(is.na(base_2014$clase_sub), base_2014$clase_sub2, base_2014$clase_sub)

base_2014$clase_sub <- factor(base_2014$clase_sub, labels = c("Clase alta", "Clase media alta", "Clase media", "Clase media baja", "Clase obrera", "Clase baja"))

#Pruebas -------------------
tabla1 <- base_2021 %>% 
  group_by(clase_sub) %>% 
  tally(PONDERA1)
sum_tab1 <- c("N", round(colSums(tabla1[,-1])))
tabla1 <- tabla1 %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>% 
  rbind(sum_tab1)


tabla2 <- base_2014 %>% 
  group_by(clase_sub) %>% 
  tally(f_calib3)
sum_tab2 <- c("N", round(colSums(tabla2[,-1])))
tabla2 <- tabla2 %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>% 
  rbind(sum_tab2)

write.xlsx(tabla1, "clase_subjetiva.xlsx", sheetName = "2021")
write.xlsx(tabla2, "clase_subjetiva.xlsx", append = TRUE, sheetName = "2014")
