rm(list = ls())
pacman::p_load(tidyverse, haven, VIM, rpart, rpart.plot)
options(scipen=999)

# Carga de la base--------------
base_hogar <- read_sav("bases/base_hogar.sav")
base_componentes <- read_sav("bases/base_componentes.sav", encoding = "latin1")

base_componentes_estado <- base_componentes %>% 
  filter(ENTREVISTADO == 1) %>%
  select(CUEST, M2.3, M2.4, M2.5, M2.7, M2.12, M2.9, M2.10)
  
base_hogar <- base_hogar %>% 
  left_join(base_componentes_estado, by = "CUEST")

rm(base_componentes, base_componentes_estado)

base_hogar <- base_hogar %>% 
  mutate(M3.22 = na_if(M3.22, 980000),
         M3.22 = na_if(M3.22, 999999),
         M3.23 = na_if(M3.23, 99),
         ing_ocup_mis = case_when(is.na(M3.22) & is.na(M3.23) ~ "perdido",
                                  is.na(M3.22) & !is.na(M3.23) ~ "perdido con rango",
                                  TRUE ~ "no perdido"))

base_hogar$M3.23 <- car::recode(base_hogar$M3.23, "15:16 = 14") 
base_hogar$M3.23_f <- factor(base_hogar$M3.23, labels = c("No tuvo ingresos", "$0 a $25.000",
                                                          "$25.001 a $40.000", "$40.001 a $60.000",
                                                          "$60.001 a $90.000", "$90.001 a $120.000",
                                                          "$120.001 y 150.000", "$150.001 y 180.000",
                                                          "$180.001 y 210.000", "$210.001 y 250.000",
                                                          "$250.001 y 300.000", "$ 300.001 a 400.000",
                                                          "$400.001 a 600.00", "Más de $ 600.000"))

base_hogar <- base_hogar %>% 
  mutate(rango_ingresos_ocup = case_when(M3.22 == 0 ~ "No tuvo ingresos",
                                         M3.22 > 0 & M3.22 <= 25000 ~ "$0 a $25.000",
                                         M3.22 > 25000 & M3.22 <= 40000 ~ "$25.001 a $40.000",
                                         M3.22 > 40000 & M3.22 <= 60000 ~ "$40.001 a $60.000",
                                         M3.22 > 60000 & M3.22 <= 90000 ~ "$60.001 a $90.000",
                                         M3.22 > 90000 & M3.22 <= 120000 ~ "$90.001 a $120.000",
                                         M3.22 > 120000 & M3.22 <= 150000 ~ "$120.001 y 150.000",
                                         M3.22 > 150000 & M3.22 <= 180000 ~ "$150.001 y 180.000",
                                         M3.22 > 180000 & M3.22 <= 210000 ~ "$180.001 y 210.000",
                                         M3.22 > 210000 & M3.22 <= 250000 ~ "$210.001 y 250.000",
                                         M3.22 > 250000 & M3.22 <= 300000 ~ "$250.001 y 300.000",
                                         M3.22 > 300000 & M3.22 <= 400000 ~ "$ 300.001 a 400.000",
                                         M3.22 > 400000 & M3.22 <= 600000 ~ "$400.001 a 600.000",
                                         M3.22 > 600000 ~ "Más de $ 600.000"))

base_hogar$rango_ingresos_ocup <- ifelse(is.na(base_hogar$rango_ingresos_ocup), 
                                         as.character(base_hogar$M3.23_f),
                                         base_hogar$rango_ingresos_ocup)

base_hogar$M2.5_f <- factor(base_hogar$M2.5, labels = c("PSH", "Cónyuge", "Hijo/a", "Hijastro/a",
                                                        "Yerno / Nuera", "Hermano/a", "Nieto/a",
                                                        "Padre/ Madre / Suegro / Suegra", "Otros"))

base_hogar$M2.3_f <- factor(base_hogar$M2.3, labels = c("Varón", "Mujer", "Otro"))

base_hogar$M2.7_f <- factor(base_hogar$M2.7, labels = c("Obra social", "Prepaga", "PAMI", "Hospital público", "Ns/Nc"))

base_hogar <- base_hogar %>% 
  mutate(nivel_ed = case_when(M2.9 == 0 | M2.9 == 1 | M2.9 == 10 ~ 1,
                              (M2.9 == 2 | M2.9 == 3 | M2.9 == 9) & M2.10 >= 2 ~ 2,
                              (M2.9 == 2 | M2.9 == 3 | M2.9 == 9) & M2.10 == 1 ~ 3,
                              (M2.9 == 4 | M2.9 == 5) & M2.10 >= 2 ~ 4,
                              (M2.9 == 4 | M2.9 == 5) & M2.10 == 1 ~ 5,
                              (M2.9 >= 6 & M2.9 <= 8) & M2.10 >= 2 ~ 6,
                              (M2.9 >= 6 & M2.9 <= 8) & M2.10 == 1 ~ 7,
                              TRUE ~ 99))

base_hogar$nivel_ed_f <- factor(base_hogar$nivel_ed, labels = c("Nivel inicial", "Primario incompleto",
                                                    "Primario completo", "Secundario incompleto",
                                                    "Secundario completo", "Superior incompleto", 
                                                    "Superior completo"))

base_hogar$M3.21_f <- factor(base_hogar$M3.21, labels = c("Si", "No", "Ns/Nc"))

base_hogar$M3.5_f <- car::recode(base_hogar$M3.5, "3:5 = 3")
base_hogar$M3.5_f <- factor(base_hogar$M3.5_f, labels = c("Patrón", "Cuenta propia", "Empleado"))

base_hogar$calificacion <- str_sub(base_hogar$CNO_encuestado, 5, 5)
base_hogar$calificacion <- car::recode(base_hogar$calificacion, "7:9 = 9; '' = NA")
base_hogar$calificacion_f <- factor(base_hogar$calificacion, labels = c("Profesional",
                                                                        "Técnico",
                                                                        "Operativo",
                                                                        "No calificado",
                                                                        "Ns/Nc"))

base_hogar$M3.6_f <- factor(base_hogar$M3.6, labels = c("1 personas",
                                                        "2 a 5",
                                                        "6 a 10",
                                                        "11 a 50",
                                                        "51 a 200",
                                                        "Más de 200",
                                                        "Ns/Nc"
                                                        ))
base_hogar$M3.10 <- car::recode(base_hogar$M3.10, "NA= 99")
base_hogar$M3.10_f <- factor(base_hogar$M3.10, labels = c("Le aportan",
                                                          "Aporta",
                                                          "No aporta ni le aportan",
                                                          "Ns/Nc")) 


# Imputación de ingresos (ocupación principal)------------------------------------------------
base_hogar_ocup <- subset(base_hogar, M2.12 == 1)

base_hogar_ocup %>% 
  select(M3.22, M3.23) %>% 
  summary()


## Cruces ------------
ingresos_cruce1 <- as.data.frame.matrix(table(base_hogar_ocup$M3.22, base_hogar_ocup$M3.23, 
                                              useNA = "always"))

ing_catocup <- table(base_hogar_ocup$M3.5, base_hogar_ocup$ing_ocup_mis) # categoría ocupacional
round(prop.table(ing_catocup, 1)*100, digits = 2)

ing_niveled <- table(base_hogar_ocup$M2.9, base_hogar_ocup$ing_ocup_mis) # nivel educativo
round(prop.table(ing_niveled, 1)*100, digits = 2)

ing_region <- table(base_hogar_ocup$REGION, base_hogar_ocup$ing_ocup_mis) # region
round(prop.table(ing_region, 1)*100, digits = 2)


## Imputación -----------
### 1) Random Hot Deck para los ingresos con rango -----
summarytools::descr(base_hogar_ocup$M3.22)


set.seed(971986)
base_imputacion1 <- hotdeck(data = base_hogar_ocup,
                           variable = "M3.22",
                           domain_var = "rango_ingresos_ocup"
                           )
view(base_imputacion1)

base_imputacion1 %>% 
  ggplot() +
  geom_density(aes(M3.22, fill = ing_ocup_mis, color = ing_ocup_mis), alpha = 0.3) +
  geom_density(aes(M3.22, fill = "red", color = "red"), alpha = 0.3) +
  scale_color_manual(labels = c("Solo ingresos \ndirectos", "Solo Rangos \nimputados", "Ingresos directos + \nrangos imputados"), values = c("blue", "red", "green"))
  

### 2) Árbol de regresión + hot deck para ingresos con pérdida total ------
base_imputacion2 <- subset(base_imputacion1, !is.na(M3.22)) 


arbol <- rpart(
  formula = M3.22 ~ REGION + M2.5_f + M2.4 + M2.3_f + M2.7_f + nivel_ed_f + M3.19 + M3.21_f + M3.5_f + calificacion_f + M3.6_f + M3.10_f,
  data    = base_imputacion2, 
  method  = "anova" 
  )

rpart.plot(arbol, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE, digits=-3) 
plotcp(arbol)

arbol.pruned <- prune(arbol, cp = arbol$cptable[which.min(arbol$cptable[, "xerror"]), "CP"])
rpart.plot(arbol.pruned, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE, digits=-3)

prediction <- predict(arbol.pruned, base_imputacion2)
output <- data.frame(base_imputacion2$M3.22, prediction)

rpart.rules(arbol.pruned, style = "tall", nn = TRUE)

####2.1) Creación de los grupos a partir de las reglas---------------
base_imputacion1 <- base_imputacion1 %>% 
  mutate(grupos_arbol = case_when(M2.7 >= 3 & M3.19 < 39 ~ 1,
                                  M2.7 < 3 & M3.19 < 40 & M3.10 >= 2 ~ 2,
                                  M2.7 >= 3 & M3.19 >= 39 ~ 3,
                                  M2.7 < 3 & M3.19 < 40 & M3.10 == 1 ~ 4,
                                  M2.7 < 3 & M3.19 >= 40 & calificacion > 1 & 
                                    REGION %in% c("CENTRO", "CUYO", "NEA", "NOA") ~ 5,
                                  
                                  M2.7 < 3 & M3.19 >= 40 & calificacion > 1 & 
                                    REGION %in% c("GBA", "PAMPEANA", "PATAGONICA") ~ 6,
                                  
                                  M2.7 < 3 & M3.19 >= 40 & calificacion == 1 & 
                                    REGION %in% c("CENTRO", "CUYO", "NEA", "NOA") ~ 7,
                                  
                                  M2.7 < 3 & M3.19 >= 40 & calificacion == 1 & 
                                    REGION %in% c("GBA", "PAMPEANA", "PATAGONICA") &
                                    (M3.6 == 1 | (M3.6 >= 3 & M3.6 <= 5)) ~ 8,
                                  
                                  M2.7 < 3 & M3.19 >= 40 & calificacion == 1 & 
                                    REGION %in% c("GBA", "PAMPEANA", "PATAGONICA") &
                                    (M3.6 == 2 | M3.6 >= 6) ~ 9
                                  ))

#### 2.2) Hot deck aleatorio en los grupos del árbol -------
set.seed(971986)
base_imputacion1 <- hotdeck(data = base_imputacion1,
                            variable = "M3.22",
                            domain_var = "grupos_arbol"
)

#### 2.3) Pego en base hogar y comparo datos--------------
base_imputacion1 <- base_imputacion1 %>% 
  mutate(ing_lab_imp = M3.22) %>% 
  select(CUEST, ing_lab_imp)

base_hogar <- base_hogar %>% 
  left_join(base_imputacion1, by = "CUEST")

base_hogar %>%
  select(M3.22, ing_lab_imp) %>% 
  gather(ingreso, valor) %>%
  mutate(ingreso = case_when(ingreso == "M3.22" ~ "Sin imputar",
                             ingreso == "ing_lab_imp" ~ "Imputado")) %>% 
  ggplot(aes(valor, fill = ingreso, color = ingreso, group = rev(ingreso))) +
  geom_density(alpha = 0.3)

ggsave(filename = "graficos/imputados_ocupacion_princ.jpg", dpi = 300, type = "cairo", width = 8, height = 5)

base_hogar_inglab_imp <- base_hogar

save(base_hogar_inglab_imp, file = "bases/base_hogar_inglab_imp.RData")


