rm(list = ls())
pacman::p_load(tidyverse, haven, VIM, rpart, rpart.plot, ggridges)
options(scipen=999)

# Carga de la base--------------
base_hogar <- read_sav("bases/base_hogar.sav")
base_componentes <- read_sav("bases/base_componentes.sav", encoding = "latin1")
load("bases/base_hogar_inglab_imp.RData")

base_componentes <- base_componentes %>% 
  filter(M2.5 == 1) %>%
  select(CUEST, M2.7, M2.12, M2.10, M2.9)

base_hogar <- base_hogar %>% 
  left_join(base_componentes, by = "CUEST")

base_hogar_inglab_imp <- base_hogar_inglab_imp %>% 
  select(CUEST, ing_lab_imp)

rm(base_componentes)

base_hogar <- base_hogar %>% 
  mutate(M8.4 = na_if(M8.4, 980000),
         M8.4 = na_if(M8.4, 999999),
         M8.5 = na_if(M8.5, 99),
         ing_fam_mis = case_when(is.na(M8.4) & is.na(M8.5) ~ "perdido",
                                 is.na(M8.4) & !is.na(M8.5) ~ "perdido con rango",
                                 TRUE ~ "no perdido"))

base_hogar$M2.7_f <- factor(base_hogar$M2.7, labels = c("Obra social", "Prepaga", "PAMI", "Hospital público", "Ns/Nc"))

base_hogar$M2.12_f <- factor(base_hogar$M2.12, labels = c("Ocupado", "Desocupado", "Inactivo"))

base_hogar <- base_hogar %>% 
  mutate(nivel_ed = case_when(M2.9 == 0 | M2.9 == 1 | M2.9 == 2 | M2.9 == 3 | M2.9 == 9 | M2.9 == 10 ~ 1,
                              M2.9 == 4 | M2.9 == 5 ~ 2,
                              M2.9 >= 6 & M2.9 <= 8 ~ 3,
                              TRUE ~ 99))

base_hogar$nivel_ed_f <- factor(base_hogar$nivel_ed, labels = c("Primario", "Secundario",
                                                                "Superior"))

base_hogar$M1.2 <- car::recode(base_hogar$M1.2, "1=1; 4=2; 2:3 = 3; 5:6 = 3")
base_hogar$M1.2_f <- factor(base_hogar$M1.2, labels = c("Casa", "Departamento", "Otro"))

base_hogar <- base_hogar %>% 
  mutate(fuente_laboral = case_when(M8.3_1 == 1 | M8.3_2 == 1 ~ 1,
                                    TRUE ~ 0),
         jubilacion = case_when(M8.3_3 == 1 ~ 1,
                                TRUE ~ 0),
         plan = case_when(M8.3_4 == 1 | M8.3_5 == 1 | M8.3_6 == 1 | M8.3_7 == 1 | 
                            M8.3_8 == 1 | M8.3_9 == 1 ~ 1,
                          TRUE ~ 0),
         renta = case_when(M8.3_10 == 1 ~ 1,
                           TRUE ~ 0))

base_hogar$M9.1 <- car::recode(base_hogar$M9.1, "1=1; 5=2; 2:4 = 3; 6:99 = 3")
base_hogar$M9.1_f <- factor(base_hogar$M9.1, labels = c("Propietario", "Inquilino",
                                                        "Otro"))

base_hogar$CNO_PSH <- ifelse(base_hogar$CNO_PSH == '', base_hogar$CNO_encuestado, base_hogar$CNO_PSH)

base_hogar$calificacion <- str_sub(base_hogar$CNO_PSH, 5, 5)
base_hogar$calificacion <- car::recode(base_hogar$calificacion, "7:9 = 9; '' = NA")
base_hogar$calificacion_f <- factor(base_hogar$calificacion, labels = c("Profesional",
                                                                        "Técnico",
                                                                        "Operativo",
                                                                        "No calificado",
                                                                        "Ns/Nc"))
base_hogar$jerarquia <- str_sub(base_hogar$CNO_PSH, 3, 3)
base_hogar$jerarquia <- car::recode(base_hogar$jerarquia, "'' = NA")
base_hogar$jerarquia_f <- factor(base_hogar$jerarquia, labels = c("Dirección",
                                                                  "Independiente",
                                                                  "Jefe",
                                                                  "Asalariado",
                                                                  "Ns/Nc"))

base_hogar$M8.5_f <- factor(base_hogar$M8.5, labels = c("No tuvo ingresos", "$0 a $25.000",
                                                          "$25.001 a $40.000", "$40.001 a $60.000",
                                                          "$60.001 a $90.000", "$90.001 a $120.000",
                                                          "$120.001 y 150.000", "$150.001 y 180.000",
                                                          "$180.001 y 210.000", "$210.001 y 250.000",
                                                          "$250.001 y 300.000", "$300.001 a 400.000",
                                                          "$400.001 a 600.000", "Más de $ 600.000"))

base_hogar <- base_hogar %>% 
  mutate(rango_ingresos_fam = case_when(M8.4 == 0 ~ "No tuvo ingresos",
                                         M8.4 > 0 & M8.4 <= 25000 ~ "$0 a $25.000",
                                         M8.4 > 25000 & M8.4 <= 40000 ~ "$25.001 a $40.000",
                                         M8.4 > 40000 & M8.4 <= 60000 ~ "$40.001 a $60.000",
                                         M8.4 > 60000 & M8.4 <= 90000 ~ "$60.001 a $90.000",
                                         M8.4 > 90000 & M8.4 <= 120000 ~ "$90.001 a $120.000",
                                         M8.4 > 120000 & M8.4 <= 150000 ~ "$120.001 y 150.000",
                                         M8.4 > 150000 & M8.4 <= 180000 ~ "$150.001 y 180.000",
                                         M8.4 > 180000 & M8.4 <= 210000 ~ "$180.001 y 210.000",
                                         M8.4 > 210000 & M8.4 <= 250000 ~ "$210.001 y 250.000",
                                         M8.4 > 250000 & M8.4 <= 300000 ~ "$250.001 y 300.000",
                                         M8.4 > 300000 & M8.4 <= 400000 ~ "$300.001 a 400.000",
                                         M8.4 > 400000 & M8.4 <= 600000 ~ "$400.001 a 600.000",
                                         M8.4 > 600000 ~ "Más de $ 600.000"))

base_hogar$rango_ingresos_fam <- ifelse(is.na(base_hogar$rango_ingresos_fam), 
                                         as.character(base_hogar$M8.5_f),
                                         base_hogar$rango_ingresos_fam)

base_hogar$M8.4[base_hogar$M8.5 == 1] <- 0


## Imputación -----------
### 1) Random Hot Deck para los ingresos con rango -----
summarytools::descr(base_hogar$M8.4)

base_hogar$itf_imp_rango <- base_hogar$M8.4

set.seed(971986)
base_imputacion1 <- hotdeck(data = base_hogar,
                            variable = "itf_imp_rango",
                            domain_var = "rango_ingresos_fam",
                            imp_suffix = "check")


### 2) Árbol de regresión + hot deck para ingresos con pérdida total ------
base_imputacion2 <- subset(base_imputacion1, !is.na(itf_imp_rango)) 

base_imputacion2$aportantes <- base_imputacion2$M8.2
base_imputacion2$cobertura_salud <- base_imputacion2$M2.7_f

arbol <- rpart(
  formula = itf_imp_rango ~ REGION + M1.2_f + MIEMBROS + cobertura_salud + nivel_ed_f + M2.12_f + aportantes + fuente_laboral +
    jubilacion + plan + renta + M9.1_f + M9.3,
  data    = base_imputacion2, 
  method  = "anova" 
)

rpart.plot(arbol, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE, digits=-3) 
plotcp(arbol)

arbol.pruned <- prune(arbol, cp = arbol$cptable[which.min(arbol$cptable[, "xerror"]), "CP"])

rpart.plot(arbol.pruned, digits = -3, type = 5, extra = 100, box.palette = "RdYlGn",
           cex = .7)


prediction <- predict(arbol.pruned, base_imputacion2)
output <- data.frame(base_imputacion2$itf_imp_rango, prediction)

rpart.rules(arbol.pruned, style = "tall", nn = TRUE)

####2.1) Creación de los grupos a partir de las reglas---------------
base_imputacion1 <- base_imputacion1 %>% 
  mutate(grupos_arbol = case_when(M2.7 >= 3 & M8.2 < 3 ~ 1,
                                  M2.7 < 3 & M8.2 < 2 ~ 2,
                                  M2.7 >= 3 & M8.2 >= 3 ~ 3,
                                  M2.7 < 3 & M8.2 >= 2 & 
                                    REGION %in% c("CENTRO", "GBA", "CUYO", "NEA", "NOA", "PAMPEANA") &
                                    nivel_ed <= 2 ~ 4,
                                  M2.7 < 3 & M8.2 >= 2 & REGION %in% c("CENTRO", "GBA", "CUYO", "NEA", "NOA", "PAMPEANA") &
                                    nivel_ed == 3 ~ 5,
                                  M2.7 < 3 & M8.2 >= 2 & REGION == "PATAGONICA" ~ 6
                                  ))

#### 2.2) Hot deck aleatorio en los grupos del árbol -------
base_imputacion1$itf_imp <- base_imputacion1$itf_imp_rango

set.seed(971986)
base_imputacion1 <- hotdeck(data = base_imputacion1,
                            variable = "itf_imp",
                            domain_var = "grupos_arbol"
)

#### 2.3) Pego en base hogar y comparo datos--------------
base_imputacion1 <- base_imputacion1 %>% 
  select(CUEST, itf_imp, itf_imp_rango)

base_hogar$itf_imp_rango <- NULL

base_hogar <- base_hogar %>% 
  left_join(base_imputacion1, by = "CUEST")

base_hogar %>%
  select(M8.4, itf_imp, itf_imp_rango) %>% 
  gather(ingreso, valor) %>%
  mutate(ingreso = case_when(ingreso == "M8.4" ~ "Sin imputar",
                             ingreso == "itf_imp_rango" ~ "Imputado por rango",
                             ingreso == "itf_imp" ~ "Imputado total"),
         ingreso = factor(ingreso, levels = c("Sin imputar", "Imputado por rango", "Imputado total"))) %>% 
  ggplot(aes(valor, y = ingreso, fill = ingreso)) +
  geom_density(alpha = 0.3) +
  geom_density_ridges() +
  labs(x = "ITF",
       y = "Densidad",
       title = "Ingreso total familiar imputado",
       caption = "Fuente: elaboración propia en base a encuesta PIRC-ESA") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 400000, 50000), limits = c(0, 400000))

ggsave(filename = "graficos/imputados_itf.jpg", dpi = 300, type = "cairo", width = 8, height = 5)


base_hogar %>%
  select(M8.4, itf_imp, itf_imp_rango) %>% 
  gather(ingreso, valor) %>%
  mutate(ingreso = case_when(ingreso == "M8.4" ~ "Sin imputar",
                             ingreso == "itf_imp_rango" ~ "Imputado por rango",
                             ingreso == "itf_imp" ~ "Imputado total"),
         ingreso = factor(ingreso, levels = c("Sin imputar", "Imputado por rango", "Imputado total"))) %>% 
  ggplot(aes(valor, fill = factor(ingreso), group = rev(ingreso))) +
  geom_density(alpha = 0.3) +
  labs(x = "ITF",
       y = "Densidad",
       title = "Ingreso total familiar imputado",
       caption = "Fuente: elaboración propia en base a encuesta PIRC-ESA") +
  scale_x_continuous(breaks = seq(0, 400000, 50000), limits = c(0, 400000))

ggsave(filename = "graficos/imputados_itf2.jpg", dpi = 300, type = "cairo", width = 8, height = 5)


### 3) Armo base con CUEST y itf_imp---------------

write.csv2(base_hogar, "outputs/ITF_imputado.csv")

base_hogar <- base_hogar %>% 
  left_join(base_hogar_inglab_imp, by = "CUEST")

save(base_hogar, file = "bases/base_hogar_imputados.RData")
