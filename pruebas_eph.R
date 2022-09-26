library(eph)
library(tidyverse)

eph <- get_microdata(year=2021, trimester = 3, type = "hogar")

eph0 <- eph %>% 
  filter(ITF > 0) 

eph4 <- eph %>% 
  filter(ITF < 400000)

ggplot() +
  geom_density(data = base_hogar, aes(M8.4, weight = POND1_hogar_calibrado_expA, fill = "ITF PIRC no imputado", color = "ITF PIRC no imputado"), alpha = 0.5) +
  geom_density(data = base_hogar, aes(itf_imp, weight = POND1_hogar_calibrado_expA, fill = "ITF PIRC imputado", color = "ITF PIRC imputado"), alpha = 0.5) +
  geom_density(data = eph0, aes(ITF, weight = PONDIH, fill = "ITF EPH", color = "ITF EPH"), alpha = 0.5) +
  geom_vline(data = base_hogar, aes(xintercept = weighted.mean(M8.4, POND1_hogar_calibrado_expA, na.rm = T)), size = .5) +
  geom_vline(data = base_hogar, aes(xintercept = weighted.mean(itf_imp, POND1_hogar_calibrado_expA, na.rm = T)), size = .5) +
  geom_vline(data = eph, aes(xintercept = weighted.mean(as.numeric(ITF), PONDIH, na.rm = T)), linetype = "dashed", size = .5) +
  geom_vline(data = eph4, aes(xintercept = weighted.mean(as.numeric(ITF), PONDIH, na.rm = T)), linetype = "dashed", size = .5) +
  scale_x_continuous(breaks = seq(0, 400000, 50000), limits = c(0, 400000)) +
  labs(title = "Comparación ITF PIRC vs EPH 3-2021",
       caption = "Fuente: EPH 3er trimestre 2021 - Encuesta PIRC-ESA",
      x = "Ingresos",
       y = "Densidad",
       fill = "Tipo de ingresos",
       color = "Tipo de ingresos")+
  theme(legend.position="bottom")


ggsave(filename = "graficos/comparacion_eph.jpg", dpi = 300, type = "cairo", width = 8, height = 5)

