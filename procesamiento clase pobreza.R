pacman::p_load(tidyverse)
options(scipen=999)

# Estadísticos desciptivos----------------

base_pirc %>%
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(cobhe_f) %>% 
  tally(wt=POND1R_FIN) %>%
  summarise(clase = cobhe_f, freq = n / sum(n)) %>% 
  ggplot(aes(x = clase, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Estructura de clases socio-ocupacionales",
       subtitle = "Hogares. Argentina urbana. 2021",
       caption = "Fuente: encuesta PIRC-ESA") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), limits = rev(levels(base_pirc$cobhe_f))) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 1, 0.05), limits = c(0,.3)) +
  coord_flip()

ggsave(filename = "graficos/estructura_clases.jpg", dpi = 300, type = "cairo", width = 8, height = 5)


base_pirc %>%
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(REGION, cobhe_f) %>% 
  tally(wt=POND1R_FIN) %>%
  group_by(REGION) %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot(aes(x = cobhe_f, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Estructura de clases socio-ocupacionales",
       subtitle = "Hogares. Argentina urbana. 2021",
       caption = "Fuente: encuesta PIRC-ESA") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(size = 11, face = "bold"),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30), limits = rev(levels(base_pirc$cobhe_f))) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 1, 0.1), limits = c(0,.5)) +
  coord_flip() + 
  facet_wrap(~REGION, nrow = 2)

ggsave(filename = "graficos/estructura_clases_region.jpg", dpi = 300, type = "cairo", width = 8, height = 5)


base_pirc %>%
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(cobhe_f) %>% 
  summarise(ITF = weighted.mean(itf_imp, POND1R_FIN)) %>% 
  ggplot(aes(x=cobhe_f, y = ITF)) +
  geom_bar(stat = "identity") +
  labs(title = "Ingreso total familiar medio según clase",
       subtitle = "Hogares. Argentina urbana. 2021",
       caption = "Fuente: encuesta ESAyPP / PISAC COVID-19") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), limits = rev(levels(base_pirc$cobhe_f))) +
  scale_y_continuous(breaks=seq(0, 160000, 20000), limits = c(0,160000)) +
  coord_flip()

ggsave(filename = "graficos/clases_ingresos.jpg", dpi = 300, type = "cairo", width = 8, height = 5)


base_pirc %>%
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(cobhe_f) %>% 
  summarise(pobreza = weighted.mean(pobreza, POND1R_FIN)) %>% 
  ggplot(aes(x=cobhe_f, y = pobreza)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(pobreza, accuracy = 0.1)), 
            position = position_dodge(.9), hjust = -.1, size = 3, fontface = "bold") +
  labs(title = "Porcentaje de hogares pobres según clase social",
       subtitle = "Argentina urbana. 2021",
       caption = "Fuente: encuesta ESAyPP / PISAC COVID-19") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), limits = rev(levels(base_pirc$cobhe_f))) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 1, 0.1), limits = c(0,.5)) +
  coord_flip()

ggsave(filename = "graficos/clases_pobreza.jpg", dpi = 300, type = "cairo", width = 8, height = 5)




genero_sum <- base_pirc %>% 
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(sexo_f) %>% 
  summarise(pobreza = weighted.mean(pobreza, POND1R_FIN)) %>% 
  spread(sexo_f, value = pobreza) %>% 
  add_column(cobhe_f = "Total", .before = "Varón")



base_pirc %>%
  filter(!is.na(cobhe_f) & M2.5 == 1) %>% 
  group_by(sexo_f, cobhe_f) %>% 
  summarise(pobreza = weighted.mean(pobreza, POND1R_FIN)) %>% 
  ggplot(aes(x=cobhe_f, y = pobreza)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(pobreza, accuracy = 0.1)), 
            position = position_dodge(.9), hjust = -.2, size = 3, fontface = "bold") +
  labs(title = "Porcentaje de hogares pobres según clase social y género del principal sostén",
       subtitle = "Argentina urbana. 2021",
       caption = "Fuente: encuesta ESAyPP / PISAC COVID-19") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), limits = rev(levels(base_pirc$cobhe_f))) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 1, 0.1), limits = c(0,.7)) +
  coord_flip() +
  facet_wrap(~sexo_f, labeller = labeller(sexo_f = c("Varón" = "Hogar PSH varón (pobreza = 28,9%)",
                                                     "Mujer" = "Hogar PSH Mujer (pobreza = 29,6%)")))

ggsave(filename = "graficos/clases_pobreza_genero.jpg", dpi = 300, type = "cairo", width = 8, height = 5)
