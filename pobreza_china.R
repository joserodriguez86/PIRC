eph <- calculate_poverty(base = eph, basket = canastas, print_summary = TRUE)

eph <- eph %>% 
  mutate(pobreza_dic = case_when(situacion %in% c("pobre", "indigente") ~ 1,
                                 situacion %in% "no_pobre" ~ 0))

eph %>%
  group_by(ano_trim) %>% 
  summarise(porcentaje = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) %>% 
  ggplot(mapping = aes(x = as.character(ano_trim), y = porcentaje, group = 1)) +
  geom_vline(xintercept=c("20182", "20201"), linetype=5, color = "#ff7f0e") +
  geom_point(size = 2, color = "#d62728") +
  geom_line(size = .7, color = "#d62728") +
  theme_classic() +
  annotate(geom = "text", x = "20183", y = .17, label = "Crisis de deuda externa", hjust = "left",
           fontface="bold", size = 3.5) +
  annotate(geom = "curve", x = "20191", y = .15, xend = "20182", yend = .05, curvature = -.5,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = "20203", y = .15, label = "Inicio ASPO", hjust = "left",
           fontface="bold", size = 3.5) +
  annotate(geom = "curve", x = "20204", y = .13, xend = "20201", yend = .1, curvature = -.5,
           arrow = arrow(length = unit(2, "mm"))) +
  labs(y = "% Pobreza",
       title = "Evolución de la población bajo la línea de pobreza",
       subtitle = "Argentina urbana 2016-2021",
       caption = "Fuente: elaboración propia en base EPH-INDEC.") +
  theme(legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 9),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 0.5, size = 9),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_y_continuous(breaks = seq(0, .6, by = 0.05), limits = c(0, .5), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave(filename = "graficos/pobreza2.jpg", dpi = 300, type = "cairo", width = 8, height = 6)
