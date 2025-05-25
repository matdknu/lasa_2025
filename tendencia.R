library(tidyverse)

set.seed(42)

# Años y tópicos
anios <- 2005:2024
topicos <- c(
  "Accidentes", "Alto riesgo", "Asistencia en catástrofes", "Conflicto Mapuche",
  "Delincuencia", "Derechos Humanos", "Drogas", "Enfrentamientos balas",
  "Judiciales", "Protestas", "Proyectos seguridad", "Violencia en los Estadios"
)

# Crear base vacía
tendencia_falsa <- expand.grid(anio = anios, name_topic = topicos) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(n = case_when(
    
    # Derechos Humanos: peak sutil entre 2019 y 2022, baja posterior, valores con ruido
    name_topic == "Derechos Humanos" ~ round(rnorm(1,
                                                   mean = case_when(
                                                     anio %in% 2019:2022 ~ 500,
                                                     anio >= 2023 ~ 200,
                                                     TRUE ~ 150
                                                   ), sd = 40)),
    
    # Conflicto Mapuche: peaks en años específicos, luego declive
    name_topic == "Conflicto Mapuche" ~ round(rnorm(1,
                                                    mean = case_when(
                                                      anio %in% c(2006, 2016:2019) ~ 380,
                                                      anio >= 2020 ~ 90,
                                                      TRUE ~ 200
                                                    ), sd = 30)),
    
    # Delincuencia: crecimiento suave con ruido
    name_topic == "Delincuencia" ~ round(rnorm(1, mean = 60 + 6 * (anio - 2005), sd = 15)),
    
    # Drogas: crecimiento sostenido con variabilidad
    name_topic == "Drogas" ~ round(rnorm(1, mean = 50 + 5 * (anio - 2005), sd = 12)),
    
    # Protestas: picos en años clave, baja después
    name_topic == "Protestas" ~ round(rnorm(1,
                                            mean = case_when(
                                              anio %in% c(2006, 2011, 2019:2022) ~ 450,
                                              anio >= 2023 ~ 160,
                                              TRUE ~ 200
                                            ), sd = 35)),
    
    # Proyectos seguridad: crecimiento fuerte en años recientes
    name_topic == "Proyectos seguridad" ~ round(rnorm(1,
                                                      mean = case_when(
                                                        anio >= 2021 ~ 380,
                                                        anio >= 2018 ~ 240,
                                                        TRUE ~ 100
                                                      ), sd = 25)),
    
    # Otros tópicos: comportamiento más plano y aleatorio
    name_topic == "Accidentes" ~ round(rnorm(1, mean = 130, sd = 20)),
    name_topic == "Alto riesgo" ~ round(rnorm(1, mean = 110, sd = 15)),
    name_topic == "Asistencia en catástrofes" ~ round(rnorm(1, mean = 120, sd = 18)),
    name_topic == "Enfrentamientos balas" ~ round(rnorm(1, mean = 100, sd = 15)),
    name_topic == "Judiciales" ~ round(rnorm(1, mean = 140, sd = 18)),
    name_topic == "Violencia en los Estadios" ~ round(rnorm(1, mean = 85, sd = 12)),
    
    TRUE ~ 50
  )) %>%
  ungroup() %>%
  mutate(n = ifelse(n < 0, 0, n))  # evitar negativos

# Ajustar total a 40.000
factor <- 40000 / sum(tendencia_falsa$n)
tendencia_falsa <- tendencia_falsa %>%
  mutate(n = round(n * factor))

# Corregir desfase de redondeo
delta <- 40000 - sum(tendencia_falsa$n)
tendencia_falsa$n[1] <- tendencia_falsa$n[1] + delta

# Resultado
tendencia_falsa



g4 <-ggplot(tendencia_falsa, aes(x = anio, y = n)) +
  geom_line() +
  geom_point(size = 0.7) +
  facet_wrap(~ name_topic) +
  labs(
    title = "Evolución de tópicos en Chile (2005–2024)",
    subtitle = "Distribución simulada de documentos por año y tópico",
    x = "Año", 
    y = "Documentos asociados"
  ) +
  theme_bw(base_size = 13) +
 # theme_classic(base_size = 13)
 # theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

g4
ggsave(plot = g4, 
       filename = "images/topico_temporal.png")
