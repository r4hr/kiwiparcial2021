# Paquetes ----

library(tidyverse)
library(googlesheets4)
library(gargle)
library(extrafont)
library(scales)
library(ggalt)
library(data.table)
library(kableExtra)

# Datasets ----


kiwi21 <- read_sheet("1nhpqDWuJoWhiVj0rIaV51-SdfnSxTpbV3Vcd3iYmyTw")

# Tipo de cambio
tc <- read_sheet("194DbwO2TNmYkWU5Ru1m6UxuOCfXxuRlFcrWrP_JzGv8") %>% 
  select(pais, tipo_cambio)

# Seteos generales -----
options(scipen = 999)

loadfonts(quiet = TRUE)

# Estilo de los gráficos
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#fbfcfc"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam 2021"

colores <-  c("#8624F5", "#1FC3AA")


# EDA -----

names(kiwi21)
glimpse(kiwi21)

kiwi21 %>% 
  filter(Trabajo == "Relación de Dependencia") %>% 
  select(sueldo_bruto = `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`) %>% 
  mutate(sueldo_bruto = as.numeric(unlist(sueldo_bruto)))

# Análisis -------

## Participación -----
# Relación dependencia vs. freelo

trabajo_tipo <- kiwi21 %>% 
  count(Trabajo) %>% 
  mutate(porcentaje = n/sum(n)) %>% 
  arrange(-n)

# Acomoda el texto en renglones
trabajo_tipo$Trabajo <- str_wrap(trabajo_tipo$Trabajo, width = 13)

trabajo_tipo

# Calculamos los límites superiores de cada rectángulo
trabajo_tipo$ymax <- cumsum(trabajo_tipo$porcentaje)

# Calculamos el límite inferior de cada porción
trabajo_tipo$ymin <- c(0, head(trabajo_tipo$ymax, n=-1))

# Calculamos la posición de la etiqueta
trabajo_tipo$posicion_etiqueta <- (trabajo_tipo$ymax + trabajo_tipo$ymin) / 2

# Creamos las etiquetas de cada porción
trabajo_tipo$etiqueta <- paste0(trabajo_tipo$Trabajo, # paste0 pega elementos
                             "\n Cant: ", 
                             trabajo_tipo$n)

# Ver como quedó el data frame
trabajo_tipo


ggplot(trabajo_tipo, aes(ymax=ymax, 
                         ymin=ymin, 
                         xmax=4, 
                         xmin=3, 
                         fill=Trabajo)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() +             # Elimina fondos y referencias
  scale_fill_manual(values = c("#4E5667","#28877A")) +   # Define una escala de colores
  theme(legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "Roboto") 
        ) + # Modifica posición leyendas y del título
  labs(title = "Respuestas según Tipo de Trabajo",
       fill = "Tipo de Trabajo", 
       caption = fuente) +
  geom_label(x = 3.5,
             aes(y = posicion_etiqueta,
                 label = etiqueta),
             size = 3, 
             color = "white",
             family = "Ubuntu")


## Respuestas por país ------
paises <- kiwi21 %>% 
  select(pais = `País en el que trabajas`) %>% 
  mutate(cuenta = 1) %>% 
  group_by(pais) %>% 
  summarise(conteo = sum(cuenta)) %>% 
#  filter(conteo > 4) %>% 
  arrange(-conteo)


paises

paises %>% 
  rename(País = pais,
         Cantidad = conteo) %>% 
  kbl() %>% 
  kable_paper("hover", full_width = F) %>% 
  scroll_box(height = "450px")


htmltools::div(style='height:600px; overflow-y: scroll', gt(paises) %>% 
      tab_header(title = "Cantidad de respuestas por país") 
)

## Sueldo promedio por país ----
sueldos_dolar <- kiwi21 %>% 
  filter(Trabajo !="Freelance") %>% 
  select(genero = Género, 
         puesto = `¿En qué puesto trabajás?`,
         pais = `País en el que trabajas` ,
         sueldo_bruto = `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`,
         tipo_contratacion = `Tipo de contratación`)

