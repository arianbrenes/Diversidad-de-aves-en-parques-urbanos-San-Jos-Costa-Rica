#grafico riqueza aves 

library(ggplot2) 
library(extrafont)  # Solo si utilizás Arial

# Datos  
datos_riqueza <- data.frame(
  Parques = c("Morazán",
              "Garantías Sociales",
              "Central",
              "Braulio Carrillo",
              "Cañas",
              "La Dolorosa"),
  Riqueza_aves = c(20, 13, 7, 13, 14, 11)
)

# Gráfico
ggplot(datos_riqueza,
       aes(x = reorder(Parques, -Riqueza_aves),
           y = Riqueza_aves,
           fill = Riqueza_aves)) +
  
  geom_col(show.legend = FALSE) +
  
  scale_fill_gradient(
    low = "#a6cee3",
    high = "#1f78b4"
  ) +
  
  labs(
    title = "Riqueza de aves por parque",
    x = NULL,
    y = "Número de especies"
  ) +
  
  theme_minimal(base_size = 12, base_family = "Arial") +
  
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Título
    plot.title = element_text(
      size = 14,
      hjust = 0.5
    ),
    
    # Título del eje Y
    axis.title.y = element_text(size = 12),
    
    # Texto de los ejes
    axis.text.x = element_text(
      size = 11,
      color = "black",
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 11,
      color = "black"
    )
  )


# riqueza plantas

library(ggplot2)
library(extrafont)  # Solo si necesitás usar Arial

datos_riqueza_plantas <- data.frame(
  Parques = c("Braulio", "Cañas", "Central", "Dolorosa", "Garantías", "Morazán"),
  Riqueza = c(15, 10, 8, 3, 3, 9)
)

ggplot(datos_riqueza_plantas,
       aes(x = reorder(Parques, -Riqueza),
           y = Riqueza,
           fill = Riqueza)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#c6dbef", high = "#2171b5") +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Tamaño del título
    plot.title = element_text(size = 16, hjust = 0.5),
    
    # Tamaño de los títulos de los ejes
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Tamaño de los nombres de los parques
    axis.text.x = element_text(
      size = 13,
      color = "black",
      angle = 45,
      hjust = 1
    ),
    
    # Tamaño de los números del eje Y
    axis.text.y = element_text(
      size = 13,
      color = "black"
    )
  ) +
  labs(
    title = "Riqueza de plantas por parque",
    x = NULL,
    y = "Número de especies"
  )




#especies más abundantes aves

#====================================================
# LIBRERÍAS
#====================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(grid)

#====================================================
# BASE DE DATOS
#====================================================
datos_total <- data.frame(
  Parques = c("Morazán","Garantías Sociales","Central","Dolorosa","Cañas","Braulio Carrillo"),
  Columba_livia = c(35,11,9,11,27,18),
  Thraupis_episcopus = c(22,0,0,5,0,0),
  Zonothrichia_capensis = c(23,29,22,50,51,19),
  Psittacara_finschi = c(16,1,0,7,14,5),
  Pitangus_sulfuratus = c(15,2,0,12,9,3),
  Tardus_grayi = c(4,1,9,4,15,14),
  Zenaida_asiatica = c(9,18,36,22,25,75),
  Quiscalus_mexicanus = c(5,1,1,2,3,2),
  Melanerpes_hoffmani = c(16,9,13,3,1,14),
  Tyrannus_melanocholicus = c(2,3,0,0,0,0),
  Notiochelidon_cyanoleuca = c(3,1,0,0,0,1),
  Buteo_plagiatus = c(1,1,1,3,0,0),
  Amazilia_tzacatl = c(10,20,0,0,3,0),
  Stelgidopteryx_serripennis = c(5,0,0,4,2,11),
  Myiozetetes_similis = c(0,0,0,0,0,2),
  Brotogeris_jugularis = c(0,0,0,0,0,3),
  Mimus_gilvus = c(0,0,0,0,1,0),
  Amazillia_saucerottei = c(0,0,0,0,1,0),
  Amazilia_rutila = c(0,0,0,0,1,0),
  Pitagioena_flavirostris = c(2,0,0,0,5,0),
  Thraupis_palmarum = c(1,0,0,0,0,0),
  Spinus_psaltria = c(3,0,0,0,0,0),
  Colombina_inca = c(0,0,0,0,3,0),
  Todirostrum_cinereum = c(0,0,0,0,0,1),
  Megarynchus_pitangua = c(4,0,0,0,0,0),
  Colibri_no_identificado = c(1,0,0,0,0,0)
)

#====================================================
# FORMATO LARGO
#====================================================
datos_long <- datos_total %>%
  pivot_longer(
    -Parques,
    names_to = "Especie",
    values_to = "Abundancia"
  ) %>%
  filter(Abundancia > 0)

#====================================================
# ESPECIES MÁS ABUNDANTES
#====================================================
top10 <- datos_long %>%
  group_by(Especie) %>%
  summarise(Total = sum(Abundancia), .groups="drop") %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>%
  pull(Especie)

#====================================================
# AGRUPAR RESTO
#====================================================
datos_long <- datos_long %>%
  mutate(
    Especie = ifelse(
      Especie %in% top10,
      Especie,
      "Otras especies"
    )
  ) %>%
  group_by(Parques, Especie) %>%
  summarise(
    Abundancia = sum(Abundancia),
    .groups="drop"
  )

#====================================================
# ETIQUETAS EN ITÁLICA
#====================================================
datos_long <- datos_long %>%
  mutate(
    Etiqueta = ifelse(
      Especie == "Otras especies",
      "Otras especies",
      paste0("<i>", gsub("_", " ", Especie), "</i>")
    )
  )

orden_etiquetas <- datos_long %>%
  group_by(Etiqueta) %>%
  summarise(Total=sum(Abundancia), .groups="drop") %>%
  arrange(Total) %>%
  pull(Etiqueta)

#====================================================
# COLORES
#====================================================
colores <- c(
  "Morazán" = "#001F54",
  "Garantías Sociales" = "#00509D",
  "Central" = "#1E88E5",
  "Dolorosa" = "#00A6D6",
  "Cañas" = "#66C2FF",
  "Braulio Carrillo" = "#BFDFFF"
)

#====================================================
# GRÁFICO
#====================================================
grafico <- ggplot(
  datos_long,
  aes(
    x = factor(Etiqueta, levels = orden_etiquetas),
    y = Abundancia,
    fill = Parques
  )
) +
  
  geom_col(
    width = 0.80,
    colour = "white",
    linewidth = 0.30
  ) +
  
  coord_flip() +
  
  scale_fill_manual(values = colores) +
  
  labs(
    x = "Especie",
    y = "Abundancia",
    fill = "Parque"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    axis.text.y = ggtext::element_markdown(size = 12),
    
    axis.text.x = element_text(size = 12),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    # Leyenda a la derecha
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(
      size = 11
    ),
    
    legend.key.width = unit(1.2, "cm"),
    
    legend.spacing.x = unit(0.4, "cm"),
    
    plot.margin = margin(10,10,10,10)
    
  )

grafico


#====================================================
# GUARDAR PNG
#====================================================
ggsave(
  "C:/Users/Arian/Downloads/Figura_abundancia_aves.png",
  grafico,
  width = 12,
  height = 8,
  dpi = 600,
  bg = "white"
)


#====================================================
# GUARDAR PDF
#====================================================
ggsave(
  "C:/Users/Arian/Downloads/Figura_abundancia_aves.pdf",
  grafico,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)




#plantas más abundantes


#==========================================================
# LIBRERÍAS
#==========================================================
library(ggplot2)
library(tidyr)
library(ggtext)

#==========================================================
# DATOS
#==========================================================
datos <- data.frame(
  Parque = c(
    "Braulio Carrillo",
    "Cañas",
    "Central",
    "Garantías Sociales",
    "La Dolorosa",
    "Morazán"
  ),
  
  "Tabebuia rosea" = c(12.2,4.2,0,22.7,30.8,0),
  "Jacaranda mimosifolia" = c(0,12.5,0,0,30.8,24.4),
  "Cojoba arborea" = c(0,0,0,50.0,0,0),
  "Clusia rosea" = c(0,0,28.6,0,0,0),
  "Sp 7" = c(0,0,0,0,0,12.2),
  "Otras especies" = c(87.8,83.3,71.4,27.3,38.4,63.4),
  
  check.names = FALSE
)

#==========================================================
# FORMATO LARGO
#==========================================================
datos_long <- pivot_longer(
  datos,
  cols = -Parque,
  names_to = "Especie",
  values_to = "Porcentaje"
)

#==========================================================
# ORDEN DE LOS PARQUES
#==========================================================
datos_long$Parque <- factor(
  datos_long$Parque,
  levels = c(
    "Morazán",
    "La Dolorosa",
    "Garantías Sociales",
    "Central",
    "Cañas",
    "Braulio Carrillo"
  )
)

#==========================================================
# ORDEN DE LA LEYENDA
#==========================================================
datos_long$Especie <- factor(
  datos_long$Especie,
  levels = c(
    "Tabebuia rosea",
    "Jacaranda mimosifolia",
    "Cojoba arborea",
    "Clusia rosea",
    "Sp 7",
    "Otras especies"
  )
)

#==========================================================
# COLORES
#==========================================================
colores <- c(
  "Tabebuia rosea"="#08306B",
  "Jacaranda mimosifolia"="#2171B5",
  "Cojoba arborea"="#4292C6",
  "Clusia rosea"="#6BAED6",
  "Sp 7"="#08519C",
  "Otras especies"="#EAF2F8"
)

#==========================================================
# ETIQUETAS DE LA LEYENDA
#==========================================================
etiquetas <- c(
  "<i>Tabebuia rosea</i>",
  "<i>Jacaranda mimosifolia</i>",
  "<i>Cojoba arborea</i>",
  "<i>Clusia rosea</i>",
  "Sp 7",
  "Otras especies"
)

#==========================================================
# GRÁFICO
#==========================================================
grafico <- ggplot(datos_long,
                  aes(x = Parque,
                      y = Porcentaje,
                      fill = Especie)) +
  
  geom_col(
    colour="white",
    linewidth=0.4,
    width=0.70
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = colores,
    labels = etiquetas
  ) +
  
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,20),
    labels = function(x) paste0(x,"%"),
    expand = c(0,0)
  ) +
  
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    fill = "Especie"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    axis.text.x = element_text(
      size = 12,
      colour = "black"
    ),
    
    axis.text.y = element_text(
      size = 12,
      colour = "black"
    ),
    
    axis.title.y = element_blank(),
    
    axis.title.x = element_text(
      face = "bold",
      size = 13
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    
    legend.text = ggtext::element_markdown(
      size = 11
    ),
    
    legend.key = element_blank(),
    
    panel.grid = element_blank(),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


#Gráfico de correspondencia canónica

# Librerías necesarias
library(vegan)
library(ggplot2)
library(ggrepel)

# ==========================
# 1) PCA de variables ambientales
# ==========================
pca_amb <- prcomp(ambiente, scale. = TRUE)

# Ve qué proporción de varianza explica cada PC
summary(pca_amb)

# Extraer las 2 primeras PCs
pca_scores <- as.data.frame(pca_amb$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")

# Asegurar que filas coincidan con especies
rownames(pca_scores) <- rownames(ambiente)
rownames(especies) <- rownames(ambiente)

# ==========================
# 2) CCA usando PCs como variables predictoras
# ==========================
resCCA <- cca(especies ~ PC1 + PC2, data = pca_scores)

# Ver resumen
summary(resCCA)

# ==========================
# 3) Extraer scores para gráfica
# ==========================
# Especies (flechas rojas)
sp_scores <- scores(resCCA, display = "species", scaling = 2)

# Sitios (parques)
sites_scores <- scores(resCCA, display = "sites", scaling = 2)

# ⚡️ Ahora para mostrar variables AMBIENTALES originales:
# Multiplica loadings * coef canónicos para proyectarlas

# Cargar loadings originales (rotación)
loadings <- pca_amb$rotation[, 1:2]

# Coeficientes de cada PC en la CCA
coef_can <- resCCA$CCA$biplot

# Multiplica: loadings * coeficientes de CCA
# Esto devuelve la proyección de cada variable original en los ejes canónicos
env_scores <- as.matrix(loadings) %*% as.matrix(coef_can)

# Convertir a dataframe
env_scores <- as.data.frame(env_scores)
colnames(env_scores) <- c("CCA1", "CCA2")
env_scores$Variable <- rownames(env_scores)

# ==========================
# 4) Convertir species y sitios
# ==========================
sp_df <- as.data.frame(sp_scores)
sp_df$Species <- rownames(sp_df)

sites_df <- as.data.frame(sites_scores)
sites_df$Site <- rownames(sites_df)

# ==========================
# 5) Graficar en ggplot2
# ==========================
ggplot() +
  # Variables ambientales originales
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "forestgreen") +
  geom_text_repel(data = env_scores, aes(x = CCA1, y = CCA2, label = Variable),
                  color = "forestgreen") +
  
  # Especies
  geom_segment(data = sp_df, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "red") +
  geom_text_repel(data = sp_df, aes(x = CCA1, y = CCA2, label = Species),
                  color = "red") +
  
  # Parques (sitios)
  geom_point(data = sites_df, aes(x = CCA1, y = CCA2), color = "blue", size = 2) +
  geom_text_repel(data = sites_df, aes(x = CCA1, y = CCA2, label = Site),
                  color = "blue") +
  
  labs(title = "CCA usando PCs + proyección variables ambientales",
       x = "CCA1", y = "CCA2") +
  theme_minimal()

ggplot() +
  # ... [tus capas] ...
  labs(title = "CCA usando PCs + variables originales",
       x = "CCA1", y = "CCA2") +
  xlim(-1, 1) +  # Ajusta estos valores
  ylim(-1, 1) +  # Ajusta estos valores
  theme_minimal()
geom_text_repel(
  data = env_scores,
  aes(x = CCA1, y = CCA2, label = Variable),
  color = "forestgreen",
  max.overlaps = 100  # Aumenta para forzar etiquetas
)
env_scores$CCA1 <- env_scores$CCA1 * 1.5
env_scores$CCA2 <- env_scores$CCA2 * 1.5

sp_df$CCA1 <- sp_df$CCA1 * 1.5
sp_df$CCA2 <- sp_df$CCA2 * 1.5
theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))


library(ggplot2)
library(ggrepel)

# Si quieres, escala flechas:
env_scores$CCA1 <- env_scores$CCA1 * 1.5
env_scores$CCA2 <- env_scores$CCA2 * 1.5
sp_df$CCA1 <- sp_df$CCA1 * 1.5
sp_df$CCA2 <- sp_df$CCA2 * 1.5
















library(ggplot2)
library(ggrepel)

# ⚡️ Si quieres, escala flechas:
env_scores$CCA1 <- env_scores$CCA1 * 1.5
env_scores$CCA2 <- env_scores$CCA2 * 1.5
sp_df$CCA1 <- sp_df$CCA1 * 1.5
sp_df$CCA2 <- sp_df$CCA2 * 1.5

# Gráfico final SIN CUADRÍCULA
ggplot() +
  # Variables ambientales originales
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "forestgreen") +
  geom_text_repel(data = env_scores, aes(x = CCA1, y = CCA2, label = Variable),
                  color = "forestgreen", max.overlaps = 100) +
  
  # Especies
  geom_segment(data = sp_df, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "red") +
  geom_text_repel(data = sp_df, aes(x = CCA1, y = CCA2, label = Species),
                  color = "red", max.overlaps = 100) +
  
  # Sitios (Parques)
  geom_point(data = sites_df, aes(x = CCA1, y = CCA2), color = "blue", size = 2) +
  geom_text_repel(data = sites_df, aes(x = CCA1, y = CCA2, label = Site),
                  color = "blue") +
  
  labs(title = "CCA con variables originales proyectadas",
       x = "CCA1", y = "CCA2") +
  
  xlim(-1, 1) + ylim(-1, 1) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),   # 🔥 QUITA CUADRÍCULA
    panel.border = element_blank(), # Opcional: quita borde del panel
    axis.line = element_line(),     # Opcional: muestra ejes X-Y limpios
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )





library(ggplot2)
library(ggrepel)

# ⚡️ Aumenta escala para ver flechas pequeñas
factor_escala <- 2
env_scores$CCA1 <- env_scores$CCA1 * factor_escala
env_scores$CCA2 <- env_scores$CCA2 * factor_escala
sp_df$CCA1 <- sp_df$CCA1 * factor_escala
sp_df$CCA2 <- sp_df$CCA2 * factor_escala

# Gráfico sin cuadrícula y sin recortes
ggplot() +
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "forestgreen") +
  geom_text_repel(data = env_scores, aes(x = CCA1, y = CCA2, label = Variable),
                  color = "forestgreen", max.overlaps = Inf) +
  
  geom_segment(data = sp_df, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2,"cm")), color = "red") +
  geom_text_repel(data = sp_df, aes(x = CCA1, y = CCA2, label = Species),
                  color = "red", max.overlaps = Inf) +
  
  geom_point(data = sites_df, aes(x = CCA1, y = CCA2), color = "blue", size = 2) +
  geom_text_repel(data = sites_df, aes(x = CCA1, y = CCA2, label = Site),
                  color = "blue", max.overlaps = Inf) +
  
  labs(title = "CCA usando PCs + Variables Originales",
       x = "CCA1", y = "CCA2") +
  
  coord_equal() +  # 🔥 Mantiene proporción sin recortes
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )


#Gráfico CCA

library(ggplot2)

# Extraemos las puntuaciones canónicas de las muestras para X (ambientales) y Y (aves)
xscores <- resCCA$scores$xscores[, 1:2]  # Dim 1 y 2
yscores <- resCCA$scores$yscores[, 1:2]

# Nombres de los parques (6 filas)
parques <- c("Parque Morazán", "Parque Garantías Sociales", "Parque Central", 
             "Parque Braulio Carrillo", "Parque Cañas", "Parque la Dolorosa")

# Data frame para puntos de parques
df_parques <- data.frame(x = xscores[,1], y = xscores[,2], grupo = "Parques", label = parques)

# Vectores para variables ambientales (loadings x coeficientes canónicos)
env_loadings <- pca_X$rotation[,1:3] %*% resCCA$xcoef  # producto para obtener peso final
env_vectors <- data.frame(env_loadings[,1:2])
env_vectors$variable <- rownames(env_vectors)
colnames(env_vectors) <- c("x", "y", "variable")

# Vectores para variables aves (loadings x coeficientes canónicos)
bird_loadings <- pca_Y$rotation[,1:2] %*% resCCA$ycoef
bird_vectors <- data.frame(bird_loadings)
bird_vectors$variable <- rownames(bird_vectors)
colnames(bird_vectors) <- c("x", "y", "variable")

# Graficar con ggplot
ggplot() +
  geom_point(data = df_parques, aes(x = x, y = y, color = grupo), size = 3) +
  geom_text(data = df_parques, aes(x = x, y = y, label = label), vjust = -1) +
  geom_segment(data = env_vectors, aes(x = 0, y = 0, xend = x*3, yend = y*3),
               arrow = arrow(length = unit(0.3, "cm")), color = "forestgreen") +
  geom_text(data = env_vectors, aes(x = x*3, y = y*3, label = variable), color = "forestgreen", hjust = 0) +
  geom_segment(data = bird_vectors, aes(x = 0, y = 0, xend = x*3, yend = y*3),
               arrow = arrow(length = unit(0.3, "cm")), color = "darkorange") +
  geom_text(data = bird_vectors, aes(x = x*3, y = y*3, label = variable), color = "darkorange", hjust = 0) +
  xlab("Dimensión Canónica 1") + ylab("Dimensión Canónica 2") +
  ggtitle("Biplot del análisis de correspondencia canónica (CCA)") +
  theme_minimal() +
  theme(legend.position = "none")









library(ggplot2)
library(ggrepel)

# FACTOR para escalar flechas (ajústalo)
factor_flechas <- 3

ggplot() +
  # Puntos de parques
  geom_point(data = df_parques, aes(x = x, y = y), color = "blue", size = 3) +
  geom_text_repel(
    data = df_parques,
    aes(x = x, y = y, label = label),
    color = "blue",
    max.overlaps = Inf
  ) +
  
  # Flechas variables ambientales
  geom_segment(
    data = env_vectors,
    aes(x = 0, y = 0, xend = x * factor_flechas, yend = y * factor_flechas),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "forestgreen"
  ) +
  geom_text_repel(
    data = env_vectors,
    aes(x = x * factor_flechas, y = y * factor_flechas, label = variable),
    color = "forestgreen",
    max.overlaps = Inf
  ) +
  
  # Flechas especies
  geom_segment(
    data = bird_vectors,
    aes(x = 0, y = 0, xend = x * factor_flechas, yend = y * factor_flechas),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "darkorange"
  ) +
  geom_text_repel(
    data = bird_vectors,
    aes(x = x * factor_flechas, y = y * factor_flechas, label = variable),
    color = "darkorange",
    max.overlaps = Inf
  ) +
  
  # Títulos y ejes
  xlab("Dimensión Canónica 1") +
  ylab("Dimensión Canónica 2") +
  ggtitle("Biplot del análisis de correspondencia canónica (CCA)") +
  
  # SIN CUADRÍCULA + ejes claros
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),  # QUITA CUADRÍCULA
    axis.line = element_line(color = "black")
  ) +
  coord_equal()  # proporción igual para que no se distorsione

















