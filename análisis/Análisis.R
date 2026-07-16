#Análisis

# regresión lineal

datos_df <- as.data.frame(ANÁLISIS_ESTADISTICOS)
datos_num <- datos_df[, 2:18]
str(datos_num)

library(usdm)
datos_df <- as.data.frame(ANÁLISIS_ESTADISTICOS)
variables_estructurales <- datos_df[, 2:13]
str(variables_estructurales)

# Asegúrate de tener tu tabla lista
datos_df <- as.data.frame(ANÁLISIS_ESTADISTICOS)


# Ve los nombres originales para revisar
names(datos_df)

# CORRIGE nombres: usa 'names' NO 'ames'
names(datos_df) <- gsub(" ", "_", names(datos_df))   # Reemplaza espacios
names(datos_df) <- gsub("\\(%\\)", "", names(datos_df))  # Quita paréntesis y %
names(datos_df) <- gsub("%", "", names(datos_df))    # Quita % sueltos
names(datos_df) <- gsub("-", "_", names(datos_df))   # Reemplaza guiones
names(datos_df) <- tolower(names(datos_df))          # Todo minúsculas

# Verifica que quedó bien

names(datos_df)
variables_indep <- datos_df[, 2:8]
vifcor(variables_indep, th = 0.7, method = "pearson")
variables_indep <- datos_df[, 9:13]
vifcor(variables_indep, th = 0.7, method = "pearson")

# =====================================================
# ✅ RIQUEZA DE AVES: UNO A UNO
# =====================================================
m1_riqueza <- lm(riqueza_de_aves ~ cobertura_arbustiva_, data = datos_df)
summary(m1_riqueza)

m2_riqueza <- lm(riqueza_de_aves ~ superficies_impermeables__, data = datos_df)
summary(m2_riqueza)

m3_riqueza <- lm(riqueza_de_aves ~ superficies_no_impermeables__, data = datos_df)
summary(m3_riqueza)

m4_riqueza <- lm(riqueza_de_aves ~ numero_de_estratos, data = datos_df)
summary(m4_riqueza)


# =====================================================
# ✅ ABUNDANCIA TOTAL DE AVES: UNO A UNO
# =====================================================
m1_abundancia <- lm(abundancia_total_de_aves ~ cobertura_arbustiva_, data = datos_df)
summary(m1_abundancia)

m2_abundancia <- lm(abundancia_total_de_aves ~ superficies_impermeables__, data = datos_df)
summary(m2_abundancia)

m3_abundancia <- lm(abundancia_total_de_aves ~ superficies_no_impermeables__, data = datos_df)

summary(m3_abundancia)

m4_abundancia <- lm(abundancia_total_de_aves ~ numero_de_estratos, data = datos_df)
summary(m4_abundancia)


# =====================================================
# ✅ DOMINANCIA DE AVES: UNO A UNO
# =====================================================
m1_dominancia <- lm(dominancia_de_aves ~ cobertura_arbustiva_, data = datos_df)
summary(m1_dominancia)

m2_dominancia <- lm(dominancia_de_aves ~ superficies_impermeables__, data = datos_df)
summary(m2_dominancia)

m3_dominancia <- lm(dominancia_de_aves ~ superficies_no_impermeables__, data = datos_df)
summary(m3_dominancia)

m4_dominancia <- lm(dominancia_de_aves ~ numero_de_estratos, data = datos_df)
summary(m4_dominancia)


# =====================================================
# ✅ EQUITATIVIDAD DE AVES: UNO A UNO
# =====================================================
m1_equitatividad <- lm(equitatividad_de_aves ~ cobertura_arbustiva_, data = datos_df)
summary(m1_equitatividad)

m2_equitatividad <- lm(equitatividad_de_aves ~ superficies_impermeables__, data = datos_df)
summary(m2_equitatividad)

m3_equitatividad <- lm(equitatividad_de_aves ~ superficies_no_impermeables__, data = datos_df)
summary(m3_equitatividad)

m4_equitatividad <- lm(equitatividad_de_aves ~ numero_de_estratos, data = datos_df)
summary(m4_equitatividad)


# =====================================================
# ✅ HETEROGENEIDAD DE AVES: UNO A UNO
# =====================================================
m1_heterogeneidad <- lm(heterogeneidad_de_aves ~ cobertura_arbustiva_, data = datos_df)
summary(m1_heterogeneidad)

m2_heterogeneidad <- lm(heterogeneidad_de_aves ~ superficies_impermeables__, data = datos_df)
summary(m2_heterogeneidad)

m3_heterogeneidad <- lm(heterogeneidad_de_aves ~ superficies_no_impermeables__, data = datos_df)
summary(m3_heterogeneidad)

m4_heterogeneidad <- lm(heterogeneidad_de_aves ~ numero_de_estratos, data = datos_df)
summary(m4_heterogeneidad)









# ========== RIQUEZA DE AVES ==========
lm_riqueza_1 <- lm(riqueza_de_aves ~ cobertura_arbustiva_ + superficies_impermeables__, data = datos_df)
summary(lm_riqueza_1)

lm_riqueza_2 <- lm(riqueza_de_aves ~ cobertura_arbustiva_ + superficies_no_impermeables__, data = datos_df)
summary(lm_riqueza_2)

lm_riqueza_3 <- lm(riqueza_de_aves ~ cobertura_arbustiva_ + numero_de_estratos, data = datos_df)
summary(lm_riqueza_3)

lm_riqueza_4 <- lm(riqueza_de_aves ~ superficies_impermeables__ + superficies_no_impermeables__, data = datos_df)
summary(lm_riqueza_4)

lm_riqueza_5 <- lm(riqueza_de_aves ~ superficies_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_riqueza_5)

lm_riqueza_6 <- lm(riqueza_de_aves ~ superficies_no_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_riqueza_6)


# ========== ABUNDANCIA TOTAL DE AVES ==========
lm_abundancia_1 <- lm(abundancia_total_de_aves ~ cobertura_arbustiva_ + superficies_impermeables__, data = datos_df)
summary(lm_abundancia_1)

lm_abundancia_2 <- lm(abundancia_total_de_aves ~ cobertura_arbustiva_ + superficies_no_impermeables__, data = datos_df)
summary(lm_abundancia_2)

lm_abundancia_3 <- lm(abundancia_total_de_aves ~ cobertura_arbustiva_ + numero_de_estratos, data = datos_df)
summary(lm_abundancia_3)

lm_abundancia_4 <- lm(abundancia_total_de_aves ~ superficies_impermeables__ + superficies_no_impermeables__, data = datos_df)
summary(lm_abundancia_4)

lm_abundancia_5 <- lm(abundancia_total_de_aves ~ superficies_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_abundancia_5)

lm_abundancia_6 <- lm(abundancia_total_de_aves ~ superficies_no_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_abundancia_6)



modelo_abundancia <- lm(abundancia_total_de_aves ~ superficies_no_impermeables__, data = datos_df)
summary(modelo_abundancia)
# ========== DOMINANCIA DE AVES ==========
lm_dominancia_1 <- lm(dominancia_de_aves ~ cobertura_arbustiva_ + superficies_impermeables__, data = datos_df)
summary(lm_dominancia_1)

lm_dominancia_2 <- lm(dominancia_de_aves ~ cobertura_arbustiva_ + superficies_no_impermeables__, data = datos_df)
summary(lm_dominancia_2)

lm_dominancia_3 <- lm(dominancia_de_aves ~ cobertura_arbustiva_ + numero_de_estratos, data = datos_df)
summary(lm_dominancia_3)


lm_dominancia_4 <- lm(dominancia_de_aves ~ superficies_impermeables__ + superficies_no_impermeables__, data = datos_df)
summary(lm_dominancia_4)

lm_dominancia_5 <- lm(dominancia_de_aves ~ superficies_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_dominancia_5)

lm_dominancia_6 <- lm(dominancia_de_aves ~ superficies_no_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_dominancia_6)


# ========== EQUITATIVIDAD DE AVES ==========
lm_equitatividad_1 <- lm(equitatividad_de_aves ~ cobertura_arbustiva_ + superficies_impermeables__, data = datos_df)
summary(lm_equitatividad_1)

lm_equitatividad_2 <- lm(equitatividad_de_aves ~ cobertura_arbustiva_ + superficies_no_impermeables__, data = datos_df)
summary(lm_equitatividad_2)

lm_equitatividad_3 <- lm(equitatividad_de_aves ~ cobertura_arbustiva_ + numero_de_estratos, data = datos_df)
summary(lm_equitatividad_3)

lm_equitatividad_4 <- lm(equitatividad_de_aves ~ superficies_impermeables__ + superficies_no_impermeables__, data = datos_df)
summary(lm_equitatividad_4)

lm_equitatividad_5 <- lm(equitatividad_de_aves ~ superficies_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_equitatividad_5)

lm_equitatividad_6 <- lm(equitatividad_de_aves ~ superficies_no_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_equitatividad_6)


# ========== HETEROGENEIDAD DE AVES ==========
lm_heterogeneidad_1 <- lm(heterogeneidad_de_aves ~ cobertura_arbustiva_ + superficies_impermeables__, data = datos_df)
summary(lm_heterogeneidad_1)

lm_heterogeneidad_2 <- lm(heterogeneidad_de_aves ~ cobertura_arbustiva_ + superficies_no_impermeables__, data = datos_df)
summary(lm_heterogeneidad_2)

lm_heterogeneidad_3 <- lm(heterogeneidad_de_aves ~ cobertura_arbustiva_ + numero_de_estratos, data = datos_df)
summary(lm_heterogeneidad_3)

lm_heterogeneidad_4 <- lm(heterogeneidad_de_aves ~ superficies_impermeables__ + superficies_no_impermeables__, data = datos_df)
summary(lm_heterogeneidad_4)

lm_heterogeneidad_5 <- lm(heterogeneidad_de_aves ~ superficies_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_heterogeneidad_5)


lm_heterogeneidad_6 <- lm(heterogeneidad_de_aves ~ superficies_no_impermeables__ + numero_de_estratos, data = datos_df)
summary(lm_heterogeneidad_6)


#bloque plantas
lm_riqueza_1 <- lm(riqueza_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_riqueza_1)

lm_abundancia_1 <- lm(abundancia_total_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_abundancia_1)

lm_dominancia_1 <- lm(dominancia_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_dominancia_1)

lm_equitatividad_1 <- lm(equitatividad_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_equitatividad_1)

lm_heterogeneidad_1 <- lm(heterogeneidad_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_heterogeneidad_1)



# Bloque 1: abundancia_total_de_plantas + dominancia_de_plantas
lm_riqueza_1 <- lm(riqueza_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_riqueza_1)

lm_abundancia_1 <- lm(abundancia_total_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_abundancia_1)

lm_dominancia_1 <- lm(dominancia_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_dominancia_1)

lm_equitatividad_1 <- lm(equitatividad_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_equitatividad_1)

lm_heterogeneidad_1 <- lm(heterogeneidad_de_aves ~ abundancia_total_de_plantas + dominancia_de_plantas, data = datos_df)
summary(lm_heterogeneidad_1)


#Correlación canónica utilizando componentes principales

# Crear tabla de variables ambientales
X <- data.frame(
  Cob_arborea = c(51.82, 15.81, 51.35, 30.36, 20.46, 14.04),
  Cob_arbustiva = c(24.26, 0.83, 51.27, 13.88, 4.78, 8.64),
  Sup_imper = c(68.7, 62.87, 58.63, 55.76, 56.7, 75.43),
  Sup_no_imper = c(38.8, 5.02, 0, 0, 17.57, 1.24),
  Edificaciones = c(3.7, 0.38, 3.68, 0, 0.49, 0),
  Tam_parque = c(6664.175, 6958.92, 2142.228, 6278.419, 7040.391, 5687.07),
  Riqueza_plantas = c(9, 3, 8, 15, 10, 3),
  Abund_pl = c(41, 22, 15, 42, 23, 13),
  Equit_pl = c(0.7585696, 0.9445114, 0.9178115, 0.828586, 0.8275972, 0.9947372),
  Dom_pl = c(0.743605, 0.6239669, 0.8265306, 0.8447353, 0.8020833, 0.6627219),
  Het_pl = c(1.666748, 1.037652, 1.908535, 2.243853, 1.905613, 1.092831),
  Num_estratos = c(6, 8, 9, 6, 6, 7)
)

# Crear tabla de variables de aves
Y <- data.frame(
  Equit_aves = c(0.7230145, 0.7514963, 0.793912, 0.7225646, 0.7705915, 0.7844345),
  Dom_aves = c(0.7997143, 0.8140358, 0.7448376, 0.7563776, 0.8235938, 0.7767863),
  Het_aves = c(2.165958, 1.92755, 1.544881, 1.853342, 2.033635, 1.880992),
  Riquez_aves = c(20, 13, 7, 13, 14, 11),
  Abund_aves = c(297, 98, 91, 168, 160, 123)
)


# Cargar la librería para CCA (vegan o CCA del paquete yacca)
install.packages("yacca")
library(yacca)

# Ejecutar análisis de correspondencia canónica
resCCA <- cc(X, Y)
#Tengo que hacer PCA porque hay multicolinealidad

# Instalar y cargar paquete CCA si no lo tienes
if (!require(CCA)) install.packages("CCA")
library(CCA)

# Tus datos (reemplaza decimales con punto)
X <- data.frame(
  Cobertura_arborea = c(51.82, 15.81, 51.35, 30.36, 20.46, 14.04),
  Cobertura_arbustiva = c(24.26, 0.83, 51.27, 13.88, 4.78, 8.64),
  Superficies_impermeables = c(68.7, 62.87, 58.63, 55.76, 56.7, 75.43),
  Superficies_no_impermeables = c(38.8, 5.02, 0, 0, 17.57, 1.24),
  Edificaciones = c(3.7, 0.38, 3.68, 0, 0.49, 0),
  Tamano_parque = c(6664.175, 6958.92, 2142.228, 6278.419, 7040.391, 5687.07),
  Riqueza_plantas = c(9, 3, 8, 15, 10, 3),
  Abundancia_total_plantas = c(41, 22, 15, 42, 23, 13),
  Equitatividad_plantas = c(0.7585696, 0.9445114, 0.9178115, 0.828586, 0.8275972, 0.9947372),
  Dominancia_plantas = c(0.743605, 0.6239669, 0.8265306, 0.8447353, 0.8020833, 0.6627219),
  Heterogeneidad_plantas = c(1.666748, 1.037652, 1.908535, 2.243853, 1.905613, 1.092831),
  Numero_estratos = c(6, 8, 9, 6, 6, 7)
)

Y <- data.frame(
  Equitatividad_aves = c(0.7230145, 0.7514963, 0.7939120, 0.7225646, 0.7705915, 0.7844345),
  Dominancia_aves = c(0.7997143, 0.8140358, 0.7448376, 0.7563776, 0.8235938, 0.7767863),
  Heterogeneidad_aves = c(2.165958, 1.927550, 1.544881, 1.853342, 2.033635, 1.880992),
  Riqueza_aves = c(20, 13, 7, 13, 14, 11),
  Abundancia_total_aves = c(297, 98, 91, 168, 160, 123)
)

# Escalar y hacer PCA (variables centradas y escaladas)
pca_X <- prcomp(X, scale. = TRUE)
pca_Y <- prcomp(Y, scale. = TRUE)

# Seleccionar primeras componentes (ajusta según % varianza o número de componentes)
X_pca <- pca_X$x[, 1:3]  # 3 componentes principales ambientales
Y_pca <- pca_Y$x[, 1:2]  # 2 componentes principales aves

# Revisar porcentaje de varianza explicada
summary(pca_X)
summary(pca_Y)

# Análisis de correspondencia canónica (CCA) con las componentes principales
resCCA <- cc(X_pca, Y_pca)

# Resultados
print(resCCA)

# Opcional: correlaciones canónicas
resCCA$cor

# Puedes ver los coeficientes canónicos así:
resCCA$xcoef  # para X
resCCA$ycoef  # para Y





# Mostrar correlaciones canónicas
print("Correlaciones canónicas:")
print(resCCA$cor)

# Opcional: graficar correlaciones canónicas
plot(resCCA$cor, type = "b", xlab = "Componente canónica", ylab = "Correlación")

# Explorar coeficientes canónicos para entender contribución de variables ambientales y aves
print("Coeficientes canónicos para variables ambientales (X):")
print(resCCA$xcoef)

print("Coeficientes canónicos para variables de aves (Y):")
print(resCCA$ycoef)

#saber cuáles variables originales son las que más pesan, debes revisar los loadings del PCA (pca_X$rotation y pca_Y$rotation) y relacionarlos con los coeficientes canónicos para interpretar qué variables ambientales y de aves están vinculadas.

# Loadings (contribución de variables originales a PCs)
print("Loadings PCA X:")
print(pca_X$rotation[, 1:3])  # Primeras 3 PCs

print("Loadings PCA Y:")
print(pca_Y$rotation[, 1:2])  # Primeras 2 PCs


# -------------------------

#Correspondencia canónica

# Instalar y cargar vegan
install.packages("vegan")
library(vegan)

# -------------------------
# Crear tabla ambiental (como la dejaste antes)
# -------------------------

ambiente <- data.frame(
  Parque = c("Parque Morazán", "Parque Garantías Sociales", "Parque Central", 
             "Parque Braulio Carrillo", "Parque Cañas", "Parque la Dolorosa"),
  Cobertura_arborea = c(51.82, 15.81, 51.35, 30.36, 20.46, 14.04),
  Cobertura_arbustiva = c(24.26, 0.83, 51.27, 13.88, 4.78, 8.64),
  Superficies_impermeables = c(68.7, 62.87, 58.63, 55.76, 56.7, 75.43),
  Superficies_no_impermeables = c(38.8, 5.02, 0, 0, 17.57, 1.24),
  Edificaciones = c(3.7, 0.38, 3.68, 0, 0.49, 0),
  Tamano_parque = c(6664.175, 6958.92, 2142.228, 6278.419, 7040.391, 5687.07),
  Numero_estratos = c(6, 8, 9, 6, 6, 7),
  Riqueza_plantas = c(9, 3, 8, 15, 10, 3),
  Abundancia_total_plantas = c(41, 22, 15, 42, 23, 13),
  Equitatividad_plantas = c(0.7585696, 0.9445114, 0.9178115, 0.828586, 0.8275972, 0.9947372),
  Dominancia_plantas = c(0.743605, 0.6239669, 0.8265306, 0.8447353, 0.8020833, 0.6627219),
  Heterogeneidad_plantas = c(1.666748, 1.037652, 1.908535, 2.243853, 1.905613, 1.092831)
)

rownames(ambiente) <- ambiente$Parque
ambiente$Parque <- NULL

# -------------------------
# Crear tabla de especies
# -------------------------

especies <- data.frame(
  `Columba livia` = c(120, 11, 9, 11, 27, 18),
  `Thraupis episcopus` = c(35, 0, 0, 5, 0, 0),
  `Zonothrichia capensis` = c(22, 29, 22, 50, 51, 19),
  `Psittacara finschi` = c(23, 1, 0, 7, 14, 5),
  `Pitangus sulfuratus` = c(16, 2, 0, 12, 9, 3),
  `Turdus grayi` = c(15, 1, 9, 4, 15, 14),
  `Zenaida asiatica` = c(4, 18, 36, 22, 25, 75),
  `Quiscalus mexicanus` = c(9, 1, 1, 2, 3, 2),
  `Melanerpes hoffmani` = c(5, 9, 13, 3, 1, 14),
  `Tyrannus melancholicus` = c(16, 3, 0, 0, 0, 0),
  `Notiochelidon cyanoleuca` = c(2, 1, 0, 0, 0, 0),
  `Buteo plagiatus` = c(3, 1, 0, 0, 0, 1),
  `Amazilia tzacatl` = c(1, 1, 1, 3, 0, 0),
  `Stelgidopteryx serripennis` = c(10, 20, 0, 0, 3, 0),
  `Myiozetetes similis` = c(5, 0, 0, 4, 2, 11),
  `Brotogeris jugularis` = c(0, 0, 0, 0, 0, 2),
  `Mimus giluus` = c(0, 0, 0, 0, 0, 3),
  `Amazilia saucerottei` = c(0, 0, 0, 0, 1, 0),
  `Amazilia rutita` = c(0, 0, 0, 0, 1, 0),
  `Pitagioena flavirostris` = c(2, 0, 0, 0, 5, 0),
  `Thraupis palmarum` = c(1, 0, 0, 0, 0, 0),
  `Spinus psaltria` = c(3, 0, 0, 0, 0, 0),
  `Colombina inca` = c(0, 0, 0, 0, 3, 0),
  `Todirostrum cinereum` = c(0, 0, 0, 0, 0, 1),
  `Megaryncus pitangua` = c(4, 0, 0, 0, 0, 0),
  `Colibrí_no_identificado` = c(1, 0, 0, 0, 0, 0)
)

rownames(especies) <- c("Parque Morazán", "Parque Garantías Sociales", "Parque Central",
                        "Parque la Dolorosa", "Parque Cañas", "Parque Braulio Carrillo")

# -------------------------
# Ejecutar CCA
# -------------------------

resCCA <- cca(especies ~ ., data = ambiente)

# Resumen
summary(resCCA)

# Gráfico biplot
plot(resCCA, scaling = 2, main = "Análisis de Correspondencia Canónica")

# Pruebas de significancia
anova(resCCA)  # Significancia global
anova(resCCA, by = "term")  # Por variable ambiental
anova(resCCA, by = "axis")  # Por eje canónico


#hay variables muy correlacionadas entonces haré PCA

# Escalar (muy importante) y hacer PCA
pca_amb <- prcomp(ambiente, scale. = TRUE)

# Ver resumen del PCA (proporción de varianza)
summary(pca_amb)

# Ver pesos de cada variable en cada componente
pca_amb$rotation #loading

# Ver scores (ejes principales para cada parque)
pca_scores <- pca_amb$x
print(pca_scores)

# Graficar
biplot(pca_amb, scale = 0)
# Tomar solo PC1 y PC2
ambiente_pca <- as.data.frame(pca_scores[, 1:2])

# Los nombres de fila siguen siendo los parques, así que quedan listos

# Asegúrate de tener la tabla de especies con los mismos rownames
# (Usa la tabla de especies que compartiste antes)

# Ejecutar CCA usando PC1 y PC2 como variables ambientales
library(vegan)
resCCA <- cca(especies ~ ., data = ambiente_pca)

# Resumen
summary(resCCA)

# Coordenadas de variables ambientales (biplot scores)
scores(resCCA, display = "bp")

# Coordenadas de las especies
scores(resCCA, display = "species")
scores(resCCA, display = "bp")


resCCA <- cca(especies ~ ., data = ambiente)


# Gráfico biplot
plot(resCCA, scaling = 2, main = "CCA usando PCs de variables ambientales")

# Significancia global y por eje
anova(resCCA)           # Significancia global
anova(resCCA, by="axis") # Por eje canónico


plot(resCCA, scaling = 2)
plot(resCCA, scaling = 2)


pca_amb$rotation

colnames(resCCA$CCA$biplot)
resCCA$CCA$biplot
pca_amb$rotation


#PERMANOVA y simper

# Instala y carga vegan si no lo tienes
# install.packages("vegan")
library(vegan)

# Crear el data frame con las abundancias por réplica
datos <- data.frame(
  Parques = rep(c("Parque Morazan", "Parque Garatias sociales", "Parque Central",
                  "Parque de la Dolorosa", "Parque Cañas", "Parque Braulio Carrillo"), each = 3),
  Replicas = rep(c("Semana 1", "Semana 2", "Semana 3"), 6),
  Columba_livia = c(40,40,40, 80,40,50, 90,90,90, 30,40,50, 40,15,25, 30,40,50),
  Thraupis_episcopus = c(20,8,7, 4,2,5, 4,1,4, 4,3,4, 8,10,9, 7,7,4),
  Zonothrichia_capensis = c(6,6,10, 0,0,0, 0,0,0, 3,1,1, 0,0,0, 0,0,0),
  Psittacara_finschi = c(9,0,14, 6,12,11, 2,12,8, 10,10,30, 16,22,13, 0,4,15),
  Pitangus_sulfuratus = c(6,5,5, 0,0,1, 0,0,0, 3,0,4, 4,5,5, 1,2,2),
  Tardus_grayi = c(5,5,5, 1,0,1, 0,0,0, 3,5,4, 3,5,1, 1,1,1),
  Zenaida_asiatica = c(3,0,1, 1,0,0, 6,0,3, 0,2,2, 7,4,4, 4,5,5),
  Quiscalus_mexicanus = c(1,2,6, 10,4,4, 16,10,10, 3,15,4, 8,7,10, 30,25,20),
  Melanerpes_hoffmani = c(1,1,3, 0,0,1, 0,0,1, 1,0,1, 1,0,2, 1,1,0),
  Tyrannus_melanocholicus = c(7,2,7, 2,5,2, 7,2,4, 1,1,1, 1,0,0, 7,7,0),
  Notiochelidon_cyanoleuca = c(2,0,0, 0,0,3, 0,0,0, 0,0,0, 0,0,0, 0,0,0),
  Buteo_plagiatus = c(1,2,0, 0,1,0, 0,0,0, 0,0,0, 0,0,0, 0,1,0),
  Amazilia_tzacatl = c(1,0,0, 1,0,0, 0,0,1, 1,1,1, 0,0,0, 0,0,0),
  Stelgidopteryx_serripennis = c(0,0,10, 15,5,0, 0,0,0, 0,0,0, 0,3,0, 0,0,0),
  Myiozetetes_similis = c(0,3,2, 0,0,0, 0,0,0, 3,0,1, 0,0,2, 6,3,2),
  Brotogeris_jugularis = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 2,0,0),
  Mimus_gilvus = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,1,1),
  Amazillia_saucerottei = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0),
  Amazilia_rutita = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0),
  Pitagioena_flavirostris = c(0,1,1, 0,0,0, 0,0,0, 0,0,0, 0,1,4, 0,0,0),
  Thraupis_palmarum = c(0,1,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,1,0),
  Spinus_psaltria = c(0,2,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0),
  Colombina_inca = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,3, 0,0,0),
  Todirostrum_cinereum = c(0,0,4, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,1),
  Megarynchus_pitangua = c(0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0),
  Colibri_no_identificado = c(0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0)
)

# Seleccionar solo columnas de abundancia (especies)
abundancias <- datos[, 3:ncol(datos)]

# Crear factor para los grupos (Parques)
grupo <- datos$Parques

# Correr PERMANOVA
set.seed(123)
resultado_permanova <- adonis2(abundancias ~ grupo, method = "bray", permutations = 999)
print(resultado_permanova)

# Correr SIMPER
resultado_simper <- simper(abundancias, grupo)
summary(resultado_simper)










# betadisper: dispersión dentro de grupos (varianza multivariada)
disp <- betadisper(vegdist(datos, method = "bray"), grupos)
anova(disp)  # ¿varianza dentro de grupos diferente?

plot(disp)  # para visualizar

# Opcionalmente prueba con TukeyHSD para pares
TukeyHSD(aov(disp$distances ~ grupos))




# Supongamos que matriz_especies es tu data original
# Extraer solo las columnas numéricas (sin Parque ni Replica)
datos_numeric <- matriz_especies[, sapply(matriz_especies, is.numeric)]
distancia <- vegdist(datos_numeric, method = "bray")
disp <- betadisper(distancia, grupos)
anova(disp)
plot(disp)
TukeyHSD(aov(disp$distances ~ grupos))










library(vegan)
library(ggplot2)

# Extraer solo columnas numéricas (abundancias)
datos_numeric <- matriz_especies[, sapply(matriz_especies, is.numeric)]

# Crear vector grupos (parques)
grupos <- factor(matriz_especies$Parque)

# NMDS con distancia Bray-Curtis
nmds <- metaMDS(datos_numeric, distance = "bray", k = 2, trymax = 100)

# Datos para ggplot
nmds_data <- as.data.frame(nmds$points)
nmds_data$Parque <- grupos

# Gráfico NMDS
ggplot(nmds_data, aes(x = MDS1, y = MDS2, color = Parque)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "NMDS: Comunidades de aves por Parque",
       x = "NMDS 1", y = "NMDS 2")

