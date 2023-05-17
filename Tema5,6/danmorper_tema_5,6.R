# Cinco primeras cifras del DNI
m <- 32089
set.seed(m)

# Cargar el archivo de datos
load(file = "Estrellas.RData")

# Extraer 1000 casos de forma aleatoria
datos <- datosel[sample(nrow(datosel), 1000), ]

# IC bootstrap ----
library(boot)

# Definir la función para calcular el cociente de los coeficientes de regresión
coef_ratio <- function(data, indices) {
  fit <- lm(plate ~ MJD + z, data = data[indices, ])
  coef(fit)["z"] / coef(fit)["MJD"]
}

# Realizar el bootstrap de pares
boot_pairs <- boot(datos, coef_ratio, R = 1000)

# Calcular el intervalo de confianza BCa
boot_pairs_bca <- boot.ci(boot_pairs, type = "bca", index = c(2, 1))
boot_pairs_bca

# Test de permutaciones ----
# Filtrar los datos para las clases "QSO" y "STAR"
datos_qso <- datos[datos$class == "QSO", ]
datos_star <- datos[datos$class == "STAR", ]

# Calcular las correlaciones lineales muestrales para cada clase
cor_qso <- cor(datos_qso$r, datos_qso$z)
cor_star <- cor(datos_star$r, datos_star$z)

# https://en.wikipedia.org/wiki/Fisher_transformation

# Transformaciones Z de Fisher de las correlaciones lineales muestrales

fisher_transform <- function(x) {
  0.5 * log((1 + x) / (1 - x))
}
z_qso <- fisher_transform(cor_qso)
z_star <- fisher_transform(cor_star)

# Obtener los números de casos para las clases "QSO" y "STAR"
n_qso <- nrow(datos_qso)
n_star <- nrow(datos_star)

# Definir una función para calcular el estadístico
statistic <- function(data, indices) {
  # Obtener las correlaciones lineales muestrales para cada clase permutada
  perm_qso <- cor(data[indices, ]$r[data[indices, ]$class == "QSO"], data[indices, ]$z[data[indices, ]$class == "QSO"])
  perm_star <- cor(data[indices, ]$r[data[indices, ]$class == "STAR"], data[indices, ]$z[data[indices, ]$class == "STAR"])
  
  # Calcular las transformaciones Z de Fisher de las correlaciones lineales muestrales permutadas
  z_perm_qso <- 0.5 * log((1 + perm_qso) / (1 - perm_qso))
  z_perm_star <- 0.5 * log((1 + perm_star) / (1 - perm_star))
  
  # Calcular el estadístico
  (z_perm_qso - z_perm_star) / sqrt(1/(n_qso-3) + 1/(n_star-3))
}

# Realizar las permutaciones
n_perm <- 4999
cor_trans_perm <- numeric(n_perm)

## QSO ----
for (i in 1:n_perm) {
  # Permutar la muestra star
  datos_star_perm <- datos_star[sample(1:n_star), ]
  
  # Calcular el coeficiente de correlación permutado
  cor_perm <- cor(datos_star_perm$r, datos_star_perm$z)
  
  # Aplicar el la transformación de Fisher al coeficiente de correlación permutado y lo guardo
  cor_trans_perm[i] <- fisher_transform(cor_perm)
}

cor_trans_perm <- na.omit(cor_trans_perm)
# Obtener el p-valor
p_value <- sum(abs(cor_trans_perm) >= abs(z_qso)) / n_perm
p_value

## STAR ----

cor_trans_perm <- numeric(n_perm)

for (i in 1:n_perm) {
  # Permutar la muestra star
  datos_qso_perm <- datos_qso[sample(1:n_star), ]
  
  # Calcular el coeficiente de correlación permutado
  cor_perm <- cor(datos_qso_perm$r, datos_qso_perm$z)
  
  # Aplicar el la transformación de Fisher al coeficiente de correlación permutado y lo guardo
  cor_trans_perm[i] <- fisher_transform(cor_perm)
}

cor_trans_perm <- na.omit(cor_trans_perm)
# Obtener el p-valor
p_value <- sum(abs(cor_trans_perm) >= abs(z_star)) / n_perm
p_value

# Algoritmo EM, mixtura ----

library(mclust)
i <- 13
# Ajustar diferentes modelos con diferentes números de componentes
model <- Mclust(datos[,i])

# Obtener el número óptimo de componentes según el criterio BIC
num_components <- model$G
num_components


# Algoritmo EM ----
# Inicializar los parámetros del modelo
s <- prelim.cat(datos[,1:2],datos[,3]) #Paso intermedio
piest<- em.cat(s,showit=TRUE) #em.cat necesita prelim.cat