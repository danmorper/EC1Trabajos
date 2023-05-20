# Daniel José Moreno Pérez
library(randtoolbox) 
library(tseries)

# Paso 1: Generar una muestra de la distribución Beta(a, b)----
generar_muestra_beta <- function(a, b, n) {
  alpha <- a + b #a,b debe ser mayor que 0, no impongo la condición (sería un if) para no hacer el código más difícil de leer
  if (min(a, b) <= 1) {
    beta <- max(1/a, 1/b)
  } else {
    beta <- sqrt((alpha - 2) / (2 * a * b - alpha))
  }
  gamma <- a + (1 / beta)
  
  muestra <- numeric(n)
  i <- 1
  
  while (i <= n) {
    U1 <- runif(1)
    U2 <- runif(1)
    V <- beta * log(U1 / (1 - U1))
    W <- a * exp(V)
    if ((alpha * log(alpha / (b + W))) + (gamma * V) - log(4) < log((U1^2) * U2)) {
      muestra[i] <- W / (b + W)
      i <- i + 1
    }
  }
  
  return(muestra)
}

# Paso 2: Generar una muestra de tamaño 1000 de la distribución Beta(3, 2)----
set.seed(89889)  # Inicializar la semilla con las últimas 5 cifras del DNI
muestra <- generar_muestra_beta(3, 2, 1000)

# Calcular la media y varianza muestral
media_muestral <- mean(muestra)
varianza_muestral <- var(muestra)

# Calcular la media y varianza poblacional
media_poblacional <- 3 / (3 + 2)
varianza_poblacional <- (3 * 2) / ((3 + 2)^2 * (3 + 2 + 1))

# Comparar los resultados
cat("Media muestral:", media_muestral, "\n")
cat("Media poblacional:", media_poblacional, "\n")
cat("Varianza muestral:", varianza_muestral, "\n")
cat("Varianza poblacional:", varianza_poblacional, "\n")

# Paso 3: Graficar histograma y densidad de la distribución Beta(3, 2)----
hist(muestra, freq = FALSE, main = "Histograma de la muestra")
curve(dbeta(x, 3, 2), add = TRUE, col = "blue", lwd = 2, n = 1000, xlab = "x", ylab = "Densidad")

# Paso 4: Aplicar un test de rachas a la muestra----
mediana_muestral <- median(muestra)
muestra_binaria <- ifelse(muestra < mediana_muestral, 0, 1)
test_rachas <- rle(muestra_binaria)$lengths

cat("Test de rachas:", test_rachas, "\n")

# Paso 5: Aplicar la función contrastes_aleato
contrastes_aleato<-function(x,titulo)
{
  print(titulo)
  ks.test(x, "punif")
  gap.test(x)      #Por defecto, l=0, u=0.5
  order.test(x,d=5)  #d puede ser 2,3,4,5, pero n debe ser múltiplo de d
  freq.test(x, 1:4)#Por defecto, secuencia 1:15
  serial.test(x,d=5)  #n debe ser múltiplo de d (t=2)
  poker.test(x)
  print(runs.test(factor(x>0.5)))
  acf(x,main=titulo)
}

# Aplicar la función contrastes_aleato a la muestra generada
contrastes_aleato(muestra, "Contrastes aleatorios (Beta(3,2))")

# Repetir los pasos 2, 3, 4 y 5 con a = 0.5 y b = 2
muestra_repetida <- generar_muestra_beta(0.5, 2, 1000)
media_muestral_repetida <- mean(muestra_repetida)
varianza_muestral_repetida <- var(muestra_repetida)

cat("Media muestral (a = 0.5, b = 2):", media_muestral_repetida, "\n")
cat("Varianza muestral (a = 0.5, b = 2):", varianza_muestral_repetida, "\n")

hist(muestra_repetida, freq = FALSE, main = "Histograma de la muestra (a = 0.5, b = 2)")
curve(dbeta(x, 0.5, 2), add = TRUE, col = "blue", lwd = 2, n = 1000, xlab = "x", ylab = "Densidad")

mediana_muestral_repetida <- median(muestra_repetida)
muestra_repetida_binaria <- ifelse(muestra_repetida < mediana_muestral_repetida, 0, 1)
test_rachas <- rle(muestra_repetida_binaria)$lengths

contrastes_aleato(muestra_repetida, "Contrastes aleatorios (Beta(0.5,2))")
