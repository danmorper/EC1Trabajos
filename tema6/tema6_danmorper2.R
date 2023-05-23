# Daniel José Moreno Pérez
library(randtoolbox) 
library(tseries)

# Apartado1 ----

## Paso 1: Generar una muestra de la distribución Beta(a, b)----
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
    if ((alpha * log(alpha / (b + W))) + (gamma * V) - log(4) >= log((U1^2) * U2)) {
      muestra[i] <- W/(b+W)
    }
    print(i)
    i<-i+1
  }
  
  return(muestra)
}

# Apartado2 ----
## Paso 2: Generar una muestra de tamaño 1000 de la distribución Beta(3, 2)----
set.seed(89889)  # Inicializar la semilla con las últimas 5 cifras del DNI
muestra <- generar_muestra_beta(3, 2, 1000)

## Calcular la media y varianza muestral----
media_muestral <- mean(muestra)
varianza_muestral <- var(muestra)

## Calcular la media y varianza poblacional ----
media_poblacional <- 3 / (3 + 2)
varianza_poblacional <- (3 * 2) / ((3 + 2)^2 * (3 + 2 + 1))

## Comparar los resultados ----
cat("Media muestral:", media_muestral, "\n")
cat("Media poblacional:", media_poblacional, "\n")
cat("Varianza muestral:", varianza_muestral, "\n")
cat("Varianza poblacional:", varianza_poblacional, "\n")

#Apartado3 ----
## Paso 3: Graficar histograma y densidad de la distribución Beta(3, 2)----
hist(muestra, freq = FALSE, main = "Histograma de la muestra", ylim = c(0,2))
curve(dbeta(x, 3, 2), add = TRUE, col = "blue", lwd = 2, n = 1000, xlab = "x", ylab = "Densidad")


# Apartado4 ----

#Aplicar un test de rachas a la muestra
mediana_muestral <- median(muestra)
runs.test(factor(muestra>media_muestral))
# pvalor mayor que 0.05 -> La muestra generada es aleatoria

# Apartado5 ----

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
# las autocorrelaciones están cerca de 0 (ninguna significativa, la primera no se tiene en cuenta)

# Apartado6 ----
# Repetir los pasos 2, 3, 4 y 5 con a = 0.5 y b = 2
muestra_repetida <- generar_muestra_beta(0.5, 2, 1000)

media_muestral_repetida <- mean(muestra_repetida)
varianza_muestral_repetida <- var(muestra_repetida)
media_poblacional_repetida <- 0.5 / (0.5 + 2)
varianza_poblacional_repetida <- (0.5 * 2) / ((0.5 + 2)^2 * (0.5 + 2 + 1))

cat("Media muestral:", media_muestral_repetida, "\n")
cat("Media poblacional:", media_poblacional_repetida, "\n")
cat("Varianza muestral:", varianza_muestral_repetida, "\n")
cat("Varianza poblacional:", varianza_poblacional_repetida, "\n")

hist(muestra_repetida, freq = FALSE, main = "Histograma de la muestra (a = 0.5, b = 2)")
curve(dbeta(x, 0.5, 2), add = TRUE, col = "blue", lwd = 2, n = 1000, xlab = "x", ylab = "Densidad")

mediana_muestral_repetida <- median(muestra_repetida)
runs.test(factor(muestra_repetida>mediana_muestral_repetida))
# pvalor mayor que 0.05 -> La muestra generada es aleatoria

contrastes_aleato(muestra_repetida, "Contrastes aleatorios (Beta(0.5,2))")
# las autocorrelaciones están cerca de 0 (ninguna significativa, la primera no se tiene en cuenta)