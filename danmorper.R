library(KernSmooth)
library(magrittr)
# Definimos la función de densidad real----
real_dens <- function(x) {
  0.5 * dnorm(x, mean = -1.5) + 0.5 * dnorm(x, mean = 1.5)
}
# Creamos la rejilla de valores representativos de la v.a. Z----
grid <- seq(grid_min, grid_max, length = grid_size)

# Definimos el número de muestras y los tamaños de muestra
M <- 1000
ns <- c(50, 100)
n_rejilla <- 512

# Definimos los métodos I y II para elegir el ancho de ventana----
# Ancho de banda knn y kernel gaussiano
method_I <- function(X,k) {
  h <- sort(abs(grid-X))[k]
  rejilla <- density(X, bw = h, kernel = "gaussian", from = -4, to = 4, n = n_rejilla)
  return(rejilla)
  
}

# Ancho de banda Sheather-Jones y kernel gaussiano
method_II <- function(X) {
  rejilla <- density(X,bw="SJ", kernel = "gaussian", from = -4, to = 4, n = n_rejilla)
  return(SJ)
}


# Realizamos las simulaciones ----
results <- data.frame(dato=c(), metodo = c(), tamaño = c(), parametro = c())
cont = 0
for (n in ns) {
  for (i in 1:M) {
    # Generamos la muestra X
    X <- 0.5*rnorm(n, mean = -1.5) + 0.5*rnorm(n, mean = 1.5)
    
    # Calculamos la densidad estimada con el método I con diferentes k
    for (k in c(15,30,50)) {
      h_I <- method_I(X, k)
      # Guardamos los resultados
      esta_vuelta <- data.frame(dato = h_I$y, metodo = rep(1,n_rejilla), tamaño = rep(n, n_rejilla), parametro = rep(k, n_rejilla))
      results <- rbind(results, esta_vuelta)
    }
    
    
    # Calculamos la densidad estimada con el método II
      h_II <- method_II(X,h)
      # Guardamos los resultados
      esta_vuelta <- data.frame(dato = h_II$y, metodo = rep(2,n_rejilla), tamaño = rep(n, n_rejilla), parametro = rep(h, n_rejilla))
      results <- rbind(results, esta_vuelta)
    
    cont=cont + 1
    print(cont)
  }
}
results[, -1] <- apply(results[, -1], 2, as.factor)
MSE <- function(dens_estimada, dens_real, grid) {
  mse <- sum((dens_estimada - dens_real)^2 * (grid[2]-grid[1]))
  return(mse)
}

MSE_aggregate <- function(dens_estimada) {
  dens_real <- real_dens(grid)
  mse <- return(MSE(dens_estimada, dens_real, grid))
}

mse <- aggregate(results$dato, by = list(results$metodo, results$tamaño, results$parametro), FUN = MSE_aggregate)
names(mse) <- c("metodo", "tamaño", "parametro", "ECM")

# Compute the configuration with minimum average MSE
min_mse_row <- mse[which.min(mse$ECM), ];min_mse_row

solouno <- subset(results, metodo ==1 & tamaño ==50 & parametro == 50)
plot(seq(-4,4, length = n_rejilla), solouno$dato, type = "l")
curve(real_dens, from = -4, to = 4, add = TRUE)

