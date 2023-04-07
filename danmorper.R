library(KernSmooth)
library(mixtools)
# Definimos la función de densidad real
real_dens <- function(x) {
  0.5 * dnorm(x, mean = -1.5) + 0.5 * dnorm(x, mean = 1.5)
}

# Creamos la rejilla de valores representativos de la v.a. Z
grid_size <- 512
grid_min <- -5
grid_max <- 5
grid <- seq(grid_min, grid_max, length = grid_size)

# Definimos el número de muestras y los tamaños de muestra
M <- 1000
ns <- c(50, 100)

# Definimos los métodos I y II para elegir el ancho de ventana
# n <- 1000
# X <- 0.5*rnorm(n, mean = -1.5) + 0.5*rnorm(n, mean = 1.5)
method_I <- function(X,k) {
  dk=numeric(grid_size)
  for (i in 1:grid_size)
  {
    distancias=abs(grid[i]-X)
    dk[i]=distancias[which(rank(distancias)==k)]
  }
  fx=(k-1)/(2*n*dk)
#  return(fx)
  # plot(grid,fx,type="l")
  # lines(grid,real_dens(grid),col="red",lwd=2)
  # grid()
  return(fx)
}
# method_I(X, k = 80)


method_II <- function(X) {
  h <- density(X, kernel = "gaussian", n = 512, bw = "nrd0")$bw
  return(h)
}


# Realizamos las simulaciones
results <- data.frame(metodo1 = c(), metodo2 = c(), tamaño = c())
cont = 0
for (n in ns) {
  for (i in 1:M) {
    # Generamos la muestra X
    X <- 0.5*rnorm(n, mean = -1.5) + 0.5*rnorm(n, mean = 1.5)
    
    # Calculamos la densidad real
    real_density <- real_dens(grid)
    
    # Calculamos la densidad estimada con el método I
    h_I <- method_I(X, k = 30)
    
    # Calculamos la densidad estimada con el método II
    h_II <- method_II(X)
    
    # Guardamos los resultados
    esta_vuelta <- data.frame(metodo1 = h_I, metodo2 = h_II, tamaño = rep(n,length(h_I)))
    results <- rbind(results, esta_vuelta)
    cont=cont + 1
    print(cont)
  }
}

MSE <- function(dens_estimada, dens_real, grid) {
  mse <- sum((dens_estimada - dens_real)^2 * (grid[2]-grid[1]))
  return(mse)
}

mse_I <- numeric(length = nrow(results))
mse_II <- numeric(length = nrow(results))
for (i in 1:nrow(results)) {
  dens_est_I <- results[i, "metodo1"]
  dens_est_II <- results[i, "metodo2"]
  dens_real <- real_dens(grid)
  mse_I[i] <- MSE(dens_est_I, dens_real, grid)
  mse_II[i] <- MSE(dens_est_II, dens_real, grid)
}

mse_by_size <- data.frame(metodo = rep(c("I", "II"), each = length(ns) * M),
                          tamaño = rep(rep(ns, each = M), 2),
                          mse = c(mse_I, mse_II))

mse_promedio <- aggregate(mse ~ metodo + tamaño, data = mse_by_size, FUN = mean)
