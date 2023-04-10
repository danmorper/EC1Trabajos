library(KernSmooth)
library(magrittr)
# Definimos la función de densidad real----
real_dens <- function(x) {
  0.5 * dnorm(x, mean = -1.5) + 0.5 * dnorm(x, mean = 1.5)
}

# Creamos la rejilla de valores representativos de la v.a. Z----
grid_size <- 512
grid_min <- -5
grid_max <- 5
grid <- seq(grid_min, grid_max, length = grid_size)

# Definimos el número de muestras y los tamaños de muestra
M <- 1000
ns <- c(50, 100)

# Definimos los métodos I y II para elegir el ancho de ventana----
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


method_II <- function(X, h) {
  h <- density(X,bw="SJ")$bw
  return(h)
}


# Realizamos las simulaciones ----
results <- data.frame(dato=c(), metodo = c(), tamaño = c(), parametro = c())
cont = 0
for (n in ns) {
  for (i in 1:M) {
    # Generamos la muestra X
    X <- 0.5*rnorm(n, mean = -1.5) + 0.5*rnorm(n, mean = 1.5)
    
    # Calculamos la densidad real
    real_density <- real_dens(grid)
    
    # Calculamos la densidad estimada con el método I
    for (k in c(15,30,50)) {
      h_I <- method_I(X, k)
      # Guardamos los resultados
      n_metodo <- length(h_I)
      esta_vuelta <- data.frame(dato = h_I, metodo = rep(1,n_metodo), tamaño = rep(n, n_metodo), parametro = rep(k, n_metodo))
      results <- rbind(results, esta_vuelta)
    }
    
    
    # Calculamos la densidad estimada con el método II
    for (h in c(15,30,60)) {
      h_II <- method_II(X,h)
      # Guardamos los resultados
      n_metodo <- length(h_II)
      esta_vuelta <- data.frame(dato = h_II, metodo = rep(2,n_metodo), tamaño = rep(n, n_metodo), parametro = rep(h, n_metodo))
      results <- rbind(results, esta_vuelta)
    }
    
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

solouno <- subset(results, metodo ==1 & tamaño ==50 & parametro == 15)


# # Initialize MSE vectors
# mse_I <- numeric(length = nrow(results))
# mse_II <- numeric(length = nrow(results))
# 
# cont = 0
# # Loop over rows of results data frame
# for (i in 1:nrow(results)) {
#   dens_est <- results[i, "dato"]
#   dens_real <- real_dens(grid)
#   metodo <- results[i, "metodo"]
#   parametro <- results[i, "parametro"]
#   
#   # Calculate MSE for method I
#   if (metodo == 1) {
#     mse_I[i] <- MSE(dens_est, dens_real, grid)
#   }
#   
#   # Calculate MSE for method II
#   if (metodo == 2) {
#     mse_II[i] <- MSE(dens_est, dens_real, grid)
#   }
#   cont = cont + 1
#   print(cont)
# }
# 
# # Combine MSE vectors into a data frame
# mse <- data.frame(metodo = rep(c("I", "II"), each = length(ns) * M * 3 * 5),
#                   tamaño = rep(rep(ns, each = M * 3 * 5), 2),
#                   parametro = rep(c(rep(c(15, 30, 50), each = M * 5),
#                                     rep(c(5, 15, 30, 45, 60), each = M)), 2),
#                   mse = c(mse_I, mse_II))
# 
# # Compute average MSE by method, sample size, and parameter
# mse_promedio <- aggregate(mse ~ metodo + tamaño + parametro, data = mse, FUN = mean)
# 
# 
# mse_by_size <- data.frame(metodo = rep(c("I", "II"), each = length(ns) * M),
#                           tamaño = rep(rep(ns, each = M), 2),
#                           mse = c(mse_I, mse_II))
# 
# mse_promedio <- aggregate(mse ~ metodo + tamaño, data = mse_by_size, FUN = mean); mse_promedio

# Compute the configuration with minimum average MSE
min_mse_row <- mse_promedio[which.min(mse_promedio$mse), ]
min_mse_method <- min_mse_row$metodo
min_mse_size <- min_mse_row$tamaño
min_mse_param <- min_mse_row$parametro

# Print the result
cat("The configuration with the minimum average MSE is",
    "method", min_mse_method,
    "for sample size", min_mse_size,
    "and parameter", min_mse_param, "\n")

