#-------------------- GRAFICOS DE SUPERFICIES Y CONTORNOS ----------------------

# Ruta donde se encuentra los archivos que se emplearan 
# Ejemplo
setwd("C:/Users/TP/Documents/Tesis/Propuesta Tesis/Código R/Mi código y bd")

# Paquetes necesarios 
source("Instalacion_Paquetes.R")
source("Carga_Paquetes.R")

# Funciones creadas
source("Funciones.R")

# Primer grafico de superficie y grafico de contorno asociado
# Vector de cuantiles 
cuan_qlnb <- c(1.3, 1.27) 
# Matriz de dispersion
Sigma_qlnb <- matrix(c(0.26, -0.18, -0.18, 0.3), nrow = 2, byrow = TRUE)
# Vector con los cuantiles de una normal estandar 
cuan_norm <- qnorm(p = c(0.75, 0.75))
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb, matriz de dispersion Sigma_qlnb 
# y vector con cuantiles de una normal estandar cuan_norm 
dqlnb <- function(y1, y2, vqb = cuan_qlnb, mSb = Sigma_qlnb, vzb = cuan_norm) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb
dqlnb <- Vectorize(dqlnb, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp <- outer(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), dqlnb)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed", nticks = 4)
# Grafico de contornos
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))

# Segundo grafico de supericie y grafico de contorno asociado
# Vector con los cuantiles de una normal estandar 
cuan_norm1 <- qnorm(p = c(0.5, 0.75))
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb, matriz de dispersion Sigma_qlnb 
# y vector con cuantiles de una normal estandar cuan_norm1 
dqlnb1 <- function(y1, y2, vqb = cuan_qlnb, mSb = Sigma_qlnb, vzb = cuan_norm1) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb1
dqlnb1 <- Vectorize(dqlnb1, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp1 <- outer(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), dqlnb1)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18, 8,0.02), seq(0.1, 8, 0.02), fdp1, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed")
# Grafico de contorno
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp1, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))

# Tercer grafico de superficie y grafico de contorno asociado
# Vector de cuantiles 
cuan_qlnb1 <- c(1.3, 0.9)
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb1, matriz de dispersion Sigma_qlnb 
# y vector con cuantiles de una normal estandar cuan_norm1 
dqlnb2 <- function(y1, y2, vqb = cuan_qlnb1, mSb = Sigma_qlnb, vzb = cuan_norm1) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb2
dqlnb2 <- Vectorize(dqlnb2, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp2 <- outer(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), dqlnb2)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18,8,0.02), seq(0.1,8,0.02), fdp2, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed")
# Grafico de contornos
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp2, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))

# Cuarto gráfico de superficie y grafico de contornos asociado
# Matriz de asociacion
Sigma_qlnb1 <- matrix(c(0.2, -0.18, -0.18, 0.3), nrow = 2, byrow = TRUE)
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb1, matriz de dispersion Sigma_qlnb1 
# y vector con cuantiles de una normal estandar cuan_norm1 
dqlnb3 <- function(y1, y2, vqb = cuan_qlnb1, mSb = Sigma_qlnb1, vzb = cuan_norm1) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb3
dqlnb3 <- Vectorize(dqlnb3, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp3 <- outer(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), dqlnb3)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp3, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed")
# Grafico de contornos
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp3, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))

# Quinto grafico de superficie y grafico de contorno asociado
# Matriz de dispersion
Sigma_qlnb2 <- matrix(c(0.2, 0, 0, 0.3), nrow = 2, byrow = TRUE)
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb1, matriz de dispersion Sigma_qlnb2 
# y vector con cuantiles de una normal estandar cuan_norm1 
dqlnb4 <- function(y1, y2, vqb = cuan_qlnb1, mSb = Sigma_qlnb2, vzb = cuan_norm1) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb4
dqlnb4 <- Vectorize(dqlnb4, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp4 <- outer(seq(0.18, 8, 0.02),seq(0.1, 8, 0.02), dqlnb4)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp4, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed")
# Grafico de contornos
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp4, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))

# Sexto grafico de superficie y grafico de contornos asociado
# Matriz de dispersion
Sigma_qlnb3 <- matrix(c(0.2, 0.15, 0.15, 0.3), nrow = 2, byrow = TRUE)
# Funcion de la densidad de la distribucion log-normal bivariada basada en 
# cuantiles con vector de cuantiles cuan_qlnb1, matriz de dispersion Sigma_qlnb3 
# y vector con cuantiles de una normal estandar cuan_norm1 
dqlnb5 <- function(y1, y2, vqb = cuan_qlnb1, mSb = Sigma_qlnb3, vzb = cuan_norm1) 
  dqlnp(my = matrix(c(y1, y2), nrow = 1), vq = vqb, mS = mSb, vz = vzb)
# Vectorizacion de la funcion dqlnb5
dqlnb5 <- Vectorize(dqlnb5, vectorize.args = c("y1", "y2"))
# Valores de la densidad
fdp5 <- outer(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), dqlnb5)
# Grafico de superficie
par(pty = "s") 
persp(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp5, theta = 53, phi = 30, 
      col = "steelblue", border = NA, shade = 0.7, xlab = "Y1", ylab = "Y2",
      zlab = "FDP", ticktype = "detailed")
# Grafico de contornos
par(pty = "s") 
contour(seq(0.18, 8, 0.02), seq(0.1, 8, 0.02), fdp5, xlab = "Y1", ylab = "Y2", 
        las = 1, drawlabels = FALSE, levels = c(0.5,0.3,0.1,0.02,0.005))








