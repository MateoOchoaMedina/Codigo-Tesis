#---------------------------------- APLICACION --------------------------------

# Ruta donde se encuentra los archivos que se emplearan 
# Ejemplo
setwd("C:/Users/TP/Documents/Tesis/Propuesta Tesis/Código R/Mi código y bd")

# Paquetes necesarios 
source("Instalacion_Paquetes.R")
source("Carga_Paquetes.R")

# Funciones creadas
source("Funciones.R")

#------------ BASE DE DATOS ------------

# Lectura de la base de datos
nutricion <- read.csv("Estado_nutricional.csv")

# Estructura de la base de datos 
str(nutricion)

# Se crea una variable con la edad en anos de los ninos, se redefinen las 
# variables sexo y comuna como categoricas, y se construye la base de datos 
# (con la que se trabajara) considerando solo los ninos entre 24 y 66 meses 
# (2 y 5.5 anos) del corregimiento de San Antonio de Prado en el ano 2018
nutricion_sap <- nutricion %>% 
  mutate(Edad_anos = Edad_dias/365,
         rango_edad = cut(x = Edad_anos, breaks = seq(2, 5.5, 0.5), 
                          include.lowest = TRUE, 
                          labels = c("2 - 2.5", "2.5 - 3", "3 - 3.5", "3.5 - 4",
                                     "4 - 4.5", "4.5 - 5", "5 - 5.5")),
         across(.cols = c("sexo", "comuna"), .fns = as.factor)) %>%
  filter(año == 2018, 
         comuna == "San Antonio de Prado", 
         Edad_anos >= 2, 
         Edad_anos <= 5.5)

#------------ ESTADISTICA DESCRIPTIVA ------------

# Bagplot del peso vs. estatura 
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0)) 
with(nutricion_sap, bagplot(x = peso, y = estatura, show.whiskers = FALSE,
                            show.looppoints = FALSE, show.bagpoints = FALSE, 
                            col.loophull = "lightgray", col.baghull = "gray", 
                            pch = 1, cex = 1.5, xlab = "", ylab = "", 
                            xaxt = "n", yaxt = "n"))
box()
axis(side = 1, at = round(seq(from = 9.2, to = 33, length.out = 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Peso", line = 1.5)
axis(side = 2, at = round(seq(from = 77.1, to = 127.9, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Estatura", line = 3)
  
# Grafico de violin para estatura discriminando por grupos de edades y sexo
par(mfrow = c(1, 1), mar = c(2, 4, 0.6, 0.6))
vioplot(formula = estatura ~ sexo + rango_edad, data = nutricion_sap,
        col = c("white", "gray"), border = "black", rectCol = "white", 
        lineCol = "black", pchMed = 20, colMed = "black", cex = 0.8,
        at = c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17, 19:20), xaxt = "n", 
        yaxt = "n", xlab = "", ylab = "")
# Dibuja las posibles estaturas atipicas discriminando por grupos de edades y 
# sexo
points(x = c(2, 7, 10, 11, 11, 11, 13, 13, 13, 14, 14, 16, 16, 17, 17, 17, 17, 
             17, 19, 19, 19, 19, 20), 
       y = c(99, 105.5, 85, 87.7, 107.8, 89, 116, 112, 112, 115.5, 89, 117, 94,
             117.2, 96, 118, 96, 96, 127.9, 97, 124, 96, 126), pch = 20, 
       cex = 0.8)
# Personaliza el eje x del grafico de violin
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.6, 13.6, 16.6, 19.6), tick = FALSE,
     mgp = c(0, 0, 0), labels = levels(nutricion_sap$rango_edad))
title(xlab = "Edad", line = 1)
# Personaliza el eje y del grafico de violin 
axis(side = 2, at = round(seq(from = 77.1, to = 127.9, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Estatura", line = 3)
# Pone una leyenda que acompana el grafico de violin 
legend("topleft", legend = c("Mujer", "Hombre"), fill = c("white", "gray"), 
       bty = "n", horiz = FALSE)

# Grafico de violin para peso discriminando por grupos de edades y sexo
par(mfrow = c(1, 1), mar = c(2, 4, 0.6, 0.6))
vioplot(formula = peso ~ sexo + rango_edad, data = nutricion_sap, 
        col = c("white", "gray"), border = "black", rectCol = "white", 
        lineCol = "black", pchMed = 20, colMed = "black", cex = 0.8,
        at = c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17, 19:20), xaxt = "n", 
        yaxt = "n", xlab = "", ylab = "")
# Dibuja los posibles pesos atipicos discriminando por grupos de edades y sexo
points(x = c(1, 2, 2, 2, 4, 4, 4, 5, 5, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10, 
             10, 10, 10, 11, 11, 11, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14,
             14, 14, 14, 14, 16, 16, 16, 16, 16, 17, 17, 17, 19, 19, 19, 19, 19,
             19, 19, 19, 19, 20, 20, 20, 20), 
       y = c(15.5, 16.5, 16, 16.3, 19.1, 19, 18.7, 19, 18, 19.9, 19.6, 19.7, 
             19.5, 21.6, 22.7, 22.1, 22.8, 21.6, 21.4, 19.3, 19.6, 10, 19.3, 
             23.7, 20, 20.1, 22.6, 22.7, 23.66, 22, 21.3, 20.3, 20.3, 20.2, 
             20.2, 24.5, 21.8, 24.4, 24.7, 22, 24.6, 24.1, 25, 25.4, 24.5, 23.7,
             24, 24.6, 26.64, 28.1, 31.95, 26.4, 29.1, 24.9, 25, 33, 25.9, 28.4,
             28, 27.5, 26, 26), pch = 20, cex = 0.8)
# Personaliza el eje x del grafico de violin
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.6, 13.6, 16.6, 19.6), tick = FALSE,
     mgp = c(0, 0, 0), labels = levels(nutricion_sap$rango_edad))
title(xlab = "Edad", line = 1)
# Personaliza el eje y del grafico de violin
axis(side = 2, at = round(seq(from = 9.2, to = 33, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Peso", line = 2.7)
# Pone una leyenda que acompana el grafico de violin
legend("topleft", legend = c("Mujer", "Hombre"), fill = c("white", "gray"), 
       bty = "n", horiz = FALSE)

#----------- INFERENCIAS DEL MODELO ------------

# Matriz con el logaritmo del peso y la estatura (logaritmo de las respuestas)
res <- with(nutricion_sap, cbind(peso, estatura))
log_res <- log(res)

# Matriz de diseno tomando como variables regresoras la edad y el sexo
diseno <- with(nutricion_sap, model.matrix(~ Edad_anos + sexo))

# Estimacion de la matriz de dispersion para modelar conjuntamente los    
# percentiles 5, 10, 25, 50, 75, 90 y 95, del peso y la estatura
Sigma_est <- mle_Sigma(mlogy = log_res, mX = diseno)

# Matriz cuyas columnas son los vectores con los percentiles 5, 10, 25, 50, 75, 
# 90 y 95 de una normal estandar
perc_norm_est <- matrix(qnorm(p = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)), 
                        nrow = ncol(log_res), ncol = 7, byrow = TRUE)

# Estimacion de la matriz con los coeficientes de regresion para modelar el 
# vector con los percentiles 5, 10, 25, 50, 75, 90 y 95, del peso y la estatura
B_est_perc <- mapply(mle_B, mlogy = list(log_res), mX = list(diseno), 
                     mSg = list(Sigma_est), 
                     vz = apply(perc_norm_est, 2, matrix, simplify = FALSE),
                     SIMPLIFY = FALSE)

# Estimacion de las varianzas y covarianzas asintoticas de los coeficientes de 
# regresion estimados cuando se modela el vector con los percentiles 5, 10, 25,
# 50, 75, 90 y 95, del peso y la estatura
varcov_est_betas <- mapply(est_varcov_coefs, mX = list(diseno), 
                           mSg = list(Sigma_est), 
                           vz = apply(perc_norm_est, 2, matrix, simplify = FALSE), 
                           SIMPLIFY = FALSE)

# Errores estandar asintoticos de los coeficientes de regresion estimados cuando
# se modela el vector con los percentiles 5, 10, 25, 50, 75, 90 y 95, del peso
# y la estatura
error_est_betas <- lapply(X = varcov_est_betas, 
                          FUN = function(mvcbg) sqrt(diag(mvcbg)))

# Limites de los intervalos de confianza asintoticos del 95% para los coeficientes
# de regresion cuando se modela el vector con los percentiles 5, 10, 25, 50, 75,
# 90 y 95, del peso y la estatura
lim_int_betas <- mapply(function(bg, sebg) {matrix(c(bg - qnorm(p = 0.975) * sebg, 
                                                     bg + qnorm(p = 0.975) * sebg),
                                                   nrow = 2, byrow = TRUE)},
                        bg = lapply(B_est_perc, as.vector), 
                        sebg = error_est_betas, SIMPLIFY = FALSE)

# Estadisticos Wald para probar la significancia de los coeficientes de regresion
# cuando se modela el vector con los percentiles 5, 10, 25, 50, 75, 90 y 95, del
# peso y la estatura
est_Wald_betas <- mapply(function(bg, sebg) (bg/sebg)^2, 
                         bg = lapply(B_est_perc, as.vector), 
                         sebg = error_est_betas, SIMPLIFY = FALSE)

# Valores p del test de Wald para probar la significancia de los coeficientes de
# regresion cuando se modela el vector con los percentiles 5, 10, 25, 50, 75, 90
# y 95, del peso y la estatura
vp_Wald_betas <- mapply(function(eW) pchisq(eW, df = 1, lower.tail = FALSE),
                        eW = est_Wald_betas, SIMPLIFY = FALSE)

# Adecuacion de los modelos

# Cuantiles teoricos de una distribucion chi-cuadrado con 2 grados de libertad
cuan_teo_chi <- qchisq(1:nrow(nutricion_sap)/(nrow(nutricion_sap) + 1), df = 2)

# Simulacion de las bandas de Atkinson 
set.seed(30129)
sim_band_Atk <- band_Atkin(n = nrow(nutricion_sap), p = ncol(log_res))

# Distancias de Mahalanobis empiricas cuando se modela el vector con los 
# percentiles 5, 10, 25, 50, 75, 90 y 95, del peso y la estatura
dist_Maha_emp_perc <- mapply(maha_emp, mlogy = list(log_res), mX = list(diseno),
                             mBg = B_est_perc, mSg = list(Sigma_est), 
                             vz = asplit(perc_norm_est, 2), SIMPLIFY = FALSE)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 5
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[1]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 10
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[2]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 25
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[3]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 50
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[4]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 75
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[5]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 90
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[6]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Q-Q plot con bandas de Atkinson para evaluar la adecuacion del modelo para el  
# vector con los percentiles 95
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
plot(x = cuan_teo_chi, y = dist_Maha_emp_perc[[7]], xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", pch = 20)
abline(a = 0, b = 1, col = "gray55", lty = 3, lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 1]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
lines(x = cuan_teo_chi, y = sort(sim_band_Atk[, 2]), xlim = c(0, 16), 
      ylim = c(0, 29), xlab = "", ylab = "", lwd = 1)
axis(side = 1, at = round(seq(from = 0, to = 15.29, length.out= 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Cuantiles teóricos", line = 1.5)
axis(side = 2, at = round(seq(from = 0, to = 28.48, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Cuantiles observados", line = 2.5)

# Modelamiento (estimacion) de los percentiles 5, 10, 25, 50, 75, 90 y 95, para  
# el peso y la estatura por separado

rej_edad <- seq(from = 2, to = 5.5, by = 0.001)   # Rejilla para la edad
vec_ceros <- rep(x = 0, times = length(rej_edad)) # Vector de ceros 
vec_unos <- rep(x = 1, times = length(rej_edad))  # Vector de unos

# Matriz de diseno conformada por la rejilla para la edad y el vector de ceros
# (emulan las variables regresoras, edad y sexo, respectivamente)
dis_muj <- model.matrix(~ rej_edad + vec_ceros)

# Matriz de diseno conformada por la rejilla para la edad y el vector de unos
# (emulan las variables regresoras, edad y sexo, respectivamente)
dis_hom <- model.matrix(~ rej_edad + vec_unos)

# Modelamiento (estimacion) de los percentiles 5, 10, 25, 50, 75, 90 y 95, para 
# el peso de las mujeres
perc_est_pes_muj <- lapply(B_est_perc, function(mX, mBg) exp(mX %*% mBg[, 1]),
                           mX = dis_muj)

# Modelamiento (estimacion) de los percentiles 5, 10, 25, 50, 75, 90 y 95, para
# el peso de los hombres
perc_est_pes_hom <- lapply(B_est_perc, function(mX, mBg) exp(mX %*% mBg[, 1]),
                           mX = dis_hom)

# Modelamiento (estimacion) de los percentiles 5, 10, 25, 50, 75, 90 y 95, para
# la estatura de las mujeres
perc_est_est_muj <- lapply(B_est_perc, function(mX, mBg) exp(mX %*% mBg[, 2]),
                           mX = dis_muj)

# Modelamiento (estimacion) de los percentiles 5, 10, 25, 50, 75, 90 y 95, para
# la estatura de los hombres
perc_est_est_hom <- lapply(B_est_perc, function(mX, mBg) exp(mX %*% mBg[, 2]),
                           mX = dis_hom)

# Grafico de dispersion del peso vs. edad para las ninas, con curvas cuantilicas 
# estimadas de los percentiles 5, 10, 25, 50, 75, 90 y 95, del peso 
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
with(nutricion_sap, plot(x = Edad_anos[sexo == "F"], y = peso[sexo == "F"], 
                         xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
                         col = "gray69"))
axis(side = 1, at = round(seq(from = 2, to = 5.5, length.out = 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Edad", line = 1.5)
axis(side = 2, at = round(seq(from = 9.2, to = 33, length.out = 5), 1), las = 2, 
     mgp = c(0, 0.6, 0))
title(ylab = "Peso (mujeres)", line = 2.7)
# Curva cuantilica estimada del percentil 5
lines(x = rej_edad, y = perc_est_pes_muj[[1]], lwd = 2)
# Curva cuantilica estimada del percentil 10
lines(x = rej_edad, y = perc_est_pes_muj[[2]], lwd = 2)
# Curva cuantilica estimada del percentil 25
lines(x = rej_edad, y = perc_est_pes_muj[[3]], lwd = 2)
# Curva cuantilica estimada del percentil 50
lines(x = rej_edad, y = perc_est_pes_muj[[4]], lwd = 2)
# Curva cuantilica estimada del percentil 75
lines(x = rej_edad, y = perc_est_pes_muj[[5]], lwd = 2)
# Curva cuantilica estimada del percentil 90
lines(x = rej_edad, y = perc_est_pes_muj[[6]], lwd = 2)
# Curva cuantilica estimada del percentil 95
lines(x = rej_edad, y = perc_est_pes_muj[[7]], lwd = 2)

# Grafico de dispersion de la estatura vs. edad para las ninas, con curvas 
# cuantilicas estimadas de los percentiles 5, 10, 25, 50, 75, 90 y 95, de la 
# estatura
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
with(nutricion_sap, plot(x = Edad_anos[sexo == "F"], y = estatura[sexo == "F"], 
                         xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
                         col = "gray69"))
axis(side = 1, at = round(seq(from = 2, to = 5.5, length.out = 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Edad", line = 1.5)
axis(side = 2, at = round(seq(from = 77.1, to = 127.9, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Estatura (mujeres)", line = 3.1)
# Curva cuantilica estimada del percentil 5
lines(x = rej_edad, y = perc_est_est_muj[[1]], lwd = 2)
# Curva cuantilica estimada del percentil 10
lines(x = rej_edad, y = perc_est_est_muj[[2]], lwd = 2)
# Curva cuantilica estimada del percentil 25
lines(x = rej_edad, y = perc_est_est_muj[[3]], lwd = 2)
# Curva cuantilica estimada del percentil 50
lines(x = rej_edad, y = perc_est_est_muj[[4]], lwd = 2)
# Curva cuantilica estimada del percentil 75
lines(x = rej_edad, y = perc_est_est_muj[[5]], lwd = 2)
# Curva cuantilica estimada del percentil 90
lines(x = rej_edad, y = perc_est_est_muj[[6]], lwd = 2)
# Curva cuantilica estimada del percentil 95
lines(x = rej_edad, y = perc_est_est_muj[[7]], lwd = 2)

# Grafico de dispersion del peso vs. edad para los ninos, con curvas cuantilicas
# estimadas de los percentiles 5, 10, 25, 50, 75, 90 y 95, del peso
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
with(nutricion_sap, plot(x = Edad_anos[sexo == "M"], y = peso[sexo == "M"], 
                         xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
                         col = "gray69"))
axis(side = 1, at = round(seq(from = 2, to = 5.5, length.out = 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Edad", line = 1.5)
axis(side = 2, at = round(seq(from = 9.55, to = 28, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Peso (hombres)", line = 2.7)
# Curva cuantilica estimada del percentil 5
lines(x = rej_edad, y = perc_est_pes_hom[[1]], lwd = 2)
# Curva cuantilica estimada del percentil 10
lines(x = rej_edad, y = perc_est_pes_hom[[2]], lwd = 2)
# Curva cuantilica estimada del percentil 25
lines(x = rej_edad, y = perc_est_pes_hom[[3]], lwd = 2)
# Curva cuantilica estimada del percentil 50
lines(x = rej_edad, y = perc_est_pes_hom[[4]], lwd = 2)
# Curva cuantilica estimada del percentil 75
lines(x = rej_edad, y = perc_est_pes_hom[[5]], lwd = 2)
# Curva cuantilica estimada del percentil 90
lines(x = rej_edad, y = perc_est_pes_hom[[6]], lwd = 2)
# Curva cuantilica estimada del percentil 95
lines(x = rej_edad, y = perc_est_pes_hom[[7]], lwd = 2)

# Grafico de dispersion de la estatura vs. edad para los hombres, con curvas 
# cuantilicas estimadas de los percentiles 5, 10, 25, 50, 75, 90 y 95, de la 
# estatura
par(mfrow = c(1, 1), pty = "s", mar = c(2.5, 2.7, 0.8, 0))
with(nutricion_sap, plot(x = Edad_anos[sexo == "M"], y = estatura[sexo == "M"], 
                         xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
                         col = "gray69"))
axis(side = 1, at = round(seq(from = 2, to = 5.5, length.out = 5), 1), 
     mgp = c(0, 0.5, 0))
title(xlab = "Edad", line = 1.5)
axis(side = 2, at = round(seq(from = 80.6, to = 126, length.out = 5), 1), 
     las = 2, mgp = c(0, 0.6, 0))
title(ylab = "Estatura (hombres)", line = 3.1)
# Curva cuantilica estimada del percentil 5
lines(x = rej_edad, y = perc_est_est_hom[[1]], lwd = 2)
# Curva cuantilica estimada del percentil 10
lines(x = rej_edad, y = perc_est_est_hom[[2]], lwd = 2)
# Curva cuantilica estimada del percentil 25
lines(x = rej_edad, y = perc_est_est_hom[[3]], lwd = 2)
# Curva cuantilica estimada del percentil 50
lines(x = rej_edad, y = perc_est_est_hom[[4]], lwd = 2)
# Curva cuantilica estimada del percentil 75
lines(x = rej_edad, y = perc_est_est_hom[[5]], lwd = 2)
# Curva cuantilica estimada del percentil 90
lines(x = rej_edad, y = perc_est_est_hom[[6]], lwd = 2)
# Curva cuantilica estimada del percentil 95
lines(x = rej_edad, y = perc_est_est_hom[[7]], lwd = 2)

# Criterio de informacion de Akaike (AIC)

# Numero de parametros del modelo propuesto
num_param <- length(vec(B_est_perc[[1]])) + length(vech(Sigma_est))
# Numero de parametros de la regresion cuantilica univariada para el peso y la
# estatura
num_param_pes_est <- length(B_est_perc[[1]][, 1]) + 1

# Para el vector de percentiles 5
# Modelo propuesto
dqln2_5 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                  vq = apply(diseno, 1, simplify = FALSE,
                             FUN = function(reg) {
                               exp(t(B_est_perc[[1]]) %*% reg)}),
                  mS = list((Sigma_est + t(Sigma_est))/2), 
                  vz = list(perc_norm_est[, 1]))
2 * num_param - 2 * sum(log(dqln2_5)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_5 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                       vq = apply(diseno, 1, simplify = FALSE,
                                  FUN = function(reg) {
                                    exp(t(B_est_perc[[1]][, 1]) %*% reg)}),
                       mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                       vz = list(perc_norm_est[1, 1]))
# Estatura
dqln_est_5 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                       vq = apply(diseno, 1, simplify = FALSE,
                                  FUN = function(reg) {
                                    exp(t(B_est_perc[[1]][, 2]) %*% reg)}),
                       mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                       vz = list(perc_norm_est[2, 1]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_5))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_5))) # AIC

# Para el vector de percentiles 10
# Modelo propuesto
dqln2_10 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[2]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 2]))
2 * num_param - 2 * sum(log(dqln2_10)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_10 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[2]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 2]))
# Estatura
dqln_est_10 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[2]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 2]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_10))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_10))) # AIC

# Para el vector de percentiles 25
# Modelo propuesto
dqln2_25 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[3]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 3]))
2 * num_param - 2 * sum(log(dqln2_25)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_25 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[3]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 3]))
# Estatura
dqln_est_25 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[3]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 3]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_25))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_25))) # AIC

# Para el vector de percentiles 50
# Modelo propuesto
dqln2_50 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[4]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 4]))
2 * num_param - 2 * sum(log(dqln2_50)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_50 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[4]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 4]))
# Estatura
dqln_est_50 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[4]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 4]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_50))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_50))) # AIC

# Para el vector de percentiles 75
# Modelo propuesto
dqln2_75 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[5]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 5]))
2 * num_param - 2 * sum(log(dqln2_75)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_75 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[5]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 5]))
# Estatura
dqln_est_75 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[5]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 5]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_75))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_75))) # AIC

# Para el vector de percentiles 90
# Modelo propuesto
dqln2_90 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[6]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 6]))
2 * num_param - 2 * sum(log(dqln2_90)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_90 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[6]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 6]))
# Estatura
dqln_est_90 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[6]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 6]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_90))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_90))) # AIC

# Para el vector de percentiles 95
# Modelo propuesto
dqln2_95 <- mapply(dqlnp, my = apply(res, 1, matrix, nrow = 1, simplify = FALSE),
                   vq = apply(diseno, 1, simplify = FALSE,
                              FUN = function(reg) {
                                exp(t(B_est_perc[[7]]) %*% reg)}),
                   mS = list((Sigma_est + t(Sigma_est))/2), 
                   vz = list(perc_norm_est[, 7]))
2 * num_param - 2 * sum(log(dqln2_95)) # AIC
# Regresion cuantilica univariada
# Peso
dqln_pes_95 <- mapply(dqlnp, my = apply(matrix(res[, 1], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[7]][, 1]) %*% reg)}),
                      mS = list(matrix(Sigma_est[1, 1], nrow = 1)), 
                      vz = list(perc_norm_est[1, 7]))
# Estatura
dqln_est_95 <- mapply(dqlnp, my = apply(matrix(res[, 2], ncol = 1), 1, matrix, 
                                        nrow = 1, simplify = FALSE),
                      vq = apply(diseno, 1, simplify = FALSE,
                                 FUN = function(reg) {
                                   exp(t(B_est_perc[[7]][, 2]) %*% reg)}),
                      mS = list(matrix(Sigma_est[2, 2], nrow = 1)), 
                      vz = list(perc_norm_est[2, 7]))
(2 * num_param_pes_est - 2 * sum(log(dqln_pes_95))) +
  (2 * num_param_pes_est - 2 * sum(log(dqln_est_95))) # AIC

