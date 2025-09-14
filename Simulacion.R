#-------------------------------- SIMULACION -----------------------------------

# Ruta donde se encuentra los archivos que se emplearan 
# Ejemplo
setwd("C:/Users/TP/Documents/Tesis/Propuesta Tesis/Código R/Mi código y bd")

# Paquetes necesarios 
source("Instalacion_Paquetes.R")
source("Carga_Paquetes.R")

# Funciones creadas
source("Funciones.R")

# Objetos guardados en el area de trabajo correspondiente a "Aplicacion.R"
load("Aplicacion.RData")

# Funcion para estimar los betas, sigmas y calcular los errores estandar del 
# modelo cuantilico bivariado log-normal simulando log(peso) y log(estatura) 
# para N replicas Monte Carlo, y para estimar los betas del modelo cuantilico 
# univariado para el peso y la estatura
esmbu <- function(n, N, eB, eS, vzs, semx, semy){
  # n: Cantidad(es) de datos simulados (tamano(s) muestral(es))
  # N: Numero de simulaciones Monte Carlo para cada tamano muestral 
  # eB: MLE de la matriz con los coeficientes de regresion para los datos reales.
  # Se encuentra con la funcion mle_B
  # eS: MLE de la matriz de dispersion para los datos reales. Se encuentra con 
  # la funcion mle_Sigma
  # vzs: Vector columna dada por el usuario con los cuantiles de una distribucion 
  # normal estandar 
  # semx: Valores distintos de semillas para generar observaciones de la edad y 
  # el sexo, asociados a cada tamano muestral. Estos deben ser los mismos
  # independientemente del vector de cuantiles que se modela. Si no se quiere
  # fijar semillas, se cambian los valores por NULL
  # semy: Valores distintos de semillas para generar obervaciones del peso y la
  # estatura, asociados a cada tamano muestral. Estos deben cambiar con el 
  # vector de cuantiles que se modela. Si no se quiere fijar semillas, se 
  # cambian los valores por NULL
  set.seed(semx)
  # Generacion de observaciones de una distribucion uniforme con limite inferior
  # 2 y limite superior 5.5 para emular la variable edad 
  sed <- runif(n = n, min = 2, max = 5.5)
  # Generacion de observaciones de una distribucion Bernoulli con probabilidad 
  # exito 0.5 para emular la variable sexo (0: femenino, 1: masculino)
  sse <- rbinom(n = n, size = 1, prob = 0.5)
  # Matriz de diseno conformada por las variables simuladas de la edad y el sexo
  md <- model.matrix(~ sed + sse)
  # Matriz identidad
  I2 <- diag(nrow = 2)
  # (Sigma gorro hadamard I)^(1/2) z
  vHz <- (eS * I2)^(1/2) %*% vzs 
  # Matriz con columnas iguales al vector vHz
  mHz <- matrix(vHz, nrow = 2, ncol = n) 
  # Matriz con columnas iguales a las medias de log(peso) y log(estatura) para 
  # la i-esima observacion, con i = 1,...,n, es decir, (B gorro)' x_i - (Sigma
  # gorro hadamard I)^(1/2) z
  mmy <- t(md %*% eB) - mHz
  # Generacion de observaciones de una normal bivariada con vector de medias mmy
  # y matriz de varianzas y covarianzas eS para emular log(peso) y log(estatura)
  # para N replicas Monte Carlo
  set.seed(semy)
  slogpe <- replicate(N, t(apply(mmy, 2, rmnorm, n = 1, varcov = eS)), 
                      simplify = FALSE)
  # Funcion para calcular las estimaciones del modelo bivariado log-normal para 
  # los respectivos cuantiles del peso y la estatura 
  estmbln <- function(smlogy, smX = md, vcne = vzs){
    # smlogy: Matriz con columnas igual a la simulacion del logaritmo del peso y 
    # la estatura
    # smX: Matriz de diseno del modelo obtenido via simulacion
    # vcne: Vector columna dada por el usuario con los cuantiles de una 
    # distribucion normal estandar
    # MLE de la matriz de dispersion
    estS <- mle_Sigma(mlogy = smlogy, mX = smX)
    # MLE de la matriz con los coeficientes de regresion
    estB <- mle_B(mlogy = smlogy, mX = smX, mSg = estS, vz = vcne)
    # Estimacion de las varianzas y covarianzas asintoticas de los coeficientes 
    # de regresion estimados 
    estvcs <- est_varcov_coefs(mX = smX, mSg = estS, vz = vcne)
    # Aplicacion del operador vec para el MLE de la matriz con los coeficientes 
    # de regresion 
    vestB <- vec(estB)
    # Aplicacion del operador vech para el MLE de la matriz de dispersion 
    vestS <- vech(estS)
    # Vector con los errores estandar de los coeficientes de regresion estimados
    vses <- sqrt(diag(estvcs))
    # Vector columna con los elementos de vestB, vestS y vses
    parg <- matrix(c(vestB, vestS, vses))
    # Retorna el vector parg
    return(parg)
  }
  # Lista con las estimaciones del modelo bivariado usando las observaciones
  # simuladas de log(peso) y log(estatura) para N replicas Monte Carlo
  lestsmbln <- lapply(slogpe, estmbln)
  # Matriz cuyas columnas tienen las estimaciones del modelo bivariado para cada
  # replica Monte Carlo
  mestsmbln <- matrix(unlist(lestsmbln), ncol = N)
  # Funciones para calcular respectivamente las estimaciones de los coeficientes
  # de regresion del modelo univariado para un cuantil del peso y de la estatura
  ebmup <- function(smlogy, smX = md, vcne = vzs){
    # Nota: El significado de los argumentos coinciden con los de la funcion 
    # estmbln
    crgp <- coefficients(rq(exp(smlogy[, 1]) ~ smX[, 2] + smX[, 3], 
                            tau = pnorm(vcne[1])))
    return(crgp)
  }
  ebmue <- function(smlogy, smX = md, vcne = vzs){
    # Nota: El significado de los argumentos coinciden con los de la funcion
    # estmbln
    crge <- coefficients(rq(exp(smlogy[, 2]) ~ smX[, 2] + smX[, 3], 
                            tau = pnorm(vcne[2])))
    return(crge)
  }
  # Listas que guardan respectivamente las estimaciones de los coeficientes de 
  # regresion del modelo univariado aplicando exponencial a las observaciones 
  # simuladas de log(peso) y log(estatura) para N replicas Monte Carlo
  lebsmup <- lapply(slogpe, ebmup)
  lebsmue <- lapply(slogpe, ebmue)
  # Matrices cuyas columnas corresponden a las estimaciones de los coeficientes 
  # de los modelos univariados para cada replica Monte Carlo 
  mebsmup <- matrix(unlist(lebsmup), nrow = 3)
  mebsmue <- matrix(unlist(lebsmue), nrow = 3)
  # Lista con la matriz de diseno simulada y con las estimaciones del modelo
  # bivariado y univariado
  res <- list(md, mestsmbln, mebsmup, mebsmue)
  # Retorna la lista res
  return(res)
}

# Aplicacion de la funcion esmbu() para tamanos de muestra n = 100, 500, 1000, 
# 5000 con N = 10000 simulaciones Monte Carlo con parametros verdaderos 
# contenidos en la estimación de la matriz de coeficientes de regresion y de la
# matriz de dispersion en "Aplicacion.R" para el vector de percentiles 25, 50 y
# 75, respectivamente
ests_sim_p25 <- mapply(esmbu, n = list(100, 500, 1000, 5000), N = list(10000),
                       eB = list(B_est_perc[[3]]), eS = list(Sigma_est), 
                       vzs = list(perc_norm_est[, 3]), 
                       semx = list(523, 715, 938, 104), 
                       semy = list(112, 143, 905, 729), SIMPLIFY = FALSE)
ests_sim_p50 <- mapply(esmbu, n = list(100, 500, 1000, 5000), N = list(10000),
                       eB = list(B_est_perc[[4]]), eS = list(Sigma_est), 
                       vzs = list(perc_norm_est[, 4]), 
                       semx = list(523, 715, 938, 104), 
                       semy = list(826, 295, 419, 612), SIMPLIFY = FALSE)
ests_sim_p75 <- mapply(esmbu, n = list(100, 500, 1000, 5000), N = list(10000),
                       eB = list(B_est_perc[[5]]), eS = list(Sigma_est), 
                       vzs = list(perc_norm_est[, 5]), 
                       semx = list(523, 715, 938, 104), 
                       semy = list(286, 307, 999, 521), SIMPLIFY = FALSE)

#--- DESEMPENO DEL MLE DE LOS PARAMETROS DEL MODELO ---

# Listas con cuatro sublistas para los tamanos de muestra considerados, los 
# cuales tienen las estimaciones de los betas y sigmas del modelo cuantilico  
# bivariado log-normal para el vector de percentiles 25, 50 y 75, resumidos 
# en una matriz por columna para cada replica Monte Carlo
ebs_mb_p25 <- lapply(ests_sim_p25, function(mebs) mebs[[2]][1:9, ])
ebs_mb_p50 <- lapply(ests_sim_p50, function(mebs) mebs[[2]][1:9, ])
ebs_mb_p75 <- lapply(ests_sim_p75, function(mebs) mebs[[2]][1:9, ])

# Mediana de las estimaciones de cada parametro en las replicas Monte Carlo
# para cada tamano muestral 
med_ebs_p25 <- lapply(ebs_mb_p25, function(mebs) matrix(apply(mebs, 1, median), 
                                                        ncol = 1))
med_ebs_p50 <- lapply(ebs_mb_p50, function(mebs) matrix(apply(mebs, 1, median), 
                                                        ncol = 1))
med_ebs_p75 <- lapply(ebs_mb_p75, function(mebs) matrix(apply(mebs, 1, median), 
                                                        ncol = 1))

# MAD de las estimaciones de cada parametro en las replicas Monte Carlo para cada
# tamano muestral
mad_ebs_p25 <- lapply(ebs_mb_p25, function(mebs) matrix(apply(mebs, 1, mad), 
                                                        ncol = 1))
mad_ebs_p50 <- lapply(ebs_mb_p50, function(mebs) matrix(apply(mebs, 1, mad), 
                                                        ncol = 1))
mad_ebs_p75 <- lapply(ebs_mb_p75, function(mebs) matrix(apply(mebs, 1, mad), 
                                                        ncol = 1))

# Mediana y MAD de las estimaciones para cada tamano muestral
mm_ebs_p25 <- mapply(cbind, med_ebs_p25, mad_ebs_p25, SIMPLIFY = FALSE)
mm_ebs_p50 <- mapply(cbind, med_ebs_p50, mad_ebs_p50, SIMPLIFY = FALSE)
mm_ebs_p75 <- mapply(cbind, med_ebs_p75, mad_ebs_p75, SIMPLIFY = FALSE)
# Resultados para el vector de percentiles 25
round(cbind(c(vec(B_est_perc[[3]]), vech(Sigma_est)), 
            do.call(cbind, mm_ebs_p25)), 4)
# Resultados para el vector de percentiles 50
round(cbind(c(vec(B_est_perc[[4]]), vech(Sigma_est)),
            do.call(cbind, mm_ebs_p50)), 4)
# Resultados para el vector de percentiles 75
round(cbind(c(vec(B_est_perc[[5]]), vech(Sigma_est)),
            do.call(cbind, mm_ebs_p75)), 4)

round(do.call(cbind, mm_ebs_p75), 4) 

#---- DESEMPENO DE LAS ESTIMACIONES DE LOS ERRORES ESTANDAR E INTERVALOS DE ---- 
#--------------------------------CONFIANZA -------------------------------------

# Resultados para el vector de percentiles 25
des_see_p25 <- lapply(FUN = function(rsim) {
  cn <- qnorm(p = 0.975) 
  vb <- as.vector(B_est_perc[[3]])
  mdn <- rsim[[1]]
  ben <- rsim[[2]][1:6, ]
  seen <- rsim[[2]][10:15, ]
  vrb <- est_varcov_coefs(mX = mdn, mSg = Sigma_est, vz = perc_norm_est[, 3])
  serb <- sqrt(diag(vrb))
  mmedmad <- t(apply(seen, 1, function(seeb) c(median(seeb), mad(seeb))))
  lisic <- list(ben - cn * seen, ben + cn * seen)
  vcp <- rowSums(lisic[[1]] < vb & vb < lisic[[2]])/10000
  cbind(serb, mmedmad, vcp)
  }, X = ests_sim_p25)

options(scipen = 999)
round(matrix(unlist(des_see_p25), ncol = 16), 4)

# Resultados para el vector de percentiles 50
des_see_p50 <- lapply(FUN = function(rsim) {
  cn <- qnorm(p = 0.975) 
  vb <- as.vector(B_est_perc[[4]])
  mdn <- rsim[[1]]
  ben <- rsim[[2]][1:6, ]
  seen <- rsim[[2]][10:15, ]
  vrb <- est_varcov_coefs(mX = mdn, mSg = Sigma_est, vz = perc_norm_est[, 4])
  serb <- sqrt(diag(vrb))
  mmedmad <- t(apply(seen, 1, function(seeb) c(median(seeb), mad(seeb))))
  lisic <- list(ben - cn * seen, ben + cn * seen)
  vcp <- rowSums(lisic[[1]] < vb & vb < lisic[[2]])/10000
  cbind(serb, mmedmad, vcp)
}, X = ests_sim_p50)

options(scipen = 999)
round(matrix(unlist(des_see_p50), ncol = 16), 4)

# Resultados para el vector de percentiles 75
des_see_p75 <- lapply(FUN = function(rsim) {
  cn <- qnorm(p = 0.975) 
  vb <- as.vector(B_est_perc[[5]])
  mdn <- rsim[[1]]
  ben <- rsim[[2]][1:6, ]
  seen <- rsim[[2]][10:15, ]
  vrb <- est_varcov_coefs(mX = mdn, mSg = Sigma_est, vz = perc_norm_est[, 5])
  serb <- sqrt(diag(vrb))
  mmedmad <- t(apply(seen, 1, function(seeb) c(median(seeb), mad(seeb))))
  lisic <- list(ben - cn * seen, ben + cn * seen)
  vcp <- rowSums(lisic[[1]] < vb & vb < lisic[[2]])/10000
  cbind(serb, mmedmad, vcp)
}, X = ests_sim_p75)

options(scipen = 999)
round(matrix(unlist(des_see_p75), ncol = 16), 4)

#--- DESEMPENO DE LAS ESTIMACIONES DE LOS PERCENTILES ---

# Resultados para el vector de percentiles 25
per25_est_pe_hm <- mapply(function(bper, bgpe, bgp, bge, mX) {
  mrh <- colMeans(as.data.frame(mX) %>% filter(sse == 1))
  mrm <- colMeans(as.data.frame(mX) %>% filter(sse == 0))
  cprhqln <- exp(sum(bper[, 1] * mrh))
  cerhqln <- exp(sum(bper[, 2] * mrh))
  cprmqln <- exp(sum(bper[, 1] * mrm))
  cermqln <- exp(sum(bper[, 2] * mrm))
  cpehqln <- exp(colSums(bgpe[1:3, ] * mrh))
  ceehqln <- exp(colSums(bgpe[4:6, ] * mrh))
  cpemqln <- exp(colSums(bgpe[1:3, ] * mrm))
  ceemqln <- exp(colSums(bgpe[4:6, ] * mrm))
  cpehtra <- colSums(bgp * mrh)
  ceehtra <- colSums(bge * mrh)
  cpemtra <- colSums(bgp * mrm)
  ceemtra <- colSums(bge * mrm)
  mmcpeh <- c(cprhqln, median(cpehqln), mad(cpehqln), median(cpehtra), 
              mad(cpehtra))
  mmceeh <- c(cerhqln, median(ceehqln), mad(ceehqln), median(ceehtra), 
              mad(ceehtra))
  mmcpem <- c(cprmqln, median(cpemqln), mad(cpemqln), median(cpemtra), 
              mad(cpemtra))
  mmceem <- c(cermqln, median(ceemqln), mad(ceemqln), median(ceemtra), 
              mad(ceemtra))
  list(mmcpem, mmceem, mmcpeh, mmceeh)}, 
  bper = list(B_est_perc[[3]]),
  bgpe = lapply(ests_sim_p25, function(mebs) mebs[[2]][1:9, ]), 
  bgp = lapply(ests_sim_p25, function(mbep) mbep[[3]][, ]),
  bge = lapply(ests_sim_p25, function(mbee) mbee[[4]][, ]),
  mX = lapply(ests_sim_p25, function(mr) mr[[1]]), SIMPLIFY = FALSE)

options(scipen = 999)
per25_est_pe_hm1 <- do.call(Map, c(list(f = c), per25_est_pe_hm))
round(do.call(rbind, per25_est_pe_hm1), 4)

# Resultados para el vector de percentiles 50
per50_est_pe_hm <- mapply(function(bper, bgpe, bgp, bge, mX) {
  mrh <- colMeans(as.data.frame(mX) %>% filter(sse == 1))
  mrm <- colMeans(as.data.frame(mX) %>% filter(sse == 0))
  cprhqln <- exp(sum(bper[, 1] * mrh))
  cerhqln <- exp(sum(bper[, 2] * mrh))
  cprmqln <- exp(sum(bper[, 1] * mrm))
  cermqln <- exp(sum(bper[, 2] * mrm))
  cpehqln <- exp(colSums(bgpe[1:3, ] * mrh))
  ceehqln <- exp(colSums(bgpe[4:6, ] * mrh))
  cpemqln <- exp(colSums(bgpe[1:3, ] * mrm))
  ceemqln <- exp(colSums(bgpe[4:6, ] * mrm))
  cpehtra <- colSums(bgp * mrh)
  ceehtra <- colSums(bge * mrh)
  cpemtra <- colSums(bgp * mrm)
  ceemtra <- colSums(bge * mrm)
  mmcpeh <- c(cprhqln, median(cpehqln), mad(cpehqln), median(cpehtra), 
              mad(cpehtra))
  mmceeh <- c(cerhqln, median(ceehqln), mad(ceehqln), median(ceehtra), 
              mad(ceehtra))
  mmcpem <- c(cprmqln, median(cpemqln), mad(cpemqln), median(cpemtra), 
              mad(cpemtra))
  mmceem <- c(cermqln, median(ceemqln), mad(ceemqln), median(ceemtra), 
              mad(ceemtra))
  list(mmcpem, mmceem, mmcpeh, mmceeh)},
  bper = list(B_est_perc[[4]]),
  bgpe = lapply(ests_sim_p50, function(mebs) mebs[[2]][1:9, ]), 
  bgp = lapply(ests_sim_p50, function(mbep) mbep[[3]][, ]),
  bge = lapply(ests_sim_p50, function(mbee) mbee[[4]][, ]),
  mX = lapply(ests_sim_p50, function(mr) mr[[1]]), SIMPLIFY = FALSE)

options(scipen = 999)
per50_est_pe_hm1 <- do.call(Map, c(list(f = c), per50_est_pe_hm))
round(do.call(rbind, per50_est_pe_hm1), 4)

# Resultados para el vector de percentiles 75
per75_est_pe_hm <- mapply(function(bper, bgpe, bgp, bge, mX) {
  mrh <- colMeans(as.data.frame(mX) %>% filter(sse == 1))
  mrm <- colMeans(as.data.frame(mX) %>% filter(sse == 0))
  cprhqln <- exp(sum(bper[, 1] * mrh))
  cerhqln <- exp(sum(bper[, 2] * mrh))
  cprmqln <- exp(sum(bper[, 1] * mrm))
  cermqln <- exp(sum(bper[, 2] * mrm))
  cpehqln <- exp(colSums(bgpe[1:3, ] * mrh))
  ceehqln <- exp(colSums(bgpe[4:6, ] * mrh))
  cpemqln <- exp(colSums(bgpe[1:3, ] * mrm))
  ceemqln <- exp(colSums(bgpe[4:6, ] * mrm))
  cpehtra <- colSums(bgp * mrh)
  ceehtra <- colSums(bge * mrh)
  cpemtra <- colSums(bgp * mrm)
  ceemtra <- colSums(bge * mrm)
  mmcpeh <- c(cprhqln, median(cpehqln), mad(cpehqln), median(cpehtra), 
              mad(cpehtra))
  mmceeh <- c(cerhqln, median(ceehqln), mad(ceehqln), median(ceehtra), 
              mad(ceehtra))
  mmcpem <- c(cprmqln, median(cpemqln), mad(cpemqln), median(cpemtra), 
              mad(cpemtra))
  mmceem <- c(cermqln, median(ceemqln), mad(ceemqln), median(ceemtra), 
              mad(ceemtra))
  list(mmcpem, mmceem, mmcpeh, mmceeh)}, 
  bper = list(B_est_perc[[5]]),
  bgpe = lapply(ests_sim_p75, function(mebs) mebs[[2]][1:9, ]), 
  bgp = lapply(ests_sim_p75, function(mbep) mbep[[3]][, ]),
  bge = lapply(ests_sim_p75, function(mbee) mbee[[4]][, ]),
  mX = lapply(ests_sim_p75, function(mr) mr[[1]]), SIMPLIFY = FALSE)

options(scipen = 999)
per75_est_pe_hm1 <- do.call(Map, c(list(f = c), per75_est_pe_hm))
round(do.call(rbind, per75_est_pe_hm1), 4)






