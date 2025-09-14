#----------------------------- FUNCIONES CREADAS ------------------------------

# Funcion de la densidad de la distribución log-normal p-variada basada en 
# cuantiles (QLNp)
dqlnp <- function(my, vq, mS, vz){
  # my: Matriz dada por el usuario con columnas igual a las respuestas
  # vq: Vector columna con los cuantiles de QLNp
  # mS: Matriz de dispersion de QLNp
  # vz: Vector con los cuantiles de una normal estandar
  # Cantidad de datos
  n <- nrow(my)
  # Numero de respuestas
  p <- ncol(mS)
  # Matriz identidad de orden p
  I <- diag(nrow = p)
  # Raiz cuadrada del producto Hadamard entre mS e I
  HSI <- (mS * I)^(1/2)
  # Vector de medias de la distribución normal multivariada (Np)
  vecmed <- log(vq) - HSI %*% vz
  # Matriz cuyas filas son vecmed 
  matmed <- matrix(vecmed, ncol = p, nrow = n, byrow = TRUE)
  # Densidad de la distribución Np con vector de medias igual a las columnas de
  # y matriz de varianzas y covarianzas mS
  dnm <- dmnorm(x = log(my), mean = matmed, varcov = mS)
  # Producto para cada uno de los vectores con los datos multivariados  
  prodvy <- apply(my, 1, prod)
  # Densidad de la distribucion QLNp
  dens <- dnm * prodvy
  print(dens)
}

# Funcion para encontrar el MLE de la matriz de dispersion para modelar cualquier 
# vector de cuantiles 
mle_Sigma <- function(mlogy, mX){
  # mlogy: Matriz dada por el usuario con columnas igual al logaritmo de las 
  # respuestas
  # mX: Matriz de diseno del modelo dada por el usuario
  n <- nrow(mX) # Tamano muestral
  py <- t(mlogy) %*% mlogy # log(Y)' log(Y)
  ipx <- solve(t(mX) %*% mX) # (X' X)^(-1)
  yx <- t(mlogy) %*% mX # log(Y)' X
  # (1/n) (log(Y)' log(Y) - log(Y)' X (X' X)^(-1) X' log(Y))
  Sg_gorro <- (1/n) * (py - yx %*% ipx %*% t(yx))
  # Retorna el MLE de la matriz de dispersion (Sigma gorro)
  return(Sg_gorro) 
}

# Funcion para encontrar el MLE de la matriz con los coeficientes de regresion 
# teniendo en cuenta el vector de cuantiles que se desea modelar 
mle_B <- function(mlogy, mX, mSg, vz){
  # mlogy: Matriz dada por el usuarion con columnas igual al logaritmo de las
  # respuestas
  # mX: Matriz de diseno del modelo dada por el usuario
  # mSg: MLE de la matriz de dispersion, la cual se encuentra con la funcion 
  # mle_Sigma
  # vz: Vector columna dada por el usuario con los cuantiles de una distribucion 
  # normal estandar
  n <- nrow(mX) # Tamano de muestra
  r <- ncol(mX) # Total de regresoras + intercepto
  p <- ncol(mSg) # Total de respuestas consideradas
  # Vector columna con 1 en la primer entrada y 0 en las restantes 
  ve <- as.matrix(replace(numeric(r), 1, 1)) 
  I <- diag(nrow = p) # Matriz identidad
  HSgI <- (mSg * I)^(1/2) # (Sigma gorro hadamard I)^(1/2)
  ipx <- solve(t(mX) %*% mX) # (X' X)^(-1)
  ezH <- ve %*% t(vz) %*% HSgI # e z' (Sigma gorro hadamard I)^(1/2)
  xy <- t(mX) %*% mlogy # X' log(Y)
  # Expresion para calcular B gorro (MLE de B)
  B_gorro <- ipx %*% xy + ezH
  # Retorna el MLE de la matriz con los coeficientes de regresion (B gorro)
  return(B_gorro) 
}

# Funcion para calcular las distancias de Mahalanobis empiricas una vez se modela
# un vector de cuantiles particular
maha_emp <- function(mlogy, mX, mBg, mSg, vz){
  # mlogy: Matriz dada por el usuario con columnas igual al logaritmo de las 
  # respuestas 
  # mX: Matriz de diseno del modelo dada por el usuario
  # mBg: MLE de la matriz con los coeficientes de regresion la cual se encuentra
  # con la funcion mle_B
  # mSg: MLE de la matriz de dispersion la cual se encuentra con la funcion 
  # mle_Sigma
  # vz: Vector columna dada por el usuario con los cuantiles de una distribucion 
  # normal estandar
  n <- nrow(mX) # Tamano de muestra
  p <- ncol(mSg) # Total de respuestas consideradas 
  I <- diag(nrow = p) # Matriz identidad
  vHz <- (mSg * I)^(1/2) %*% vz # (Sigma gorro hadamard I)^(1/2) z
  # Matriz con columnas iguales al vector vHz
  mHz <- matrix(vHz, nrow = p, ncol = n) 
  # Matriz con columnas iguales a log(y_i) - (B gorro)' x_i + 
  # (Sigma gorro hadamard I)^(1/2) z
  myBxHz <- t(mlogy) - t(mX %*% mBg) + mHz
  # Vector con la distancia de Mahalanobis empirica para cada observacion, es 
  # decir, (q_i)' (Sigma gorro)^(-1) q_i, donde q_i = log(y_i) - (B gorro)' x_i 
  # + (Sigma gorro hadamard I)^(1/2) z 
  dMe <- apply(myBxHz, 2, function(yBxHz, Sg) t(yBxHz) %*% solve(Sg) %*% yBxHz,
               Sg = mSg)
  # Distancias de Mahalanobis empiricas organizadas de menor a mayor
  dMe <- sort(dMe)
  # Retorna el vector con las distancias de Mahalanobis empiricas ordenadas
  return(dMe)
}

# Funcion para calcular las bandas de Atkinson, 1981
band_Atkin <- function(n, p){
  # n: Numero de observaciones (del conjunto de datos con el que se esta 
  # trabajando)
  # p: Total de respuestas consideradas
  # Generacion de n * 45000 observaciones de una normal multivariada con vector 
  # de medias igual al nulo y matriz de varianzas y covarianzas igual a la 
  # identidad
  onm <- rmnorm(n = n * 40000, mean = rep(0, p), varcov = diag(nrow = p))
  # Las n * 45000 observaciones simuladas elevadas al cuadrado
  conm <- onm^2
  # Suma por columna de la matriz conm
  sconm <- rowSums(conm)
  # Division de los valores (distancias de Mahalanobis) de sconm en 40000 
  # conjuntos de tamano n
  dMc <- matrix(sconm, nrow = n, ncol = 40000)
  # Distancias de Mahalanobis ordenadas de menor a mayor dentro de cada uno de 
  # los 45000 conjuntos
  dMc <- apply(dMc, 2, sort)
  # Seleccion del minimo y maximo por cada fila de la matriz dMc, respectivamente
  mindM <- apply(dMc, 1, min)
  maxdM <- apply(dMc, 1, max)
  # Matriz con los minimos (primera columna) y los maximos (segunda columna)
  mmdM <- cbind(mindM, maxdM)
  # Retorna la matriz mmdM
  return(mmdM)
} 

# Funcion para estimar las varianzas y covarianzas asintoticas de los coeficientes 
# de regresion estimados para un vector de cuantiles a partir de la estimacion de  
# la matriz de informacion de Fisher inversa 
est_varcov_coefs <- function(mX, mSg, vz){
  # mX: Matriz de diseno del modelo dada por el usuario 
  # mSg: MLE de la matriz de dispersion la cual se encuentra con la funcion
  # mle_Sigma
  # vz: Vector columna dada por el usuario con los cuantiles de una distribucion
  # normal estandar
  n <- nrow(mX) # Tamano de muestra 
  p <- ncol(mSg) # Total de respuestas consideradas 
  I <- diag(nrow = p) # Matriz identidad (I_p)
  vu <- matrix(1, nrow = nrow(mX)) # Vector columna de unos
  ipx <- solve(t(mX) %*% mX) # (X' X)^(-1)
  iSg <- solve(mSg) # Inversa de la matriz de dispersion estimada (Sigma gorro)
  iHSgI <- diag(1/diag(mSg)^(1/2)) # (Sigma gorro hadamard I_p)^(-1/2)
  sx <-  t(mX) %*% vu # X' 1
  mdvI <- diag(as.vector(I)) # diag(vec(I_p))
  Dp <- duplication.matrix(n = p) # Matriz de duplicacion 
  # ((Sigma gorro) kronecker (X' X)^(-1) (I_11 gorro))^(-1)
  iI11g <- kronecker(mSg, ipx)
  # - ((Sigma gorro)^(-1) (Sigma gorro hadamard I_p)^(-1/2) kronecker
  # X' 1 z') diag(vec(I_p)) D_p (I_12 gorro)
  I12g <- - kronecker(iSg %*% iHSgI, sx %*% t(vz)) %*% mdvI %*% Dp
  # - (D_p)' diag(vec(I_p)) ((Sigma gorro hadamard I_p)^(-1/2) (Sigma gorro)^(-1)
  # kronecker z 1' X) (I_21 gorro = (I_12 gorro)')
  I21g <- t(I12g)
  # diag(vec(I_p)) ((Sigma gorro hadamard I_p)^(-1/2) (Sigma gorro)^(-1)
  # (Sigma gorro hadamard I_p)^(-1/2) kronecker z z') diag(vec(I_p))
  pI22g <- mdvI %*% kronecker(iHSgI %*% iSg %*% iHSgI, vz %*% t(vz)) %*% mdvI
  # (1/2) (D_p)' (((Sigma gorro)^(-1) kronecker (Sigma gorro)^(-1)) + (1/2)
  # diag(vec(I_p)) ((Sigma gorro hadamard I_p)^(-1/2) (Sigma gorro)^(-1)
  # (Sigma gorro hadamard I_p)^(-1/2) kronecker z z') diag(vec(I_p))) D_p (I_22
  # gorro)
  I22g <- (n/2) * t(Dp) %*% (kronecker(iSg, iSg) + (1/2) * pI22g) %*% Dp
  # I_22 gorro - (I_21 gorro) (I_11 gorro)^(-1) (I_12 gorro) (I_22.1 gorro)
  I22.1g <- I22g - I21g %*% iI11g %*% I12g
  # (I_22.1 gorro)^(-1)
  iI22.1g <- solve(I22.1g)
  # (I_11 gorro)^(-1) + (I_11 gorro)^(-1) (I_12 gorro) (I_22.1 gorro)^(-1)
  # (I_21 gorro) (I_11 gorro)^(-1)
  varcov_gorro <- iI11g + iI11g %*% I12g %*% iI22.1g %*% I21g %*% iI11g
  # Retorna de la matriz de informacion de Fisher el bloque correspondiente a la
  # estimacion de las varianzas y covarianzas asintoticas de los coeficientes de
  # regresion estimados   
  return(varcov_gorro)
}




