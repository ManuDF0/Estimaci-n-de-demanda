library(data.table)
library(tidyverse)
library(microbenchmark)

# Función para simular la integral doble y poder estimar los deltas que minimizan la diferencia entre el share predicho y el share observado
# Con dplyr (es mas lento)
estimo_shares_dplyr <- function(data, inputs, delta_v, sigma_B, sigma_I){
  data <- data %>%
    mutate(delta = delta_v) # fijo la columna delta en el valor que yo le de
  suma_shares <- 0
  for (i in 1:nrow(inputs)) {
    data <- data %>%
      mutate(numerador = exp(delta_v + sigma_B * de_marca * inputs[i, 1] + sigma_I * precio * inputs[i, 2])) %>% # Calculo el numerador para el individuo i en la semana t, en la tienda k para la marca j
      group_by(semana, tienda) %>% # Junto las variables por semana y tienda para poder calcular la suma del denominador a lo largo de todas las marcas
      mutate(denominador = 1 + sum(numerador)) %>% # Para la tienda k en el momento t sumo los numeradores
      mutate(ratio = numerador / denominador) %>% # Calculo el share predicho para el individuo i en el momento t en la tienda k del producto j
      ungroup()
    share_jitk <- data %>%
      pull(ratio) # Agarro el share predicho para la persona i de la marca j en la tienda k en el momento t
    suma_shares <- suma_shares + share_jitk # Voy sumando para todos los individuos
  }
   share_promedio_predicho <- suma_shares/nrow(inputs) # Lo promedio
  return(share_promedio_predicho)
}

estimo_shares_table <- function(data, inputs, delta_v, sigma_B, sigma_I) {
  data <- as.data.table(data)
  sum_shares <- 0
  
  for (i in 1:nrow(inputs)) {
    v_i <- inputs[i,1]
    i_i <- inputs[i,2]
    
    data[, numerador := exp(delta_v + sigma_B * de_marca * v_i + sigma_I * precio * i_i)]
    data[, denominador := sum(numerador) + 1, by = .(semana, tienda)]
    data[, ratio := numerador / denominador]
    
    share_jitk <- data[, ratio]
    sum_shares <- sum_shares + share_jitk
    sum_shares_promedio <- sum_shares / nrow(inputs)
  }
  return(sum_shares_promedio)
}

estimo_shares_error <- function(data, inputs, delta_v, sigma_B, sigma_I) {
  data <- as.data.table(data)
  inputs <- inputs %>%
    as.matrix()
  mat <- data %>%
    select(de_marca , precio) %>%
    mutate(de_marca = sigma_B*de_marca, precio = sigma_I*precio) %>%
    as.matrix() %>%
    t()
  gustos_ingreso <- inputs %*% mat %>%
    t()
  delta_v_replicado <- matrix(rep(delta_v, times=ncol(gustos_ingreso)), nrow=nrow(gustos_ingreso), ncol=ncol(gustos_ingreso))
  exponente <- exp(gustos_ingreso + delta_v_replicado)
  data[, numerador := rowSums(exponente)]
  data[, denominador := sum(numerador) + 1, by = .(semana, tienda)]
  data[, ratio := numerador / denominador]

  return(data$ratio)
}

estimo_shares_matrix <- function(data, inputs, delta_v, sigma_B, sigma_I) {
  data <- as.data.table(data)
  inputs <- inputs %>% 
    as.matrix()
  mat <- data[, .(de_marca = sigma_B * de_marca, precio = sigma_I * precio)] %>% 
    as.matrix() %>% 
    t()
  gustos_ingreso <- inputs %*% mat %>% 
    t()
  delta_v_replicado <- matrix(rep(delta_v, times=ncol(gustos_ingreso)), nrow=nrow(gustos_ingreso), ncol=ncol(gustos_ingreso))
  numerador <- exp(gustos_ingreso + delta_v_replicado)
  numerador <- cbind(data[, .(tienda, semana)], numerador)
  denominador <- numerador[, lapply(.SD, sum), by = .(tienda, semana)]
  denominador <- denominador[,3:(ncol(denominador))] + 1
  denominador <- denominador[rep(1:.N, each = n_distinct(datos$marca))]
  ratio <- numerador[,3:ncol(numerador)]/denominador
  promedio <- rowMeans(ratio)
  
  return(promedio)
}

microbenchmark(
  dplyr = estimo_shares_dplyr(datos, inputs, delta_v, sB, sI),
  matrix = estimo_shares_matrix(datos, inputs, delta_v, sB, sI),
  times = 10
)

microbenchmark(
  data.table = estimo_shares_table(datos, inputs, delta_v, sB, sI),
  dplyr = estimo_shares_dplyr(datos, inputs, delta_v, sB, sI),
  times = 10
)

microbenchmark(
  data.table = estimo_shares_table(datos, inputs, delta_v, sB, sI),
  matrix = estimo_shares_matrix(datos, inputs, delta_v, sB, sI),
  times = 10
)

microbenchmark(
  error = estimo_shares_error(datos, inputs, delta_v, sB, sI),
  matrix = estimo_shares_matrix(datos, inputs, delta_v, sB, sI),
  times = 10
)
