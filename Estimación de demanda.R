library(tidyverse)
library(readxl)
library(stargazer)
library(AER)
library(lfe)
library(optimx)
library(data.table)
library(reshape2)
library(antitrust)

datos <- read_xlsx('DATA_UDESA.xlsx', sheet = 1)
demograficas <- read_xlsx('DATA_UDESA.xlsx', sheet = 2)

datos$tienda <- as.factor(datos$tienda)
datos$marca <- as.factor(datos$marca)
demograficas$tienda <- as.factor(demograficas$tienda)

# Tabla consignas ---------------------------------------------------------
medicamentos <- data.frame(
  marca = c("Marca 1", "Marca 1", "Marca 1", "Marca 2", "Marca 2", "Marca 2", 
            "Marca 3", "Marca 3", "Marca 3", "Marca 4", "Marca 4"),
  tamano = c(25, 50, 100, 25, 50, 100, 25, 50, 100, 50, 100),
  share = c(8.90, 11.10, 7.60, 9.30, 5.10, 2.20, 2.50, 1.00, 4.90, 7.20, 4.20),
  precio = c(3.52, 4.99, 7.14, 3.02, 5.19, 8.23, 2.66, 3.81, 4.05, 3.57, 3.29),
  precio_50_tab = c(6.91, 4.99, 3.57, 6.29, 5.19, 4.12, 5.35, 3.81, 4.05, 3.57, 3.29),
  precio_mayorista_unitario = c(2.26, 3.76, 5.93, 2.02, 3.70, 6.25, 1.90, 2.67, 3.73, 2.15, 1.51)
)

# Estimo deltas -----------------------------------------------------------
# Necesito el share del bien externo
share_externo <- 1 - sum(medicamentos$share)/100

ventas_semana <- datos %>% 
  group_by(semana, tienda) %>% 
  summarise(total_ventas = sum(ventas)/(1-share_externo))

# El bien externo tiene el 33,5% del mercado, hago regla de 3 simple para despejar el total
# ventas ----- 1 - 0.335
# Total ------ 1
# El total incluyendo el bien externo es (Ventas)/(1-0.335)
# Estoy haciendo el supuesto implícito de que semana a semana y tienda a tienda el share del externo se mantiene constante

datos <- datos %>% 
  left_join(ventas_semana, by = c('semana', 'tienda')) %>% 
  mutate(share = ventas/total_ventas) %>% 
  mutate(delta = log(share) - log(share_externo))

rm(ventas_semana, medicamentos)
# MCO ---------------------------------------------------------------------
#Precio y promocion
pp <- lm(delta ~ precio + descuento, datos)
summary(pp)

ppm <- lm(delta ~ precio + descuento + marca, datos)
summary(ppm)

# ppi <- lm(delta ~ precio + descuento + I(tienda:marca), datos) # Solo para que despues funcione una función
ppi <- felm(delta ~ precio + descuento | tienda:marca, datos)
summary(ppi)

# VI ----------------------------------------------------------------------
# Instrumentando por costos
pp_c <- ivreg(delta ~ precio + descuento | costo + descuento, data = datos)
summary(pp_c)

ppm_c <- ivreg(delta ~ precio + descuento + marca | costo + descuento + marca, data = datos)
summary(ppm_c)

ppi_c <- felm(delta ~ descuento| tienda:marca |(precio ~ costo), data = datos)
summary(ppi_c)

# Instrumentando por competencia ------------------------------------------
# Necesito calcular el precio promedio del resto de las otras marcas en el resto de las tiendas
promedio_e <- function(data) {
  data %>%
    group_by(semana) %>%
    mutate(hausman = sapply(1:n(), function(i) {
      mean(data$precio[data$semana == semana[i] & data$marca == marca[i] & data$tienda != tienda[i]], na.rm = TRUE)
    })) %>%
    ungroup()
}
datos <- promedio_e(datos)

pp_h <- ivreg(delta ~ precio + descuento | hausman + descuento, data = datos)
summary(pp_h)

ppm_h <- ivreg(delta ~ precio + descuento + marca | hausman + descuento + marca, data = datos)
summary(ppm_h)

ppi_h <- felm(delta ~ descuento| tienda:marca |(precio ~ hausman), data = datos)
summary(ppi_h)

stargazer(pp, pp_c, pp_h, type = "latex",
          title = "Modelo Precios y Promoción",
          column.labels = c('MCO', 'Cost-shifter', 'Hausman'))

stargazer(ppm, ppm_c, ppm_h, type = "latex",
          title = "Modelo Precios, Promoción y EF",
          column.labels = c('MCO', 'Cost-shifter', 'Hausman'))

stargazer(ppi, ppi_c, ppi_h, type = "latex",
          title = "Modelo Precios, Promoción e interacción marca-tienda",
          column.labels = c('MCO', 'Cost-shifter', 'Hausman'), 
          keep = c('precio', 'descuento'))

# Elasticidades -----------------------------------------------------------
elasticidades <- function(modelo, data) {
  delta_pred <- modelo$fitted.values
  share_pred <- exp(delta_pred)/sum(exp(delta_pred))
  alpha <- modelo$coefficients[["precio"]]
  df <- data %>% 
    mutate(delta = delta_pred,
           d = share_pred,
           elasticidad = (1-d)*alpha*precio) %>% 
    group_by(tienda, marca) %>% 
    mutate(mean_elasticidad = mean(elasticidad)) %>% 
    ungroup() %>% 
    dplyr::select(tienda, marca, mean_elasticidad)
  
  return(df)
}

modelos <- list(pp, ppm, ppi)
epsilon <- lapply(modelos, elasticidades, data = datos)
names(epsilon) <- c('pp', 'ppm', 'ppi')

combined_df <- bind_rows(epsilon, .id = "model")

ggplot(combined_df, aes(x = tienda, y = marca, fill = mean_elasticidad)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(x = "Tienda",
       y = "Marca",
       fill = "Elasticidad Promedio") +
  scale_x_discrete(breaks = seq(2, 123, by = 7)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ model, labeller = as_labeller(c(pp = 'Precios y Descuento', ppm = 'Precios, Descuento y Marca', ppi = 'Precios, Descuento e Interacción')), ncol = 3)

combined_df <- combined_df %>% 
  mutate(tienda = factor(tienda, levels = unique(tienda)))

ggplot(combined_df, aes(x = tienda, y = mean_elasticidad, group = marca, color = marca)) +
  geom_line(aes(color = marca)) +
  theme_minimal() +
  labs(x = "Tienda",
       y = "Elasticidad Promedio",
       color = "Marca") +
  scale_x_discrete(breaks = seq(2, 123, by = 7)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ model, labeller = as_labeller(c(pp = 'Precios y Descuento', ppm = 'Precios, Descuento y Marca', ppi = 'Precios, Descuento e Interacción')), ncol = 3, scales = 'free_y')

rm(modelos, epsilon)
# BLP ---------------------------------------------------------------------
# Agrego a una dummy que especifica si es de marca o genérica
datos <- datos %>% 
  mutate(de_marca = if_else(marca == 10, 0, if_else(marca == 11, 0, 1)))

# Primero genero las distribuciones del ingreso
distribuciones_ingreso <- function(data, n){
  sd_ingreso <- sd(data$ingreso) -.10
  dist_ingreso <- list()
  for (i in unique(data$tienda)) {
    ingreso <- data$ingreso[data$tienda == i]
    distrib <- rnorm(n, ingreso, sd_ingreso)
    dist_ingreso[[as.character(i)]] <- distrib
  } # Genero una lista de 74 distribuciones del ingreso
  d_ingreso <- sample(unique(unlist(dist_ingreso)), size = n) # Agarro una muestra aleatoria de esas distribuciones
}

set.seed(1234)
dist_ingreso <- distribuciones_ingreso(demograficas, 15)
dist_gustos <- rnorm(15)

# Armo un df con el producto cartesiano de las distribuciones
inputs <- expand.grid(v = dist_gustos, i = dist_ingreso) # Genera combinaciones aleatorias del par gustos-ingreso

# Función para simular la integral doble y poder estimar los deltas que minimizan la diferencia entre el share predicho y el share observado
estimo_shares <- function(data, inputs, delta, sigma_B, sigma_I) {
  data <- as.data.table(data)
  inputs <- inputs %>% 
    as.matrix() # Armo la matriz de inputs con todas las combinaciones de ingresos-gustos
  mat <- data[, .(de_marca = sigma_B * de_marca, precio = sigma_I * precio)] %>% 
    as.matrix() %>% # Armo una matriz con sigma_B*B_jt y sigma_I*precio
    t()
  gustos_ingreso <- inputs %*% mat %>% # dim(inputs) = nx2, dim(mat) = 2xobservaciones
    t()
  delta_replicado <- matrix(rep(delta, times=ncol(gustos_ingreso)), nrow=nrow(gustos_ingreso), ncol=ncol(gustos_ingreso)) # Armo una matriz con las condiciones iniciales para delta
  numerador <- exp(gustos_ingreso + delta_replicado) # A sigma_B*B_jt *v_i+ sigma_I*precio*I_i le sumo delta inicial y a toda esa suma la exponencio
  numerador <- cbind(data[, .(tienda, semana)], numerador) # A esa matriz con e^(delta + sigma_B*B_jt*v_i+ sigma_I*precio*I_i) le agrego tienda y semana para poder agrupar por esas variables
  denominador <- numerador[, lapply(.SD, sum), by = .(tienda, semana)] # Calculo el denominador, primero sumando los numeradores por tienda y semana
  denominador <- denominador[,3:(ncol(denominador))] + 1 # A esa sumatoria le sumo 1
  denominador <- denominador[rep(1:.N, each = n_distinct(data$marca))] # La transformo para que tenga la misma dimensión que el numerador
  ratio <- numerador[,3:ncol(numerador)]/denominador # Tomo el ratio del numerador y el denominador
  promedio <- rowMeans(ratio) # Calculo el promedio
  
  return(promedio)
}

# Fijo un theta arbitrario
sB <- 0.1
sI <- 0.1
delta_init <- rep(0, nrow(datos))

# El loop estima los shares predichos, y va actualizando los deltas hasta que el share predicho y el observado tienen una diferencia muy chica
deltas <- function(data, inputs, deltas, sigma_B, sigma_I){
  for(m in 1:10000) {
    shares_pred <- estimo_shares(data, inputs, deltas, sigma_B, sigma_I) # Predice shares
    if (m == 1) {
      cat("Participaciones de mercado observadas:", head(data$share), "\n")
      cat("Participaciones de mercado predichas (iteración 1):", head(shares_pred), "\n") # Solo printea las condiciones iniciales
    }
    new_delta <- deltas + log(data$share) - log(shares_pred) # Calcula los nuevos deltas usando la formula de Berry
    if (max(abs(new_delta - deltas)) < 1e-4) {
      break # Frena el loop si la diferencia entre lo predicho y lo observado es menor al límite
    }
    cat("Iteración:", m, "Diferencia máxima:", max(abs(new_delta - deltas)), "\n") # Printea las diferencias maximas entre predicho y observado
    deltas <- new_delta # Actualiza los deltas para volver a estimar los shares y repetir el loop
  
  }
  return(deltas)
}  

delta <- deltas(datos, inputs, delta_init, sB, sI)

# GMM ---------------------------------------------------------------------
# Planteo la matriz de características
X <- datos %>%
  select(precio, descuento) %>% 
  as.matrix()

# Diego me paso sus instrumentos
Z <- read.csv('z_base.csv') %>%  
  as.matrix()

# Armo las dummies por marca para estimar efectos fijos
dummies <- datos %>% 
  mutate(marca_1 = if_else(marca %in% c(1,2,3), 1, 0)) %>% 
  mutate(marca_2 = if_else(marca %in% c(4,5,6), 1, 0)) %>% 
  mutate(marca_3 = if_else(marca %in% c(7,8,9), 1, 0)) %>% 
  select(marca_1, marca_2, marca_3) %>% 
  as.matrix()

# Agrego las dummies a X y Z
X <- cbind(X, dummies)
Z <- cbind(Z, dummies)  

# Calculo la matriz de ponderadores optima
W <- solve(t(Z) %*% Z)

# Propongo parametros iniciales
beta_init <- rep(0, ncol(X) - 1)  
alpha_init <- 0  

# Con esto calculo el xi para meter en la funcion objetivo y poder minimizar
formula_xi <- function(deltas, X, beta, alpha, data) {
  xi <- deltas - X[, 2:ncol(X)] %*% beta - alpha * X[, "precio"]
  return(xi)
}

# Esta esla funcion a minimizar
gmm_objective <- function(params, deltas, X, Z, W, inputs, data) {
  beta <- params[2:ncol(X)]
  alpha <- params[1]
  sB <- params[ncol(X) + 1]
  sI <- params[ncol(X) + 2]
  
  delta <<- deltas(data, inputs, deltas, sB, sI) 
  
  xi <- formula_xi(delta, X, beta, alpha, data)
  obj <- t(xi) %*% Z %*% W %*% t(Z) %*% xi
  
  cat("Parámetros actuales:\n")
  cat("Alpha:", alpha, "\n")
  cat("Beta:", beta, "\n")
  cat("Sigma_B:", sB, "\n")
  cat("Sigma_I:", sI, "\n")
  cat("Función objetivo:", as.numeric(obj), "\n\n")
  
  return(as.numeric(obj))  
}

params_init <- c(alpha_init, beta_init,  sB, sI) # Condiciones iniciales de total ignorancia
# params_init <- c(-1, beta_init, sB, 1) # Si queremos que los resultados nos den mejor

# Minimizacion sin restricciones
result <- optimx(par = params_init, fn = gmm_objective, method = "Nelder-Mead", deltas = delta, X = X, Z = Z, W = W, inputs = inputs, data = datos)

# Si queremos restringir los valores que pueden tomar los parametros
lower_bound <- c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0)
upper_bound <- c(0, Inf, Inf, Inf, Inf, Inf, Inf)

# Minimizacion con restricciones sobre los parametros
result_restringido <- optimx(par = params_init, fn = gmm_objective, method = "L-BFGS-B", lower = lower_bound, upper = upper_bound, deltas = delta, X = X, Z = Z, W = W, inputs = inputs, data = datos)

# Separando parametros lineales de no lineales ----------------------------
# Primero meto los intrumentos en la base
instrumentos <- as.data.frame(Z) %>% 
  select(-c('costo', 'descuento'))

datos <- cbind(datos, instrumentos)

nl_gmm_objective <- function(params, deltas, X, Z, W, inputs, data, beta, alpha) {
  sB <- params[1]
  sI <- params[2]
  
  delta <<- deltas(data, inputs, deltas, sB, sI) 
  
  xi <- formula_xi(delta, X, beta, alpha, data)
  obj <- t(xi) %*% Z %*% W %*% t(Z) %*% xi
  
  cat("Parámetros actuales:\n")
  cat("Sigma_B:", sB, "\n")
  cat("Sigma_I:", sI, "\n")
  cat("Función objetivo:", as.numeric(obj), "\n\n")
  
  return(as.numeric(obj))  
}

nl_params_init <- c(sB, sI) # Planteo condiciones iniciales para los parámetros no lineales

nl_result <- optimx(par = nl_params_init, fn = nl_gmm_objective, method = "Nelder-Mead", deltas = delta, X = X, Z = Z, W = W, inputs = inputs, data = datos, beta = beta_init, alpha = alpha_init)
nl_params <- coef(nl_result) # Guardo los sigmas estimados

# Iterar a partir de aca
delta <- deltas(datos, inputs, delta, nl_params[1], nl_params[2]) # Actualizo los deltas

datos <- datos %>% 
  mutate(delta_blp = delta) # Meto los deltas en los datos

nombres_instrumentos <- names(as.data.frame(Z)) # Para hacer la regresion mas facil
modelo <- as.formula(paste("delta ~ precio + descuento + marca_1 + marca_2 + marca_3 |", paste(nombres_instrumentos, collapse = "+")))

iv <- ivreg(modelo, data = datos) # Regreso los deltas contra los precios y los X, usando iv
l_params <- coef(iv) # Guardo los parametros lineales estimados
alpha_hat <- l_params[2]
beta_hat <- l_params[3:6]
# Vuelvo a estimar los sigmas
nl_result <- optimx(par = nl_params_init, fn = nl_gmm_objective, method = "Nelder-Mead", deltas = delta, X = X, Z = Z, W = W, inputs = inputs, data = datos, beta = beta_hat, alpha = alpha_hat)
nl_params <- coef(nl_result)

# Resultados de separar los parámetros lineales de los no lineales
resultado_lnl <- c(alpha_hat, beta_hat, nl_params[1], nl_params[2])

# Gráficos resultados -----------------------------------------------------
# Corri varias veces la minimizacion con distintos metodos
resultados <- read.csv('resultados_nlminb_lbfgsb.csv')
resultados_neldermead <- read.csv('resultados_neldermead.csv')

# Normalizo la escala de colores para los graficos
min_objetivo <- min(resultados$objetivo, resultados_neldermead$objetivo, na.rm = TRUE)
max_objetivo <- max(resultados$objetivo, resultados_neldermead$objetivo, na.rm = TRUE)

resultados %>%
  filter(metodo == 'nlminb') %>%
  ggplot(aes(x = parámetros, y = valores, fill = objetivo)) +
  geom_bar(position = position_dodge(width = 0.9), stat = 'identity') +
  geom_text(aes(label = round(valores, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(efectos_fijos ~ restringido, scales = 'free') +
  scale_fill_continuous(limits = c(min_objetivo, max_objetivo)) +
  scale_x_discrete(labels = function(x) parse(text = resultados$etiquetas[match(x, resultados$parámetros)])) +
  labs(x = NULL, y = NULL, fill = 'Función Objetivo', caption = 'Método: nlminb') +
  theme_minimal()

resultados %>%
  filter(metodo == 'L-BFGS-B') %>%
  ggplot(aes(x = parámetros, y = valores, fill = objetivo)) +
  geom_bar(position = position_dodge(width = 0.9), stat = 'identity') +
  geom_text(aes(label = round(valores, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(efectos_fijos ~ restringido, scales = 'free') +
  scale_fill_continuous(limits = c(min_objetivo, max_objetivo)) +
  scale_x_discrete(labels = function(x) parse(text = resultados$etiquetas[match(x, resultados$parámetros)])) +
  labs(x = NULL, y = NULL, fill = 'Función Objetivo', caption = 'Método: L-BFGS-B') +
  theme_minimal()

ggplot(resultados_neldermead, aes(x = parámetros, y = valores, fill = objetivo)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(valores, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(efectos_fijos ~ cambio, scales = 'free') +
  scale_fill_continuous(limits = c(min_objetivo, max_objetivo)) +
  scale_x_discrete(labels = function(x) parse(text = resultados$etiquetas[match(x, resultados$parámetros)])) +
  labs(x = NULL, y = NULL, fill = 'Función Objetivo', caption = 'Método: Nelder-Mead') +
  theme_minimal()

# Elasticidades -----------------------------------------------------------
# Este es el modelo final que elegimos despues de todas las variaciones
theta_hat <- resultados_neldermead %>% 
  select(-etiquetas) %>% 
  filter(efectos_fijos == 'Con Efectos Fijos' & cambio == 'Modificando parámetros iniciales') %>% 
  pivot_wider(names_from = parámetros, values_from = valores) %>% 
  select(-c('objetivo', 'cambio', 'metodo', 'efectos_fijos')) %>% 
  as.vector()

# Actualizo los deltas
delta_blp <- deltas(datos, inputs, delta, as.numeric(resultado_lnl[6]), as.numeric(resultado_lnl[7]))
#Estimo los shares
shares <- estimo_shares(datos, inputs, delta, as.numeric(resultado_lnl[6]), as.numeric(resultado_lnl[7]))

# Calculo las elasticidades propias
datos <- datos %>% 
  mutate(delta_blp = delta_blp, share_blp = shares) %>% 
  mutate(elasticidad_propia = as.numeric(resultado_lnl[1])*(1-share_blp)*precio)

# Elasticidad cruzada
datos_reduc <- datos %>% 
  filter(tienda == 9 & semana == 10) %>% 
  mutate(elasticidad_cruzada_resto = share_blp*precio)

elasticidades_cruzadas <- matrix(rep(datos_reduc$elasticidad_cruzada_resto, times=11), nrow=11, ncol=11)
diag(elasticidades_cruzadas) <- datos_reduc$elasticidad_propia

# Armo un df para poder graficar
elasticidades_cruzadas_long <- melt(elasticidades_cruzadas)
colnames(elasticidades_cruzadas_long) <- c("marca_i", "marca_j", "Elasticidad")

ggplot(elasticidades_cruzadas_long, aes(x = as.factor(marca_i), y = as.factor(marca_j), fill = Elasticidad)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = expression(Marca[i]),
       y = expression(Marca[j]),
       fill = "Elasticidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Costos ------------------------------------------------------------------
# Estimo los costos usando el indice de Lerner
datos <- datos %>% 
  mutate(costo_blp = precio/elasticidad_propia + precio)

# Grafico el promedio de la diferencia entre el costo estimado y el observado
datos %>% 
  group_by(tienda, marca) %>% 
  summarise(diferencia_promedio = mean(costo - costo_blp)) %>% 
  ggplot(mapping = aes(x = tienda, y = diferencia_promedio, color = marca)) +
  geom_point() +
  labs(y = 'Diferencia promedio entre costo observado y costo estimado', x = 'Tienda',color = 'Producto') +
  #ylim(-1,1) +
  scale_x_discrete(breaks = seq(2, 123, by = 7)) +
  theme_minimal()

# Fusión ------------------------------------------------------------------
datos_5 <- datos %>%
  filter(tienda == 9 & semana == 10) %>% 
  dplyr::mutate(margins = precio*ventas-costo*ventas)

# corriendo el modelo
price <-  datos_5$precio
margins <- datos_5$margins
prodNames <- c("1","2","3","4","5","6","7","8","9","10","11")
ownerPre <-c("1","1","1","2","2","2","3","3","3","4","4")
ownerPost <-c("1","1","1","1","1","1","1","1","1","4","4")

# parámetros estimados con el minstrumento de Hausman y con efectos fijos por marca
demand.param=list(alpha=-0.5,
                  meanval=c(0.219915,
                            0.93546,
                            1.50910,
                            -0.49737,
                            0.08629,
                            0.76296,
                            -1.75867,
                            -1.54805,
                            -0.43832,
                            -1.43111,
                            -0.61623))

simulacion <- antitrust::sim(
  prices = price,
  demand = "Logit",
  demand.param = demand.param,
  ownerPre=ownerPre,
  ownerPost=ownerPost, 
  insideSize = 85)

antitrust::elast(simulacion,TRUE)

# simulación sin merge

simulacion_sin_merge <- antitrust::sim(prices = price,
                                       demand = "Logit",
                                       demand.param = demand.param,
                                       ownerPre=ownerPre,
                                       ownerPost=ownerPre)

# gráfico de la simulación ------------------------------------------------
# Crear una lista vacía
my_list <- list()

# Obtener los nombres de todos los slots del objeto S4
slots <- slotNames(simulacion)

# Rellenar la lista con los valores de los slots
for (slot_name in slots) {
  my_list[[slot_name]] <- slot(simulacion, slot_name)
}

antitrust::calcShares(simulacion, preMerge = TRUE)
antitrust::calcShares(simulacion, preMerge = FALSE)
sum(antitrust::calcShares(simulacion, preMerge = TRUE))
sum(antitrust::calcShares(simulacion, preMerge = FALSE))

my_list$mktSize

q1 <- as.data.frame(antitrust::calcQuantities(simulacion, preMerger = TRUE)) %>% 
  rename("q1" ="antitrust::calcQuantities(simulacion, preMerger = TRUE)") %>% 
  tibble::rownames_to_column("Producto")

q2 <- as.data.frame(antitrust::calcQuantities(simulacion, preMerger = FALSE)) %>%  
  rename("q2" ="antitrust::calcQuantities(simulacion, preMerger = FALSE)") %>% 
  tibble::rownames_to_column("Producto")

q <- left_join(q1,q2,by="Producto") %>% 
  pivot_longer(cols=-Producto,
               values_to = "q",
               names_to = "variable") %>% 
  dplyr::mutate(variable = recode_factor(variable,
                                         "q1"="Pre_merging",
                                         "q2" = "Post_merging"))

precios <- as.data.frame(cbind(my_list$pricePre,my_list$pricePost)) %>% 
  rename("p1" = "V1",
         "p2" = "V2") %>% 
  tibble::rownames_to_column("Producto") %>% 
  pivot_longer(cols=-Producto,
               values_to = "p",
               names_to = "variable") %>% 
  dplyr::mutate(variable = recode_factor(variable,
                                         "p1"="Pre_merging",
                                         "p2" = "Post_merging"))

p_q <- left_join(q,precios,by=c("Producto","variable"))

p_q$Producto <- factor(p_q$Producto, 
                       levels = c("Prod1", "Prod2", "Prod3",
                                  "Prod4","Prod5","Prod6","Prod7",
                                  "Prod8","Prod9","Prod10","Prod11"))

paleta <- c("#9a031e","#003049")

ggplot(p_q %>% 
         dplyr::mutate(
           Producto = recode_factor(Producto,
                                    "Prod1" = "Producto 1","Prod2" = "Producto 2",
                                    "Prod3" = "Producto 3","Prod4" = "Producto 4",
                                    "Prod5" = "Producto 5","Prod6" = "Producto 6",
                                    "Prod7" = "Producto 7","Prod8" = "Producto 8",
                                    "Prod9" = "Producto 9","Prod10" = "Producto 10",
                                    "Prod11" = "Producto 11")))+
  geom_point(aes(x=q,y=p,color = variable), size = 2)+
  geom_vline(aes(xintercept = q,color = variable), linetype = 2)+
  geom_hline(aes(yintercept = p,color = variable), linetype = 2)+
  scale_x_continuous(breaks=seq(2,20,by=2))+
  scale_color_manual(values = paleta)+
  theme_minimal()+
  labs(x="Cantidad", y = "Precio", color = "")+
  facet_wrap(.~Producto)+
  theme(legend.position = c(.9, .15),
        legend.background = element_rect(color = "transparent"),
        text=element_text(size=14))

# gráfico de la simulación sin merge --------------------------------------
# Crear una lista vacía
my_list <- list()

# Obtener los nombres de todos los slots del objeto S4
slots <- slotNames(simulacion_sin_merge)

# Rellenar la lista con los valores de los slots
for (slot_name in slots) {
  my_list[[slot_name]] <- slot(simulacion_sin_merge, slot_name)
}

q1 <- as.data.frame(antitrust::calcQuantities(simulacion_sin_merge, preMerger = TRUE)) %>% 
  rename("q1" ="antitrust::calcQuantities(simulacion_sin_merge, preMerger = TRUE)") %>% 
  tibble::rownames_to_column("Producto")

q2 <- as.data.frame(antitrust::calcQuantities(simulacion_sin_merge, preMerger = FALSE)) %>%  
  rename("q2" ="antitrust::calcQuantities(simulacion_sin_merge, preMerger = FALSE)") %>% 
  tibble::rownames_to_column("Producto")

q <- left_join(q1,q2,by="Producto") %>% 
  pivot_longer(cols=-Producto,
               values_to = "q",
               names_to = "variable") %>% 
  dplyr::mutate(variable = recode_factor(variable,
                                         "q1"="Pre_merging",
                                         "q2" = "Post_merging"))

precios <- as.data.frame(cbind(my_list$pricePre,my_list$pricePost)) %>% 
  rename("p1" = "V1",
         "p2" = "V2") %>% 
  tibble::rownames_to_column("Producto") %>% 
  pivot_longer(cols=-Producto,
               values_to = "p",
               names_to = "variable") %>% 
  dplyr::mutate(variable = recode_factor(variable,
                                         "p1"="Pre_merging",
                                         "p2" = "Post_merging"))

p_q <- left_join(q,precios,by=c("Producto","variable"))

p_q$Producto <- factor(p_q$Producto, 
                       levels = c("Prod1", "Prod2", "Prod3",
                                  "Prod4","Prod5","Prod6","Prod7",
                                  "Prod8","Prod9","Prod10","Prod11"))

paleta <- c("#9a031e","#003049")

ggplot(p_q %>% 
         dplyr::mutate(
           Producto = recode_factor(Producto,
                                    "Prod1" = "Producto 1","Prod2" = "Producto 2",
                                    "Prod3" = "Producto 3","Prod4" = "Producto 4",
                                    "Prod5" = "Producto 5","Prod6" = "Producto 6",
                                    "Prod7" = "Producto 7","Prod8" = "Producto 8",
                                    "Prod9" = "Producto 9","Prod10" = "Producto 10",
                                    "Prod11" = "Producto 11"),
           variable = recode_factor(variable,
                                    "Pre_merging" = "Pre-fusión",
                                    "Post_merging" = "Post-fusión")))+
  geom_point(aes(x=variable,y=p), color = "#9a031e",size = 2)+
  geom_line(aes(x=variable,y=p, group = Producto), color = "#9a031e",linetype = 2)+
  # scale_x_continuous(breaks=seq(2,20,by=2))+
  # scale_color_manual(values = paleta)+
  theme_minimal()+
  labs(x="", y = "Precio predicho", color = "")+
  facet_wrap(.~Producto)+
  theme(legend.position = c(.9, .15),
        legend.background = element_rect(color = "transparent"),
        text=element_text(size=14))
