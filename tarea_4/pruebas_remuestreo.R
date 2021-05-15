# Pruebas de la Tarea 4 con remuestreo

# correr Rmd hasta sección del remuestreo (en la pregunta 6)

# undersampling

# Pregunta 6


# dividir set de entrenamiento en k folds
set_train_a$id <- seq(1,nrow(set_train_a))

k <- treatment_assign(data = set_train_a, 
                      share_control = 0.2,
                      strata_varlist = "mailing_indicator",
                      n_t = 4,
                      missfits = "global",
                      seed = 1900, key = "id")$data

k <- k %>% mutate(k = treat + 1) %>%
  ungroup()


# set_training <- bind_cols(set_training, k %>% select(k))

k <- k$k


# Lasso con cross fitting (en set de entrenamiento)

# separo variable de interés y de tratamiento
y_train <- set_train_a %>%
  select(outcome_spend)

trat_train <- set_train_a %>% select(mailing_indicator)

set_train_a <- set_train_a %>% select(-c(outcome_spend,mailing_indicator, id))


# para correr modelos en paralelo
cores <- detectCores()
cl<-makeCluster(cores)
inicio<-Sys.time()

modelo <- NULL
modelo <- map_dfr(1:5, 
                  function(a) {
                    treat_fit <-gamlr(x = set_train_a[k!=a, , drop= F],
                                      y = trat_train[k !=a],
                                      family="binomial")
                    
                    
                    
                    treat_hat <- as.numeric(predict(
                      treat_fit,
                      newdata = set_train_a[k==a, , drop= F],
                      type = "response"))
                    
                    spend_fit <- gamlr(x = set_train_a[k!=a, , drop= F],
                                       y = y_train[k !=a])
                    
                    spend_hat<-as.numeric(predict(
                      spend_fit,
                      newdata = set_train_a[k==a, , drop= F],
                      type = "response"))
                    
                    treat_resid <- trat_train[k==a] - treat_hat
                    spend_resid <- y_train[k==a]- spend_hat
                    
                    
                    fits <- bind_cols("treat_hat" = treat_hat,
                                      "spend_hat"= spend_hat,
                                      "treat_resid" = treat_resid,
                                      "spend_resid" = spend_resid) %>%
                      as.data.frame()
                    
                    
                    return(fits)
                  }
)



# predicciones en set de validación

# separo variable de tratamiento
trat_validation <- set_validation %>% select(mailing_indicator)

set_validation <- set_validation %>% select(-c(mailing_indicator))


# funcion para hacer la estimacion de T y spend en validacion
predics <- NULL
predics <- map_dfc(1:5, 
                   function(a) {
                     
                     # LASSO tratamiento
                     treat_fit <-gamlr(x = set_train_a[k!=a, , drop= F],
                                       y = trat_train[k !=a],
                                       family="binomial")
                     
                     # estimacion de tratamiento
                     treat_hat <- as.numeric(predict(
                       treat_fit,
                       newdata = set_validation,
                       type = "response"))
                     
                     # LASSO spend
                     spend_fit <- gamlr(x = set_train_a[k!=a, , drop= F],
                                        y = y_train[k !=a])
                     
                     # estimación spend
                     spend_hat <- as.numeric(predict(
                       spend_fit,
                       newdata = set_validation,
                       type = "response"))
                     
                     # guardo los scores
                     fits <- bind_cols(treat_hat,
                                       spend_hat) %>%
                       as.data.frame()
                     
                   }
)

(tiempo<-Sys.time() - inicio)
stopCluster(cl)

# nombro columnas de predics

# ALGUIEN SABE CÓMO METER ESTA LÍNEA A LA FUNCIÓN?

names(predics) <- c("treat_hat_1","spend_hat_1",
                    "treat_hat_2","spend_hat_2",
                    "treat_hat_3","spend_hat_3",
                    "treat_hat_4","spend_hat_4",
                    "treat_hat_5","spend_hat_5")

# promedio de los scores
predics$spend_hat <- (predics$spend_hat_1+
                        predics$spend_hat_2+
                        predics$spend_hat_3+
                        predics$spend_hat_4+
                        predics$spend_hat_5)/5

predics$treat_hat <- (predics$treat_hat_1+
                        predics$treat_hat_2+
                        predics$treat_hat_3+
                        predics$treat_hat_4+
                        predics$treat_hat_5)/5







## Pregunta 7


# ATE en el set de entrenamiento

# primero agrego las variables spend y treat a la base con los residuales
modelo$spend <- y_train[[1]]
modelo$treat <- trat_train[[1]]
colnames(modelo)[3:4] <- c("treat_resid","spend_resid")

# regresion 1
spend_ate<-lm(spend_resid ~ treat_hat + treat,
              data = modelo) %>%
  tidy()



# regresion 2
spend_ate2 <-lm(spend ~ treat_resid,
                data = modelo) %>%
  tidy()


# ATE en el set de validacion
predics$spend <- y_validation[[1]]
predics$treat <- trat_validation[[1]] 

# residuales
predics$treat_resid <- predics$spend - predics$spend_hat
predics$spend_resid <- predics$treat - predics$treat_hat


# regresion 1
spend_atev <- lm(spend_resid ~ treat_hat + treat,
                 data = predics) %>%
  tidy()



# regresion 2
spend_ate2v <-lm(spend ~ treat_resid,
                 data = predics) %>%
  tidy()


# train
# ate 1
spend_ate$estimate[3]
# ate 2
spend_ate2$estimate[2]

# 2.03 y 2.57

# test
# ate 1
spend_atev$estimate[3]
# ate 2
spend_ate2v$estimate[2]

# 1 y 0.99



