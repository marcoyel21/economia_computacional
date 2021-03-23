## Script de pruebas con remuestreo alternativo



# Primero replicamos los pasos de las preguntas 1-3


# PENDIENTE. COPIAR VERSIÓN FINAL



# creamos las bases alternativas
data_training_b<-oversampled
data_training_c<-under_over_sampling



# LASSO
#####################################################################


## B

#Matriz de covariates
Xb <-data_training_b %>% select(-customer,-churn) 

#se quita intercepto
Xb <- sparse.model.matrix(~.+0, data = Xb) 

#vector de Y´s
Yb<-data_training_b$churn 

#CV LASSO
cvlasso_b<-cv.gamlr(x = Xb, y = Yb, verb = T, family = 'binomial', nfold = 5) 

#Grafica
plot(cvlasso_b)


## C

#Matriz de covariates
Xc <-data_training_c %>% select(-customer,-churn) 

#se quita intercepto
Xc <- sparse.model.matrix(~.+0, data = Xc) 

#vector de Y´s
Yc<-data_training_c$churn 

#CV LASSO
cvlasso_c<-cv.gamlr(x = Xc, y = Yc, verb = T, family = 'binomial', nfold = 5) 

#Grafica
plot(cvlasso_c)


# 5.
plot(cvlasso_b$gamlr)
plot(cvlasso_c$gamlr)


# 6.
# lambda resultante
#B
b_lambda<- colnames(coef(cvlasso_b, select="min")) 
cvlasso_b$gamlr$lambda[b_lambda] 

#c
c_lambda<- colnames(coef(cvlasso_c, select="min")) 
cvlasso_a$gamlr$lambda[c_lambda] 

# tabla de coeficientes
coef(cvlasso_b, select="min", k=2, corrected=TRUE) #b
coef(cvlasso_c, select="min", k=2, corrected=TRUE) #c

#Para c y b coeficientes en blanco, a final aparecen dos coeficientes classFALSE y classTRUE que no puedo quitar.


### 7. Genera un data frame (usando el validation set) que tenga: `customer`, `churn` y las predicciones del LASSO.


#Predicciones
y_pred<-predict(cv_lasso, newdata = X, type = 'response')
y_pred<-as.numeric(y_pred)

#A
y_pred_a<-predict(cvlasso_a, newdata = Xa, type = 'response')
y_pred_a<-as.numeric(y_pred_a)

#B
y_pred_b<-predict(cvlasso_b, newdata = Xb, type = 'response')
y_pred_b<-as.numeric(y_pred_b)

#C
y_pred_c<-predict(cvlasso_c, newdata = Xc, type = 'response')
y_pred_c<-as.numeric(y_pred_c)

#dataframe

B<-data.frame(data_validation_b$customer, data_validation_b$churn, y_pred_b)

C<-data.frame(data_validation_c$customer, data_validation_c$churn, y_pred_c)



### 8. Estima ahora tree. Usa `mindev = 0.05, mincut = 1000` Cuántos nodos terminales salen? Muestra el summary del árbol

cat_variables <- c("churn",
                   "children",
                   "credita",
                   "creditaa",
                   "prizmrur",
                   "prizmub",
                   "prizmtwn",
                   "refurb",
                   "webcap",
                   "truck",
                   "rv",
                   "occprof",
                   "occcler",
                   "occcrft",
                   "occstud",
                   "occhmkr",
                   "occret",
                   "occself",
                   "ownrent",
                   "marryun",
                   "marryyes",
                   "mailord",
                   "mailres",
                   "mailflag",
                   "travel",
                   "pcown",
                   "creditcd",
                   "newcelly",
                   "newcelln",
                   "incmiss",
                   "mcycle",
                   "setprcm",
                   "retcall")


round_up_down <- function(x){
  
  return(if_else(x<0.5,as.integer(floor(x)),
                 as.integer(ceiling(x))))
  
}

data_training_b[,cat_variables] <- lapply(data_training_b[,cat_variables],round_up_down)
data_training_c[,cat_variables] <- lapply(data_training_c[,cat_variables],round_up_down)


summary(data_training_c %>% select(cat_variables) %>% arrange())
View(data_training_c %>% select(cat_variables) %>% arrange())


# hago para los 3 training datasets que tenemos: a,b y c
# b2 es restringiendo variables



#B
tree_control_b <- tree.control(nobs=nrow(data_training_b),
                               mincut = 1000,
                               mindev = 0.05)


tree_estimation_b <- tree(as.factor(churn) ~ . -customer ,
                          data = data_training_b,
                          control = tree_control_b)


tree_estimation_b2 <- tree(as.factor(churn) ~ eqpdays + refurb + changem + months + mou + changer + recchrge + creditcd + setprc + revenue + retcall + marryyes + credita,
                           data = data_training_b)

#C
tree_control_c <- tree.control(nobs=nrow(data_training_c),
                               mincut = 1000,
                               mindev = 0.05)


tree_estimation_c <- tree(as.factor(churn) ~ . -customer,
                          data = data_training_c,
                          control = tree_control_c)


summary(tree_estimation_b)
summary(tree_estimation_b2)
summary(tree_estimation_c)

# El a utiliza más variables


### 9. Grafica el árbol resultante

plot(tree_estimation_b)
text(tree_estimation_b,pretty=0)

plot(tree_estimation_b2)
text(tree_estimation_b2,pretty=0)

plot(tree_estimation_c)
text(tree_estimation_c,pretty=0)


#cross validation y confusionMatrix (opcional)
#data_validation_a$churn <- as.factor(data_validation_a$churn)

#DTpred<- predict(tree_estimation_a,type="class" ,newdata=data_validation_a)
#caret::confusionMatrix(data_validation_a$churn,DTpred)


### 10. Poda el árbol usando CV. Muestra el resultado. Grafica Tree Size vs Binomial Deviance. Cuál es el mejor tamaño del árbol? Mejora el Error?
cv_tree_b <- cv.tree(tree_estimation_b, K=10)
cv_tree_b2 <- cv.tree(tree_estimation_b2, K=10)
cv_tree_c <- cv.tree(tree_estimation_c, K=10)

cv_tree_b
cv_tree_b2
cv_tree_c

# Size con menor deviance
min_dev_size_b <-cv_tree_b$size[match(min(cv_tree_b$dev),cv_tree_b$dev)]
min_dev_size_b2 <-cv_tree_b2$size[match(min(cv_tree_b2$dev),cv_tree_b2$dev)]
min_dev_size_c <-cv_tree_c$size[match(min(cv_tree_c$dev),cv_tree_c$dev)]

# Gráfica Tree Size vs. Binomial Deviance

plot_size_dev <- function(cv_tree){
  
  ggplot(data=as.data.frame(cbind(size=cv_tree$size,dev=cv_tree$dev)),
         aes(x=size,y=dev))+
    geom_point(size=3)+
    labs(title="Deviance against Tree Size")+
    xlab("Tree Size")+
    ylab("Deviance")+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
}

plot_size_dev(cv_tree_b)
plot_size_dev(cv_tree_b2)
plot_size_dev(cv_tree_c)



### 11. Gráfica el árbol final. (Tip: Checa `prune.tree`)
tree_cut_b <- prune.tree(tree_estimation_b, best = min_dev_size_b)
tree_cut_b2 <- prune.tree(tree_estimation_b2, best = min_dev_size_b2)
tree_cut_c <- prune.tree(tree_estimation_c, best = min_dev_size_c)

plot(tree_cut_b)
text(tree_cut_b,pretty=0)

plot(tree_cut_b2)
text(tree_cut_b2,pretty=0)

plot(tree_cut_c)
text(tree_cut_c,pretty=0)


### 12. Genera las predicciones del árbol pruned. Guardalas en la base de predicciones. Guarda el score y la prediccion categorica en la misma data frame donde guardaste las predicciones del LASSO

tree_score_predict_b <- predict(tree_cut_b,data=data_validation_b)
tree_class_predict_b <- predict(tree_cut_b,data=data_validation_b,type="class")

tree_score_predict_b2 <- predict(tree_cut_b2,data=data_validation_b2)
tree_class_predict_b2 <- predict(tree_cut_b2,data=data_validation_b2,type="class")

tree_score_predict_c <- predict(tree_cut_c,data=data_validation_c)
tree_class_predict_c <- predict(tree_cut_c,data=data_validation_c,type="class")


# unir con base de predicciones


### 13 (4pts). Corre un Random Forest ahora. Cuál es la $B$ para la que ya no ganamos mucho más en poder predictivo?


# eficientar el Random Forest corriendolo en los nodos disponibles del equipo
cl<- detectCores() %>% makeCluster()
cl

treesb <- c()
errorb <- c()

k <- 1 # iterador
for (i in c(100,200,300,500,700,800)){
  
  rf<-ranger(churn~.,
             data = data_training_b,
             num.trees = i,
             mtry = (ncol(data_training_b)-1) %>% sqrt() %>% floor(),
             min.node.size = 1,
             splitrule = "gini",
             classification = T,
  )
  
  treesb[k] <- rf$num.trees
  errorb[k] <- rf$prediction.error
  
  rf <- NULL
  k <- k + 1
}

treesc <- c()
errorc <- c()
k <- 1 # iterador
for (i in c(100,200,300,500,700,800)){
  
  rf<-ranger(churn~.,
             data = data_training_c,
             num.trees = i,
             mtry = (ncol(data_training_c)-1) %>% sqrt() %>% floor(),
             min.node.size = 1,
             splitrule = "gini",
             classification = T,
  )
  
  treesc[k] <- rf$num.trees
  errorc[k] <- rf$prediction.error
  
  rf <- NULL
  k <- k + 1
}

Sys.time() -a
stopCluster(cl)

rfb <- data.frame(treesb,errorb)
rfc <- data.frame(treesc,errorc)
rfb
rfc


### 14. Escoge un random forest para hacer las predicciones. Grafica la importancia de las variables. Interpreta

# vuelvo a correr el mejor random forest
cl<- detectCores() %>% makeCluster()
cl
best_rfb <- ranger(churn~.,
                  data = data_training_b[,-1],
                  num.trees = 200,
                  mtry = (ncol(data_training_b)-1) %>% sqrt() %>% floor(),
                  importance = "impurity",
                  classification = T)

best_rfc <- ranger(churn~.,
                  data = data_training_c[,-1],
                  num.trees = 200,
                  mtry = (ncol(data_training_c)-1) %>% sqrt() %>% floor(),
                  importance = "impurity",
                  classification = T)

stopCluster(cl)


### 15. Genera las predicciones OOS para el random forest. Guardalas en la misma data.frame que los otros modelos

pred_rfb <- predict(best_rfb, 
                 data = (data_validation[,-1]),
                 type = "response")


evalb <-bind_cols((churn_validation),
                 pred_rfb$predictions)
names(eval) <- c("validation","pred_rf")

pred_rfc<-predict(best_rfc, 
                 data = (data_validation[,-1]),
                 type = "response")


evalc <-bind_cols((churn_validation),
                 pred_rfc$predictions)
names(eval) <- c("validation","pred_rf")


### 17 (4 pts). Genera graficas de las curvas ROC para los tres modelos. Cual parece ser mejor?
library(yardstick)
# ROC Curve
roc_curveb <- roc_curve(data = evalb, 
                       truth = factor(validation),
                       pred_rfb,
                       event_level = "second")

ggplot(roc_curveb, aes(x = 1-specificity, y = sensitivity))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_path()+theme_bw()


roc_curvec <- roc_curve(data = evalc, 
                       truth = factor(validation),
                       pred_rfc,
                       event_level = "second")

ggplot(roc_curvec, aes(x = 1-specificity, y = sensitivity))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_path()+theme_bw()

