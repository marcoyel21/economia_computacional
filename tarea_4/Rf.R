```{r, cache=FALSE, include=FALSE}

# !!!! OJO
#CAUSAL TREE (ES OPCIONAL, para quien le interese)
####################################################


#1.- usamos nuestra base de entrenamiento
#df_training <- crm %>% 
#filter(training_sample==1)


#2.- Dividimos entrenamiento entre Splitting y Estimating

# tamaño del split
split_size <- floor(nrow(set_training_original)*0.5)

# aleatoriamente generamos el split
split_id <- sample(nrow(set_training_original),replace=FALSE, size=split_size)

# hacemos el split
df_split <- set_training_original[split_id,] 
df_est <- set_training_original[-split_id]

# Ahora sí, el árbol:

tree_formula <- as.formula(paste("outcome_spend",paste(names(crm %>% select(-no_covariates)),collapse = ' + '), sep = " ~ "))

ctree_unpruned <- honest.causalTree(
  formula=tree_formula,
  data=df_split,
  est_data=df_est,
  treatment=df_split$mailing_indicator,       # variable de tratamiento
  est_treatment=df_est$mailing_indicator,
  split.Rule="CT",
  cv.option = "TOT",
  cp=0,
  split.Honest = TRUE,
  cv.Honest=TRUE,
  minsize=5,
  HonestSampleSize = nrow(df_est)
)


# Ahora hacemos crossvalidate
ctree_cptable <- as.data.frame(ctree_unpruned$cptable)

# Obtenemos parámetro óptimo de complejidad para podar el árbol
selected_cp <- which.min(ctree_cptable$xerror)
optim_cp_ct <- ctree_cptable[selected_cp, "CP"]

# podamos el árbol
ctree_pruned <- prune(tree=ctree_unpruned, cp=optim_cp_ct)

# dibujemos el árbol


# predecimos estimaciones puntuales en la estimation sample
tauhat_causaltree_est <- predict(ctree_pruned, newdata=df_est)

# saquemos errore estándar
# el paquete no los saca

num_hojas <- length(unique(tauhat_causaltree_est))

```