## Probamos a hacer una demo pero para BRAC:
rm(list = ls())
# Cargar la librería evamtools
library(evamtools)

##Preguntamos alguna cosilla/función
?`evamtools-deprecated`

# Paso 1: Generar un modelo aleatorio
set.seed(123) 
model <- random_evam(ngenes = 5, model = "MHN")

# Inspeccionar el modelo generado
cat("Modelo generado:\n")
print(model)

# Paso 2: Muestrear datos del modelo generado
n_samples <- 100  # Número de muestras
sampled_data <- sample_evam(model, n_samples)

cat("Datos muestreados:\n")
print(head(sampled_data))  # Mostrar las primeras filas de los datos

# Paso 3: Visualizar el modelo generado
plot_evam(model)

# (Opcional) Guardar la visualización como archivo de imagen
png("modelo_evamtools.png", width = 800, height = 600)
plot_evam(model, main = "Modelo de Acumulación de Eventos")
dev.off()

# Paso 4: Usar la aplicación interactiva (Shiny)
cat("Ejecutando la aplicación interactiva...\n")
runShiny()

#############################################

# Leer el archivo CSV como un data frame (como demo mhn Python)
input_df <- read.csv('BRCA_ba_s.csv')

# Convertir el data frame a una matriz
input_matrix <- as.matrix(input_df) 
##Es necesrio que sea una matriz??? En python tiene que ser un dataframe??

# Verificar la clase
class(input_matrix)
class(input_df)


################## Probamos como dataframe

dim(input_df)  # Ver las dimensiones
head(input_df) # Ver las primeras filas

# Número de observaciones (filas)
cat("Number of observations:", nrow(input_df), "\n")

# Número de eventos (columnas)
cat("Number of events:", ncol(input_df), "\n")

# Imprimir mensaje
cat("Event frequencies:\n")

# Calcular frecuencias
event_frequencies <- colSums(input_df) / nrow(input_df)

# Imprimir frecuencias
print(event_frequencies)

# Establecer la semilla para la reproducibilidad
set.seed(6)

# Seleccionar un subconjunto aleatorio de 500 filas #### SOLO SI ES GRANDE
#input_subset_df <- input_df[sample(1:nrow(input_df), size = 500, replace = FALSE), ]
#En dataframe, permite "pinchar" en el environment

# Calcular la suma por filas
row_sums <- rowSums(input_subset_df)

# Contar las frecuencias de cada suma y ordenarlas
distribution <- table(row_sums)

# Mostrar la distribución ordenada
print(distribution)




################## Probamos como matriz:

dim(input_matrix)  # Ver las dimensiones
head(input_matrix) # Ver las primeras filas

# Número de observaciones (filas)
cat("Number of observations:", nrow(input_matrix), "\n")

# Número de eventos (columnas)
cat("Number of events:", ncol(input_matrix), "\n")

# Imprimir mensaje
cat("Event frequencies:\n")

# Calcular frecuencias
event_frequencies <- colSums(input_matrix) / nrow(input_matrix)

# Imprimir frecuencias
print(event_frequencies)

# Establecer la semilla para la reproducibilidad
set.seed(6)

# Seleccionar un subconjunto aleatorio de 500 #### SOLO SI ES GRANDE
#input_subset_mat <- input_matrix[sample(1:nrow(input_matrix), size = 500, replace = FALSE), ]

# Calcular la suma por filas
row_sums <- rowSums(input_subset_mat)

# Contar las frecuencias de cada suma y ordenarlas
distribution <- table(row_sums)

# Mostrar la distribución ordenada
print(distribution)


#####################################################################
#####################################################################


### Probamos con evam, sin modificar cores

# Dataframe 
evam_input_df <- evam(input_df, methods = "MHN") ##No usar este si la mustra es muy grande
evam_input_df$MHN_theta

# Tarda 0.393

##Dataframe de subconjunto:
#evam_input_subset_df <- evam(input_subset_df, methods = "MHN")
#evam_input_subset_df$MHN_theta
#(event_frequencies_df <- colSums(input_subset_df) / nrow(input_subset_df))

#############
# Matriz
evam_input_matrix <- evam(input_matrix, methods = "MHN")
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.217

##Matriz de subconjunto
#evam_input_subset_mat <- evam(input_subset_mat, methods = "MHN")
#evam_input_subset_mat$MHN_theta
#(event_frequencies_mat <- colSums(input_subset_mat) / nrow(input_subset_mat))


#######################################
### Vamos a ver si hay alguna diferencia en el tiempo de procesado según el nº de cores.
### Probamos con evam, modificando cores = 1, = 2, = 3, = 4

# Dataframe 
evam_input_df <- evam(input_df, methods = "MHN", cores = 1)
evam_input_df$MHN_theta

# Tarda 0.24


evam_input_df <- evam(input_df, methods = "MHN", cores = 2)
evam_input_df$MHN_theta

# Tarda 0.252


evam_input_df <- evam(input_df, methods = "MHN", cores = 3)
evam_input_df$MHN_theta

# Tarda 0.164


evam_input_df <- evam(input_df, methods = "MHN", cores = 4)
evam_input_df$MHN_theta

# Tarda 0.209


evam_input_df <- evam(input_df, methods = "MHN", cores = 7)
evam_input_df$MHN_theta

# Tarda 0.152



#############
# Matriz
evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 1)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.298


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 2)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.253


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 3)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.248


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 4)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.29

evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 7)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 0.156




################################################################################
################################################################################
###### Repetimos los tiempos pero con una base de datos grande:

# Leer el archivo CSV como un data frame (como demo mhn Python)
input_df <- read.csv('LUAD_n12.csv')

# Convertir el data frame a una matriz
input_matrix <- as.matrix(input_df) 

### Probamos con evam, sin modificar cores

# Dataframe 
evam_input_df <- evam(input_df, methods = "MHN") ##No usar este si la mustra es muy grande
evam_input_df$MHN_theta
evam_input_df$MHN_trans_mat

# Tarda 54.359 LUAD
# Tarda 52.138 LUAD_500
# Tarda 0.221 BRCA


#############
# Matriz
evam_input_matrix <- evam(input_matrix, methods = "MHN")
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 53.017


#######################################
### Vamos a ver si hay alguna diferencia en el tiempo de procesado según el nº de cores.
### Probamos con evam, modificando cores = 1, = 2, = 3, = 4

rm(list = ls())  ## No sé si ayudará borrar y cargar los datos cada vez
input_df <- read.csv('LUAD_n12.csv')
input_matrix <- as.matrix(input_df) 

##y el otro:
input_df <- read.csv('LUAD_500.csv')

############
#Probamos la función:
#

# Dataframe 
evam_input_df <- evam(input_df, methods = "MHN", cores = 1)
evam_input_df$MHN_theta

# Tarda 53.48 LUAD
# Tarda 50.872 LUAD_500
# Tarda 0.331 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 2)
evam_input_df$MHN_theta

# Tarda 53.237 LUAD
# Tarda 47.513 LUAD_500
# Tarda 0.136 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 3)
evam_input_df$MHN_theta

# Tarda 53.34 LUAD
# Tarda 49.798 LUAD_500
# Tarda 0.146 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 4)
evam_input_df$MHN_theta

# Tarda 49.845 LUAD
# Tarda 49.689 LUAD_500
# Tarda 0.188 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 5)
evam_input_df$MHN_theta

# Tarda 59.316 LUAD
# Tarda 52.229 LUAD_500
# Tarda 0.283 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 6)
evam_input_df$MHN_theta

# Tarda 61.362 LUAD
# Tarda 51.777 LUAD_500
# Tarda 0.178 BRCA


evam_input_df <- evam(input_df, methods = "MHN", cores = 7)
evam_input_df$MHN_theta

# Tarda 48.871 LUAD
# Tarda 52.208 LUAD_500
# Tarda 0.353 BRCA



#############
# Matriz
evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 1)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 51.872


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 2)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 52.296


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 3)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 49.072


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 4)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 50.542


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 5)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 55.692


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 6)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 53.852


evam_input_matrix <- evam(input_matrix, methods = "MHN", cores = 7)
evam_input_matrix$MHN_theta   #Comprobamos que es el mismo resultado que el dataframe

# Tarda 50.054







#########################################
# Resultados importantes:
out$MHN_theta      # Es la tabla cMHN

out$MHN_exp_theta  # Es la tabla tras hacer e^



########################################################
########################################################
########################################################
############# Probamos si podems modificar la función.


trace("evam", edit = TRUE)
# Vemos que habría que modificar varios apartados y que habría que añadir o un 
# nuevo método que repita el código de MHN pero con la corrección, o una variable a parte
# (Yo creo que la idea del método puede ser más sencilla).

## Voy a ir dejando por aquí las funciones escritas para que podemos modificarlas por aquí 
## y luego sea sólo pegarla.

### evam:
#
#function (x, methods = c("CBN", "OT", "HESBCN", "MHN", "OncoBN", 
#"MCCBN"), max_cols = 15, cores = detectCores(), paths_max = FALSE, 
#mhn_opts = list(lambda = 1/nrow(x), omp_threads = ifelse(cores > 
#                                                           1, 1, detectCores())), ot_opts = list(with_errors_dist_ot = TRUE), 
#                                                           cbn_opts = list(omp_threads = 1, init_poset = "OT"), hesbcn_opts = list(MCMC_iter = 1e+05, 
#                                                                                                                                   seed = NULL, reg = c("bic", "aic", "loglik"), silent = TRUE), 
#                                                           oncobn_opts = list(model = "DBN", algorithm = "DP", k = 3, 
#                                                                              epsilon = min(colMeans(x)/2), silent = TRUE), mccbn_opts = list(model = "OT-CBN", 
#                                                                                                                                              tmp_dir = NULL, addname = NULL, silent = TRUE, L = 100, 
#                                                                                                                                              sampling = c("forward", "add-remove", "backward", "bernoulli", 
#                                                                                                                                                           "pool"), max.iter = 100L, update.step.size = 20L, 
#                                                                                                                                              tol = 0.001, max.lambda.val = 1e+06, T0 = 50, adap.rate = 0.3, 
#                                                                                                                                              acceptance.rate = NULL, step.size = NULL, max.iter.asa = 10000L, 
#                                                                                                                                              neighborhood.dist = 1L, adaptive = TRUE, thrds = 1L, 
#                                                                                                                                              verbose = FALSE, seed = NULL)) 
#{
#  gn_comma <- stringi::stri_count_fixed(colnames(x), ",")
#  if (any(gn_comma)) 
#    stop("At least one of your gene names has a comma. That is not allowed")
#  gn_backslash <- stringi::stri_count_fixed(colnames(x), "\\")
#  if (any(gn_backslash)) 
#    stop("At least one of your gene names has a backslash. That is not allowed")
#  gn_space <- stringi::stri_count_regex(colnames(x), "[\\s]")
#  if (any(gn_space)) 
#    stop("At least one of your gene names has a space. That is not allowed")
#  if (any(colnames(x) == "WT")) 
#    stop("One of your genes is called WT. That is not allowed")
#  methods <- unique(methods)
#  accepted_methods <- c("OT", "OncoBN", "CBN", "MCCBN", "MHN", 
#                        "HESBCN")
#  not_valid_methods <- which(!(methods %in% accepted_methods))
#  if (length(not_valid_methods)) {
#    warning("Method(s) ", paste(methods[not_valid_methods], 
#                                sep = ", ", collapse = ", "), " not among the available methods.", 
#            " Ignoring the invalid method.")
#    methods <- methods[-not_valid_methods]
#  }
#  if ("MCCBN" %in% methods) {
#    MCCBN_INSTALLED <- requireNamespace("mccbn", quietly = TRUE)
#    if (!MCCBN_INSTALLED) {
#      warning("MCCBN method requested, but mccbn packaged not installed. ", 
#              "Removing MCCBN from list of requested methods.")
#      methods <- setdiff(methods, "MCCBN")
#    }
#  }
#  if (length(methods) == 0) 
#    stop("No valid methods given.")
#  x <- df_2_mat_integer(x)
#  xoriginal <- x
#  x <- add_pseudosamples(x)
#  x <- pre_process(x, remove.constant = FALSE, min.freq = 0, 
#                   max.cols = max_cols)
#  if (ncol(x) < 2) {
#    stop("Fewer than 2 columns in the data. ", "There must be at least two genes ", 
#         "and two different genotypes to run evam ", "(and remember that any genes that are ", 
#         "completely aliased, i.e., indistinguishable, ", 
#         "because they have identical patterns ", "---identical columns in the data matrix--- ", 
#         "are regarded as a single gene).")
#  }
#  d_mhn_opts <- list(lambda = 1/nrow(x), omp_threads = ifelse(cores > 
#                                                                1, 1, detectCores()))
#  d_cbn_opts <- list(omp_threads = 1, init_poset = "OT")
#  d_hesbcn_opts <- list(MCMC_iter = 1e+05, seed = NULL, reg = c("bic", 
#                                                                "aic", "loglik"), silent = TRUE)
#  d_oncobn_opts <- list(model = "DBN", algorithm = "DP", k = 3, 
#                        epsilon = min(colMeans(x)/2), silent = TRUE)
#  d_mccbn_opts <- list(model = "OT-CBN", tmp_dir = NULL, addname = NULL, 
#                       silent = TRUE, L = 100, sampling = c("forward", "add-remove", 
#                                                            "backward", "bernoulli", "pool"), max.iter = 100L, 
#                       update.step.size = 20L, tol = 0.001, max.lambda.val = 1e+06, 
#                       T0 = 50, adap.rate = 0.3, acceptance.rate = NULL, step.size = NULL, 
#                       max.iter.asa = 10000L, neighborhood.dist = 1L, adaptive = TRUE, 
#                       thrds = 1L, verbose = FALSE, seed = NULL)
#  mhn_opts_2 <- fill_args_default(mhn_opts, d_mhn_opts)
#  cbn_opts_2 <- fill_args_default(cbn_opts, d_cbn_opts)
#  hesbcn_opts_2 <- fill_args_default(hesbcn_opts, d_hesbcn_opts)
#  oncobn_opts_2 <- fill_args_default(oncobn_opts, d_oncobn_opts)
#  mccbn_opts_2 <- fill_args_default(mccbn_opts, d_mccbn_opts)
#  rm(cbn_opts, hesbcn_opts, oncobn_opts, mccbn_opts, mhn_opts)
#  rm(d_cbn_opts, d_hesbcn_opts, d_oncobn_opts, d_mccbn_opts, 
#     d_mhn_opts)
#  if (!(cbn_opts_2$init_poset %in% c("OT", "linear"))) 
#    stop("CBN's init_poset must be one of OT or linear. ", 
#         " Custom not allowed in call from evam.")
#  if ("MCCBN" %in% methods) {
#    stopifnot(mccbn_opts_2$model %in% c("OT-CBN", "H-CBN2"))
#  }
#  if ("OncoBN" %in% methods) {
#    stopifnot(oncobn_opts_2$model %in% c("DBN", "CBN"))
#  }
#  do_method <- function(method) {
#    if (method == "MHN") {
#      RhpcBLASctl::omp_set_num_threads(mhn_opts_2$omp_threads)
#      time_out <- system.time({
#        out <- do_MHN2(x, lambda = mhn_opts_2$lambda)
#        out <- c(out, predicted_genotype_freqs = list(probs_from_trm(out$transitionRateMatrix)))
#      })["elapsed"]
#    }
#    else if (method == "HESBCN") {
#      time_out <- system.time({
#        out <- do_HESBCN(x, MCMC_iter = hesbcn_opts_2$MCMC_iter, 
#                         seed = hesbcn_opts_2$seed, silent = hesbcn_opts_2$silent, 
#                         reg = hesbcn_opts_2$reg)
#        out <- c(out, cpm2tm(out))
#        out <- c(out, td_trans_mat = trans_rate_to_trans_mat(out[["weighted_fgraph"]], 
#                                                             method = "uniformization"))
#        out <- c(out, predicted_genotype_freqs = list(probs_from_trm(out$weighted_fgraph)))
#      })["elapsed"]
#    }
#    else if (method == "CBN") {
#      time_out <- system.time({
#        out <- try(cbn_proc(x, addname = "tmpo", init.poset = cbn_opts_2$init_poset, 
#                            nboot = 0, parall = TRUE, omp_threads = cbn_opts_2$omp_threads))
#        out <- c(out, cpm2tm(out))
#        out <- c(out, td_trans_mat = trans_rate_to_trans_mat(out[["weighted_fgraph"]], 
#                                                             method = "uniformization"))
#        out <- c(out, predicted_genotype_freqs = list(probs_from_trm(out$weighted_fgraph)))
#      })["elapsed"]
#    }
#    else if (method == "MCCBN") {
#      if (mccbn_opts_2$model == "OT-CBN") 
#        time_out <- system.time(out <- try(do_MCCBN_OT_CBN(x)))["elapsed"]
#      else if (mccbn_opts_2$model == "H-CBN2") {
#        mccbn_hcbn2_opts_2 <- mccbn_opts_2
#        mccbn_hcbn2_opts_2$model <- NULL
#        time_out <- system.time(out <- try(do_MCCBN_HCBN2(x, 
#                                                          mccbn_hcbn2_opts = mccbn_hcbn2_opts_2)))["elapsed"]
#      }
#      time_out2 <- system.time({
#        out <- c(out, cpm2tm(out))
#        out <- c(out, td_trans_mat = trans_rate_to_trans_mat(out[["weighted_fgraph"]], 
#                                                             method = "uniformization"))
#        out <- c(out, predicted_genotype_freqs = list(probs_from_trm(out$weighted_fgraph)))
#      })["elapsed"]
#      time_out <- time_out + time_out2
#    }
#    else if (method == "OT") {
#      time_out <- system.time({
#        out <- try(suppressMessages(ot_proc(x, nboot = 0, 
#                                            distribution.oncotree = TRUE, with_errors_dist_ot = ot_opts$with_errors_dist_ot)))
#        out <- c(out, cpm2tm(out))
#      })["elapsed"]
#    }
#    else if (method == "OncoBN") {
#      time_out <- system.time({
#        out <- do_OncoBN(x, model = oncobn_opts_2$model, 
#                         algorithm = oncobn_opts_2$algorithm, k = oncobn_opts_2$k, 
#                         epsilon = oncobn_opts_2$epsilon, silent = oncobn_opts_2$silent)
#        out <- c(out, cpm2tm(out))
#      })["elapsed"]
#    }
#    message(paste0("time ", method, ": ", round(time_out, 
#                                                3)))
#    out
#  }
#  all_out <- mclapply(methods, do_method, mc.cores = cores)
#  names(all_out) <- methods
#  get_output <- function(method, component) {
#    if (!exists(method, all_out)) 
#      return(NA)
#    if (!exists(component, all_out[[method]])) 
#      return(NA)
#    return(all_out[[method]][[component]])
#  }
#  get_paths_max <- function(method) {
#    if (paths_max) {
#      trans_mat_name <- ifelse(method == "MHN", "transitionMatrixCompExp", 
#                               "trans_mat_genots")
#      trans_mat <- get_output(method, trans_mat_name)
#      if ((length(trans_mat) == 1) && is.na(trans_mat)) 
#        return(NA)
#      return(paths_probs_2_df(trans_mat_2_paths_probs(trans_mat), 
#                              order = "prob"))
#    }
#    else {
#      return(NA)
#    }
#  }
#  return(list(OT_model = get_output("OT", "edges"), OT_f_graph = get_output("OT", 
#                                                                            "weighted_fgraph"), OT_trans_mat = get_output("OT", "trans_mat_genots"), 
#              OT_predicted_genotype_freqs = get_output("OT", "predicted_genotype_freqs"), 
#              OT_eps = get_output("OT", "eps"), OT_fit = get_output("OT", 
#                                                                    "ot_fit"), OT_paths_max = get_paths_max("OT"), CBN_model = get_output("CBN", 
#                                                                                                                                          "edges"), CBN_trans_rate_mat = get_output("CBN", 
#                                                                                                                                                                                    "weighted_fgraph"), CBN_trans_mat = get_output("CBN", 
#                                                                                                                                                                                                                                   "trans_mat_genots"), CBN_td_trans_mat = get_output("CBN", 
#                                                                                                                                                                                                                                                                                      "td_trans_mat"), CBN_predicted_genotype_freqs = get_output("CBN", 
#                                                                                                                                                                                                                                                                                                                                                 "predicted_genotype_freqs"), CBN_paths_max = get_paths_max("CBN"), 
#              MCCBN_model = get_output("MCCBN", "edges"), MCCBN_trans_rate_mat = get_output("MCCBN", 
#                                                                                            "weighted_fgraph"), MCCBN_trans_mat = get_output("MCCBN", 
#                                                                                                                                             "trans_mat_genots"), MCCBN_td_trans_mat = get_output("CBN", 
#                                                                                                                                                                                                  "td_trans_mat"), MCCBN_predicted_genotype_freqs = get_output("MCCBN", 
#                                                                                                                                                                                                                                                               "predicted_genotype_freqs"), MCCBN_paths_max = get_paths_max("MCCBN"), 
#              MHN_theta = get_output("MHN", "theta"), MHN_trans_rate_mat = get_output("MHN", 
#                                                                                      "transitionRateMatrix"), MHN_trans_mat = get_output("MHN", 
#                                                                                                                                          "transitionMatrixCompExp"), MHN_td_trans_mat = get_output("MHN", 
#                                                                                                                                                                                                    "transitionMatrixTimeDiscretized"), MHN_exp_theta = exp(get_output("MHN", 
#                                                                                                                                                                                                                                                                       "theta")), MHN_predicted_genotype_freqs = get_output("MHN", 
#                                                                                                                                                                                                                                                                                                                            "predicted_genotype_freqs"), MHN_paths_max = get_paths_max("MHN"), 
#              OncoBN_model = get_output("OncoBN", "edges"), OncoBN_likelihood = get_output("OncoBN", 
#                                                                                           "likelihood"), OncoBN_f_graph = get_output("OncoBN", 
#                                                                                                                                      "weighted_fgraph"), OncoBN_trans_mat = get_output("OncoBN", 
#                                                                                                                                                                                        "trans_mat_genots"), OncoBN_predicted_genotype_freqs = get_output("OncoBN", 
#                                                                                                                                                                                                                                                          "predicted_genotype_freqs"), OncoBN_fitted_model = get_output("OncoBN", 
#                                                                                                                                                                                                                                                                                                                        "model"), OncoBN_epsilon = get_output("OncoBN", "epsilon"), 
#              OncoBN_parent_set = get_output("OncoBN", "parent_set"), 
#              OncoBN_fit = get_output("OncoBN", "fit"), OncoBN_paths_max = get_paths_max("OncoBN"), 
#              HESBCN_model = get_output("HESBCN", "edges"), HESBCN_parent_set = get_output("HESBCN", 
#                                                                                           "parent_set"), HESBCN_trans_rate_mat = get_output("HESBCN", 
#                                                                                                                                             "weighted_fgraph"), HESBCN_trans_mat = get_output("HESBCN", 
#                                                                                                                                                                                               "trans_mat_genots"), HESBCN_td_trans_mat = get_output("HESBCN", 
#                                                                                                                                                                                                                                                     "td_trans_mat"), HESBCN_predicted_genotype_freqs = get_output("HESBCN", 
#                                                                                                                                                                                                                                                                                                                   "predicted_genotype_freqs"), HESBCN_command = get_output("HESBCN", 
#                                                                                                                                                                                                                                                                                                                                                                            "command"), HESBCN_paths_max = get_paths_max("HESBCN"), 
#              original_data = xoriginal, analyzed_data = x, genotype_id_ordered = stats::setNames(1:(2^ncol(x)), 
#                                                                                                  genotypes_standard_order(colnames(x))), all_options = list(mhn_opts = mhn_opts_2, 
#                                                                                                                                                             ot_opts = ot_opts, cbn_opts = cbn_opts_2, hesbcn_opts = hesbcn_opts_2, 
#                                                                                                                                                             oncobn_opts = oncobn_opts_2, mccbn_opts = mccbn_opts_2)))
#
#


########################################################
########################################################
########################################################
############# Probamos Reticulate. 
# (Intento fallido, seguí en R del escritorio, pero no merece la pena poner el código).


# Lo instalamos
install.packages("reticulate")
library(reticulate)

?`reticulate-package`

# Para escoger el archivo de forma interactiva (tiene que estar subido):
file.choose()

# Ponemos el path al archivo
use_python("/home/rstudio/Trabajo_R2024_master/demo/demo.ipynb")
#### Da error
## Es porque lo que hay que introducir no es el path del archivo, sino de "python" en sí.


########################################################
########################################################
########################################################
############# Reticulate con código de Tania. 

# Vamos a comparar el resultado de cMHN con reticulate y con evam

# Cargar el paquete 'reticulate'
library("reticulate")

# Configurar reticulate para usar Python 3
use_python("/usr/bin/python3", required = TRUE)

# Verificar la configuración de Python
py_config()

###

# Importar librerías a usar numpy y matplotlib
mhn <- import("mhn")
np <- import("numpy")
plt <- import("matplotlib.pyplot")
random <- import("random")

# Usar reticulate para obtener el atributo __version__ del paquete mhn
mhn_version <- py_get_attr(mhn, "__version__")

# Imprimir la versión
print(mhn_version)

###

# Cargar el archivo CSV en un dataframe que creó Claudia en R
data <- read.csv('LUAD_500.csv')

# Asegurarse de que los datos se carguen correctamente
head(data)

py_run_string("
import random
import pandas as pd

input = pd.read_csv('LUAD_500.csv')
print (input.head())

print('Number of observations:', len(input)) 
print('Number of events:', len(input.columns))

print('Event frequencies:')
input.sum(axis=0) / len(input)

print('istribution of active event counts across observations:')
input.sum(axis=1).value_counts().sort_index()

# The dataset is a little large to be used completely in this notebook, so let's only take a part of the data:

# Establecer semilla para reproducibilidad
random.seed(6)

# Crear un subconjunto aleatorio de datos (en ese caso, no)
# input_subset = input.sample(n=500)
input_subset = input

# Generar resultados
result = input_subset.sum(axis=1).value_counts().sort_index()
print (result)
")

# Convertir el dataframe de R a un dataframe de pandas en Python
py$data <- r_to_py(data)  # Convertir el dataframe de R a Python

# Verificar cómo está estructurado el DataFrame en Python
py_run_string("
print(result)
")

# Acceder a los resultados de Python en R
input_subset_python <- py$input_subset

# Convertir la Serie de pandas a una lista en Python
py_run_string("
result_list = result.tolist()  # Convertir la Serie a lista
")
# Acceder a la lista desde R
result_list <- py$result_list
print(result_list)


# Ejecutar las funciones de Python usando reticulate
py_run_string("from mhn.optimizers import cMHNOptimizer")
py_run_string("cMHN_opt = cMHNOptimizer()")

# Cargar los datos en los optimizadores
py_run_string("cMHN_opt.load_data_matrix(input_subset)")

# Establecer las penalizaciones para cMHN
py_run_string("cMHN_opt.set_penalty(cMHN_opt.Penalty.L1)")

# Verificar si se cargaron correctamente los datos y la configuración
py_run_string("print(cMHN_opt)")

###

py_run_string("
import random
import pandas as pd
from mhn.optimizers import cMHNOptimizer

# Establecer la semilla para reproducibilidad
random.seed(6)

# Convertir el dataframe a un objeto manejable en Python
input = data

# Estadísticas básicas del conjunto de datos
print('Number of observations:', len(input))  # Número de individuos
print('Number of events:', len(input.columns))  # Número de eventos

print('Event frequencies:')
print(input.sum(axis=0) / len(input))  # Frecuencias de eventos

print('Distribution of active event counts across observations:')
print(input.sum(axis=1).value_counts().sort_index())  # Distribución

# Crear un subconjunto aleatorio de los datos
# input_subset = input.sample(n=500)
result = input_subset.sum(axis=1).value_counts().sort_index()

# Inicializar el optimizador
cMHN_opt = cMHNOptimizer()

# Cargar los datos en el optimizador
cMHN_opt.load_data_matrix(input)

# Establecer las penalizaciones para cMHN
cMHN_opt.set_penalty(cMHNOptimizer.Penalty.L1)

# Verificar la configuración del optimizador
print(cMHN_opt)
")

# Si necesitas acceder a 'result' desde R
result <- py$result
print(result)

######
##Esta parte nos interesa para obtener la lambda:

py_run_string('
lambda_min= 0.1 / len(input_subset)
lambda_max = 100 / len(input_subset)

print("Minimum lambda:", lambda_min)
print("Maximum lambda:", lambda_max)

n_cv_steps = 5

import numpy as np

lambda_sequence = np.exp(np.linspace(
    np.log(lambda_min + 1e-10), np.log(lambda_max + 1e-10), n_cv_steps))

print("Lambda sequence:")
lambda_sequence

n_cv_folds = 3

cMHN_opt.set_device(cMHNOptimizer.Device.CPU)

cMHN_lambda = cMHN_opt.lambda_from_cv(
    lambda_min=lambda_min, lambda_max=lambda_max, steps=n_cv_steps, nfolds=n_cv_folds,
    show_progressbar=True)

import matplotlib.pyplot as plt

# plot original lambda sequence
plt.scatter(lambda_sequence, np.full(len(lambda_sequence), 1), color="black")

# plot cross-validated lambdas
plt.scatter(cMHN_lambda, 2, color="red")

# labels
plt.text(cMHN_lambda, 1.9, "cMHN", ha="center")

# log scale
plt.xscale("log")
plt.yticks([])
plt.ylabel("")
plt.show()
')

# Obtener las secuencias de lambdas desde Python a R si necesitas usarlas
lambda_sequence <- py$lambda_sequence
cMHN_lambda <- py$cMHN_lambda

### Con esto abríamos obtenido la lambda

#####
## ¿Esta parte la consideráis importante?

py_run_string("
# train cMHN
cMHN_opt.train(lam=cMHN_lambda)
# save output
cMHN_opt.result.save(filename='cMHN.csv')

import json
import pprint

with open('cMHN_meta.json') as f:
    cMHN_log = json.load(f)

pprint.pprint(cMHN_log)
")

# Comprobaremos si de alguna manera cambia el resultado.
######

# Gráfico cMHN
py_run_string("import matplotlib.pyplot as plt")

py_run_string("cMHN_opt.result.plot()")

# Mostrar el gráfico generado
py_run_string("plt.show()")

#### Mostramos simplemente la matriz generada
py_run_string("print(cMHN_opt.result)")


###
#####
#######
## Ahora probamos los resultados obtenidos con la función evam

library("evamtools")

datos <- evam(data, methods = "MHN")
datos$MHN_theta      # Es la tabla cMHN

datos$MHN_exp_theta  # Es la tabla tras hacer e^

#otras tablas son:
datos$MHN_trans_mat  #No creo que sea nada de lo que buscamos
datos$MHN_trans_rate_mat #No sé cómo lo saca

##
matriz_normal <- datos$MHN_theta  # Lo guardamos por si acaso


### Repetimos el evam pero utilizaremos la lambda, para ello hay que guardarla en R
#lambda <- py$cMHN_lambda   # Se haría con esto

?evam
datos2 <- evam(data, methods = "MHN", mhn_opts = list(lambda = cMHN_lambda))
datos2$MHN_theta








