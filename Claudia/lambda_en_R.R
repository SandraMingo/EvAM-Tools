rm(list=ls())

# Cargar el paquete 'reticulate'
library("reticulate")

# Configurar reticulate para usar Python 3
use_python("/usr/bin/python3", required = TRUE)

# Verificar la configuración de Python
py_config()

# Importar librerías a usar numpy y matplotlib
mhn <- import("mhn")
np <- import("numpy")
plt <- import("matplotlib.pyplot")
random <- import("random")

# Usar reticulate para obtener el atributo __version__ del paquete mhn
mhn_version <- py_get_attr(mhn, "__version__")

# Imprimir la versión
print(mhn_version)


#Dataset a emplear 
# Cargar el archivo CSV en un dataframe en R
data <- read.csv('LUAD_n12.csv')

# Asegurarse de que los datos se carguen correctamente
head(data)

#Nos quedamos con tan solo 500 registros
set.seed(6)
data_500 <- data[sample(nrow(data), 500), ]

#Pasar data_500 a python
py$input <- data_500

py_run_string("

from mhn.optimizers import oMHNOptimizer, cMHNOptimizer

# Inicializar los optimizadores
cMHN_opt = cMHNOptimizer()
oMHN_opt = oMHNOptimizer()

# Cargar los datos en los optimizadores
cMHN_opt.load_data_matrix(input)
oMHN_opt.load_data_matrix(input)

# Establecer las penalizaciones para cMHN y oMHN
cMHN_opt.set_penalty(cMHNOptimizer.Penalty.L1)
oMHN_opt.set_penalty(oMHNOptimizer.Penalty.SYM_SPARSE)

# Verificar la configuración de los optimizadores
print(cMHN_opt)
print(oMHN_opt)
")




#Valores para calcular lambda
py_run_string("

import numpy as np

lambda_min = 0.1 / len(input)
lambda_max = 100 / len(input)

n_cv_steps = 5

lambda_sequence = np.exp(np.linspace(
    np.log(lambda_min + 1e-10), np.log(lambda_max + 1e-10), n_cv_steps))

n_cv_folds = 3

cMHN_opt.set_device(cMHNOptimizer.Device.CPU)
oMHN_opt.set_device(cMHNOptimizer.Device.CPU)
")

#Calcular lambda para cada modelo
py_run_string("

cMHN_lambda = cMHN_opt.lambda_from_cv(
    lambda_min=lambda_min, lambda_max=lambda_max, steps=n_cv_steps, nfolds=n_cv_folds, show_progressbar=True
)

oMHN_lambda = oMHN_opt.lambda_from_cv(
    lambda_min=lambda_min, lambda_max=lambda_max, steps=n_cv_steps, nfolds=n_cv_folds, show_progressbar=True
)
")


# Obtener las secuencias de lambdas desde Python a R si necesitas usarlas
cMHN_lambda <- py$cMHN_lambda
oMHN_lambda <- py$oMHN_lambda



#Calcular
c_modelo <- evam(data_500, methods = c('MHN'), mhn_opts = list(lambda = cMHN_lambda))
c_modelo$MHN_theta


#####################################################################################################
#Intentar crear funcion
calculate_c_lambda <- function(dataframe, n_steps, n_folds, penalty = 'L1'){
  
  py$input <- dataframe
  py$n_cv_steps <- n_steps
  py$n_cv_folds <- n_folds
  py$type_penalty <- penalty
  
  py_run_string("
  
import numpy
from mhn.optimizers import cMHNOptimizer

#Convertir n_cv_steps y n_cv_folds en enteros
n_cv_steps = int(n_cv_steps)
n_cv_folds = int(n_cv_folds)

# Inicializar los optimizadores
cMHN_opt = cMHNOptimizer()

#Introducimos los datos
cMHN_opt.load_data_matrix(input)


if type_penalty == 'L1':
  cMHN_opt.set_penalty(cMHNOptimizer.Penalty.L1)

elif type_penalty == 'SYM_SPARSE':
  cMHN_opt.set_penalty(cMHNOptimizer.Penalty.SYM_SPARSE)
  
else:
  print('Type of penalty not valid')
  raise ValueError


import numpy as np

lambda_min = 0.1 / len(input)
lambda_max = 100 / len(input)

lambda_sequence = np.exp(np.linspace(
    np.log(lambda_min + 1e-10), np.log(lambda_max + 1e-10), n_cv_steps))
    
cMHN_opt.set_device(cMHNOptimizer.Device.CPU)

cMHN_lambda = cMHN_opt.lambda_from_cv(
    lambda_min=lambda_min, lambda_max=lambda_max, steps=n_cv_steps, nfolds=n_cv_folds, show_progressbar=True)

")
  lambda <- py$cMHN_lambda
  
  return (lambda)
   
}

cMHN_lambda <-calculate_c_lambda(data_500, 4, 5, 'SYM_SPARSE')

c_modelo <- evam(data_500, methods = c('MHN'), mhn_opts = list(lambda = cMHN_lambda))
c_modelo$MHN_theta



####################################################################################################
#####################################################################################################
#####################################################################################################
#Intentar crear funcion
calculate_o_lambda <- function(dataframe, n_steps, n_folds, penalty = 'L1'){
  
  py$input <- dataframe
  py$n_cv_steps <- n_steps
  py$n_cv_folds <- n_folds
  py$type_penalty <- penalty
  
  py_run_string("
  
import numpy
from mhn.optimizers import oMHNOptimizer

#Convertir n_cv_steps y n_cv_folds en enteros
n_cv_steps = int(n_cv_steps)
n_cv_folds = int(n_cv_folds)

# Inicializar los optimizadores
oMHN_opt = oMHNOptimizer()

#Introducimos los datos
oMHN_opt.load_data_matrix(input)


if type_penalty == 'L1':
  oMHN_opt.set_penalty(oMHNOptimizer.Penalty.L1)

elif type_penalty == 'SYM_SPARSE':
  oMHN_opt.set_penalty(oMHNOptimizer.Penalty.SYM_SPARSE)
  
else:
  print('Type of penalty not valid')
  raise ValueError


import numpy as np

lambda_min = 0.1 / len(input)
lambda_max = 100 / len(input)

lambda_sequence = np.exp(np.linspace(
    np.log(lambda_min + 1e-10), np.log(lambda_max + 1e-10), n_cv_steps))
    
oMHN_opt.set_device(oMHNOptimizer.Device.CPU)

oMHN_lambda = oMHN_opt.lambda_from_cv(
    lambda_min=lambda_min, lambda_max=lambda_max, steps=n_cv_steps, nfolds=n_cv_folds, show_progressbar=True)

")
  lambda <- py$oMHN_lambda
  
  return (lambda)
  
}

oMHN_lambda <-calculate_o_lambda(data_500, 4, 5, 'SYM_SPARSE')

o_modelo <- evam(data_500, methods = c('MHN'), mhn_opts = list(lambda = oMHN_lambda))
o_modelo$MHN_theta

