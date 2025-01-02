from mhn.optimizers import oMHNOptimizer
import numpy as np

###############################################
################### MODELO oMHN ###############
###############################################

#Dado que oMHNOptimizer agrega una fila adicional a theta, 
# es importante ajustar los datos en consecuencia. 
# Con los datos proporcionados (7×4), 
# theta será de forma (5×4) en lugar de (4×4).

# Crear una instancia del optimizador cMHN
optimizer = oMHNOptimizer()


# Crear una matriz de datos binaria (genes x muestras)
data_matrix = np.array([
    [0, 0, 1, 1],
    [1, 1, 0, 0],
    [1, 1, 1, 1],
    [0, 0, 0, 0], 
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 1, 0, 0],
])

# Cargar la matriz de datos (como matriz)
optimizer.load_data_matrix(data_matrix)

# Cargar el archivo CSV
#optimizer.load_data_from_csv("data.csv")

# Ver propiedades de los datos cargados
data_properties = optimizer.get_data_properties()
print("Propiedades de los datos:", data_properties)


# Encontrar el mejor valor de lambda con validación cruzada
#best_lambda, lambda_scores = optimizer.lambda_from_cv(
#    lambda_min=0.001, 
#    lambda_max=0.1, 
#    steps=5, 
#    nfolds=3, 
#    show_progressbar=True, 
#    return_lambda_scores=True
#)

#print("Mejor valor de lambda:", best_lambda)
#print("Resultados de la validación cruzada:")
#print(lambda_scores)

# Crear una matriz inicial theta inicial oMHNOptimizer
#vanilla_theta = create_indep_model(data_matrix)
#n = vanilla_theta.shape[0]
#omega_theta = np.zeros((n + 1, n))
#omega_theta[:n] = vanilla_theta
#init_theta = omega_theta

# Configurar theta inicial
#optimizer.set_init_theta(init_theta)

#optimizer._init_theta = init_theta  # Asegúrate de asignar el `theta` correcto

# Entrenamiento
best_lambda = 0.1 
result_model = optimizer.train(
    lam=best_lambda, 
    maxit=100, 
    trace=True, 
    reltol=1e-6, 
    round_result=True
)

print("Modelo entrenado:")
print(result_model)