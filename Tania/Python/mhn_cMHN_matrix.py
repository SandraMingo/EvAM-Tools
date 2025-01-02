from mhn.optimizers import cMHNOptimizer
import numpy as np

####################################
######## MODELO cMHN ###############
####################################

# Crear una instancia del optimizador cMHN
optimizer = cMHNOptimizer()


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


# Ver propiedades de los datos cargados
data_properties = optimizer.get_data_properties()
print("Propiedades de los datos:", data_properties)

# Configurar el dispositivo (opcional)
optimizer.set_device(cMHNOptimizer.Device.CPU)


# Encontrar el mejor valor de lambda con validación cruzada (opcional)
best_lambda, lambda_scores = optimizer.lambda_from_cv(
    lambda_min=0.001, 
    lambda_max=0.1, 
    steps=5, 
    nfolds=3, 
    show_progressbar=True, 
    return_lambda_scores=True
)

print("Mejor valor de lambda:", best_lambda)
print("Resultados de la validación cruzada:")
print(lambda_scores)


# Crear una matriz inicial theta inicial (deb ser 4x4 en este caso) (logaritmica)
init_theta = np.log(np.ones((4, 4)))

# Configurar theta inicial
optimizer.set_init_theta(init_theta)


# Entrenar el modelo con el mejor lambda encontrado
result_model = optimizer.train(
    lam=best_lambda, 
    maxit=100, 
    trace=True, 
    reltol=1e-6, 
    round_result=True
)

# Ver el modelo entrenado
print("Modelo entrenado:")
print(result_model)


# Ver la matriz theta final
print("Theta final (log):")
print(result_model.log_theta)

# Ver metadatos del modelo
print("Metadatos del entrenamiento:")
print(result_model.meta)

# Obtener la matriz de datos binaria original
training_data = optimizer.training_data
print("Datos de entrenamiento:")
print(training_data)
