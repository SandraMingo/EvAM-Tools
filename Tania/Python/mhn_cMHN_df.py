import numpy as np
import pandas as pd
from mhn.optimizers import cMHNOptimizer

# Crear un DataFrame de ejemplo con datos de mutación
data = {
    'Gene1': [0, 1, 1, 0, 1, 0, 0],
    'Gene2': [0, 1, 1, 0, 0, 1, 1],
    'Gene3': [1, 0, 1, 0, 0, 0, 0],
    'Gene4': [1, 0, 1, 0, 0, 0, 0]
}
df = pd.DataFrame(data)

# Inicializar el optimizador
optimizer = cMHNOptimizer()

# Cargar los datos en el optimizador
optimizer.load_data_matrix(df)

# Establecer un valor inicial para theta (opcional)
#init_theta = np.log(np.random.rand(df.shape[1], df.shape[1]))
#optimizer.set_init_theta(init_theta)

# Entrenar el modelo
trained_model = optimizer.train(maxit=1000, trace=True)

# Obtener las propiedades de los datos
data_properties = optimizer.get_data_properties()
print("Data Properties:", data_properties)

# Obtener el resultado del entrenamiento
result = optimizer.result
print("Trained Model:", result)

# Guardar el progreso del entrenamiento (opcional)
#optimizer.save_progress(steps=100, always_new_file=True, filename='theta_backup.npy')

# Realizar validación cruzada para encontrar el mejor valor de lambda
#best_lambda = optimizer.lambda_from_cv(lambda_min=0.01, lambda_max=1.0, steps=10, nfolds=5, show_progressbar=True)
#print("Best Lambda:", best_lambda)