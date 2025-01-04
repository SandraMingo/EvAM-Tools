set.seed(42)  # Para la reproducibilidad

# Generamos una matriz Theta de tamaño n x n
n <- 4  # Número de eventos o nodos
Theta <- matrix(runif(n^2, min=-1, max=1), nrow=n, ncol=n)

# Generamos Omega, que será un vector con valores entre 1 y 2 (ya que Omega > 0)
Omega <- runif(n, min=1, max=2)

# Mostrar las matrices generadas
cat("Matriz Theta:\n")
print(Theta)
cat("\nVector Omega:\n")
print(Omega)


## Paso 2: Construcción de la matriz Q

# La matriz Build.Q.Extended es la que tiene la corrección de sesgo Omega
Q <- Build.Q.Extended(Theta, Omega)

# Mostrar la matriz de transición Q generada
cat("\nMatriz de transición Q:\n")
print(Q)

## Paso 3: Verificación de la corrección de sesgo

# Caso sin corrección de sesgo (Omega = 1)
Omega_no_bias <- rep(1, n)
Q_no_bias <- Build.Q.Extended(Theta, Omega_no_bias)

# Caso con corrección de sesgo (Omega aleatorio generado)
Q_with_bias <- Build.Q.Extended(Theta, Omega)

# Comparar las matrices Q
cat("\nMatriz Q sin corrección de sesgo (Omega = 1):\n")
print(Q_no_bias)

#### RESULTADO ESPERADOS #######
### matriz dispersa de 32×32, donde cada elemento tiene valores significativos solo en ciertas posiciones, 
#mientras que el resto son ceros. 
#Esta matriz describe las transiciones entre estados del sistema sin ajustar por el sesgo aleatorio.

cat("\nMatriz Q con corrección de sesgo (Omega aleatorio):\n")
print(Q_with_bias)

##### RESULTADO ESPERADO #######
# Al aplicar la corrección de sesgo utilizando un valor aleatorio para omega, 
#la matriz cambia en algunos valores, reflejando el ajuste en las transiciones. 
#A diferencia de la primera, algunos valores de la matriz ahora están modificados por el sesgo 
#y aparecen más dispersos o ajustados en algunas posiciones.


## resultados Tania: 
## Ambas matrices son dispersas y muestran una estructura similar, 
#pero la matriz con corrección de sesgo tiene una ligera alteración en los 
#valores en comparación con la matriz original.
#El proceso de simulación ha ejecutado correctamente y las matrices generadas son 
#consistentes con los ajustes esperados.


# Paso 4: Cálculo de la distribución estacionaria

# Suponiendo que ya tienes la función Stationary.Distribution con la corrección de Omega
p0 <- rep(1, 2^n) / 2^n  # Distribución inicial uniforme

# Distribución estacionaria sin corrección de sesgo
p_no_bias <- Stationary.Distribution(Theta, Omega_no_bias, p0)

# Distribución estacionaria con corrección de sesgo
p_with_bias <- Stationary.Distribution(Theta, Omega, p0)

# Mostrar las distribuciones estacionarias
cat("\nDistribución estacionaria sin corrección de sesgo (Omega = 1):\n")
print(p_no_bias)


### Distribución estacionaria sin corrección de sesgo (Omega = 1)
### Los valores en la matriz suman 1, lo cual es característico de una distribución de probabilidad válida.
### El valor más grande en la distribución es 0.576493623, 
### lo que indica que un estado tiene una probabilidad significativamente mayor que los otros. 
### Esto es normal en sistemas donde un estado es más probable que los demás 
### debido a la naturaleza de las transiciones de la matriz Q.

cat("\nDistribución estacionaria con corrección de sesgo (Omega aleatorio):\n")
print(p_with_bias)

# Distribución estacionaria con corrección de sesgo (Omega aleatorio)
# Los valores también suman 1, lo que confirma que es una distribución de probabilidad válida.
# El cambio de valores en esta matriz muestra que la corrección de sesgo (aplicando
# omega aleatorio) ha alterado las probabilidades de cada estado.
# Esto refleja cómo las transiciones son modificadas por el sesgo aleatorio,
# generando una ligera reordenación en las probabilidades. En este caso, la probabilidad más alta
# (antes  0.481241569, mientras que otros valores han aumentado ligeramente.


# Verificación de que la matriz es estocástica
if(all(abs(rowSums(Q) - 1) < 1e-6)) {
  print("Q es estocástica: cada fila suma 1.")
} else {
  print("Q no es estocástica.")
}

                                                                                                                                                                                                                                                                                                                    
