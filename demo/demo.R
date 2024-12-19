##INSPECCIÓN DE EVAMTOOLS
library(evamtools)
ls("package:evamtools")
#lista de todas las funciones exportadas por el paquete

##DEMO
# Cargar la librería evamtools
library(evamtools)

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