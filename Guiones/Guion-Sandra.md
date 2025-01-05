Los dos pasos más intensivos en computación de este módulo son la búsqueda del lambda óptimo y el entrenamiento del modelo. El modelo oMHN tarda más que cMHN, aunque la diferencia es mínima (décimas de segundo). Además, existe la posibilidad de utilizar CUDA, la GPU de Nvidia que aumenta la velocidad de cálculo, pero no hemos podido comprobarlo por las características de nuestros ordenadores. Entre las desventajas se encuentra que el módulo parece tener problemas con datos relativamente pequeños a la hora de calcular el lambda óptimo y que la instalación de CUDA es muy, muy complicada.

En cuanto al paquete de evamtools, se pueden diferenciar la web y la librería de R. Empezando por la web, proporciona una interfaz gráfica para una rápida construcción, manipulación y exploración de estos modelos. Permite cargar datos, editarlos o introducir frecuencias manualmente. Bajo opciones avanzadas se pueden modificar algunos parámetros, aunque para MHN solo interesa la modificación del valor de lambda. Así se generan gráficos y matrices descargables en formato RDS que se pueden importar a R para análisis posteriores. Es la herramienta más lenta de las tres, sirviendo solo para pequeños análisis, ya que a partir de los 7 genes muestra un aviso. 

Finalmente, la librería de R es la herramienta más completa y versátil. Permite calcular varios modelos de acumulación de eventos, aunque nosotros nos hayamos centrado en MHN. Entre las funciones destacadas se encuentran:
- Evam: para ajustar modelos
- Random_evam: para la generación de modelos aleatorios
- Plot_evam: para la visualización gráfica
- runShiny: acceso a la web app desde R.

No incluye la última versión de MHN corregida por el sesgo del colisionador, pero la salida es más detallada que en el módulo de Python, presentando, además de la matriz Theta, la matriz exponencial, la matriz de transición, la frecuencia de los genotipos predichos, el orden de los genotipos, etc. Otra ventaja es que tiene muchos parámetros customizables por el usuario, tanto en la computación del modelo como en su representación gráfica. 
