Máster en Bioinformática y Biología Computacional - PRSTR 2024/25 - Grupo 6
# Mutual Hazard Networks en Python y R
En este trabajo hemos analizado las herramientas disponibles en Python y R para la construcción de modelos de progresión de cáncer y acumulación de eventos, con un enfoque especial a los Mutual Hazard Networks.

Se aporta:
- Carpeta Informe:
	- Fichero .Rnw con el informe redactado
	- Fichero .bib con la bibliografía utilizada 
	- PDF compilado
	- Carpeta config con el estilo del Rnw para la compilación del PDF
- Carpeta Datos:
	- tinydata.csv
	- BRCA_ba_s.csv
	- LUAD_500.csv
	- LUAD_n12.csv
- Carpeta Ficheros:
	- ModelContructor_oMHN_def.R: implementación de oMHN en evamtools utilizando la teoría matemática del artículo de Schill,2024
	- ModelContructor_oMHN_py.R: implementación de oMHN en evamtools utilizando como inspiración la implementación que hay en el módulo mhn de Python
	- Reticulate_demo_def.Rmd: uso del paquete de Reticulate de R para combinar y correr el código de Python del paquete mhn.
	- Test_ModelContructor.R: pruebas realizadas para probar ModelContructor_oMHN_def.R
	- Evamtools.Rmd: pruebas realizadas con la librería evamtools y varias visualizaciones