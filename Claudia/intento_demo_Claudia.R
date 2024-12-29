rm(list=ls())

#Reinterpretación demo en python
data <- read.table('BRCA_ba_s.csv', header = TRUE, sep = ',')

#Funciones de evamtools
help(package = 'evamtools')

#Crear matrix
mi_matrix <- as.matrix(data)

#Frecuencia eventos
apply(mi_matrix, 2, sum)/nrow(mi_matrix)

#Nº eventos de las observaciones
table(apply(mi_matrix, 1, sum))

#Correr MHN
#Podemos modificar lambda y el nº cores
MHN_list <- evam(data, paths_max = FALSE)

#Matriz de transicion que aparece en Shiny!!
MHN_list$MHN_theta
MHN_list$MHN_exp_theta #Si calculamos su neperiano nos da theta

#Esta no se para que sirve
MHN_list$MHN_trans_rate_mat

#Genotypes predicted by the MHN model (under a model where sampling times 
#are distributed as an exponential of rate 1).
MHN_list$MHN_predicted_genotype_freqs

#Nº fods?
#Nº steps?
#La lambda que se emplea es directamente la que indicamos en evam?


#Realizar los gráficos
?plot_evam
plot_evam(MHN_list, methods = c('MHN'), plot_type = 'trans_mat',
          label_type = 'genotype')     ## Grafico de abajo locura


#Simular datos bajo modelo específico
#Theta matrix
random_mhn <- random_evam(gene_names=c("TP53", "ATP2B2", "PIK3CA", "PNPLA3", "RB1", "TRIM6"),
            model=c('MHN'))


#Generar los datos
?sample_evam
sample_mhn <- sample_evam(random_mhn, N = 81, obs_noise = 0.05, genotype_freqs_as_data = TRUE)

sample_mhn$MHN_sampled_genotype_counts #cuanta gente en cada genotipo
sample_mhn$MHN_sampled_genotype_counts_as_data #matriz binaria

#Histograma con genotipos
plot_evam(samples = sample_mhn, plot_type="obs_genotype_transitions"
          label_type = 'genotype')  #Error, no entiendo "Output from a call to 
                                    #sample_evam. Necessary if you request plot 
                                    #type transitions."




#No encuentro ninguna función para recrear orden eventos
#Ni predecir cuál es el próximo evento más probable
#Ambas funcionalidades sí las tiene python











