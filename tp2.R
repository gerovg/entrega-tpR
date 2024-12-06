###TRABAJO PRACTICO 2



#1- Deberán seleccionar un dataset (puede ser público o de sus trabajos) que contenga como mínimo 50.000 filas y 10 columnas con variedad en sus tipos, por ej: numéricas, fecha, categóricas, etc.

# ELEGÍ EL DATA SET "ACV PREDICTION"

#2- Una vez obtenido el dataset, la idea es que planteen 4 o 5 preguntas de interés sobre el mismo para poder responderlas posteriormente.

# PREGUNTAS:

# A- ¿ Existe una diferencia de ACV de acuerdo al género?
# B- ¿ Las enfermedades crónicas como la HTA aumentan el riesgo de ACV?
# C- ¿ Hay algun rubro de que se asocie más a padecer un ACV?
# D- ¿ Estar/haber estado casado genera un aumento de probabilidad de un ACV?
# E- ¿ Qué porcentaje de fumadores vs no fumadores padecieron ACV?

#3- Sera necesario que carguen el archivo y realicen un chequeo/limpieza de los datos, es decir, verificar la integridad de los mismos.

#4- Realizar un análisis exploratorio de datos considerando cuestiones generales como, por ejemplo: tipos de datos, distribuciones, valores atípicos, correlación entre las variables, etc.

#5- Responder las preguntas planteadas en el punto 2 mediante el uso de tablas de contingencia y gráficos relevantes. (Serán necesarios como mínimo 5 gráficos, 3 de ellos distintos).

# ---------------------------------------------------

#3-

#---------------------------
# Librerias para instalar:

install.packages("tidyverse")
install.packages("ggcorrplot")


# Librerias para cargar: 
library(tidyverse) # Chequear el error que sale
#install.packages("installr")
#library(installr)
#updateR()
#version
library(ggplot2)
library(ggcorrplot)

# Actualizar  
update.packages("ggplot2")
update.packages("ggcorrplot")
update.packages("tidyverse")
#---------------------------

# Cargar el dataset de ACV
dataset <- read.csv("C:/Users/Ramiro/Desktop/Gero curso R/U1/tp1/Trabajo Práctico 2/5_ACV_prediction.csv",
                    stringsAsFactors = TRUE)

# Ver fx para explorar datos clase 5

head(dataset) # Ver filas del dataset

# Chequear integridad de los datos:
summary(dataset)  # Resumen estadístico
str(dataset)      # Estructura del dataset
any(duplicated(dataset))  # Verificar duplicados

# Verificar valores faltantes
colSums(is.na(dataset))  # Conteo de NA por columna
data <- na.omit(dataset) # Eliminar las filas con NA

# Análisis de tipos de datos
print(sapply(dataset, class))

# Histogramas para evaluar distribuciones
dataset %>% 
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ key, scales = "free_x")

# Boxplot para valores atípicos
dataset %>%
  gather() %>%
  ggplot(aes(x = key, y = value)) +
  geom_boxplot() +
  coord_flip()

# Matriz de correlación
correlation <- cor(select_if(dataset, is.numeric), use = "complete.obs")
ggcorrplot(correlation, lab = TRUE)

#4- PREGUNTA A: ¿ Existe una diferencia de ACV de acuerdo al género?

# Convertir las columnas relevantes en factores
data$gender <- as.factor(data$gender)
data$stroke <- as.factor(data$stroke)

# Tabla de contingencia: género vs ACV
gender_stroke_table <- prop.table(table(data$gender, data$stroke), margin = 1) * 100
print(gender_stroke_table)

# Graficar
gender_stroke_df <- as.data.frame(gender_stroke_table)
colnames(gender_stroke_df) <- c("Gender", "Stroke", "Percentage")

ggplot(gender_stroke_df, aes(x = Gender, y = Percentage, fill = Stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de ACV por Género (%)",
       x = "Género", y = "Porcentaje") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No ACV", "ACV")) +
  theme_minimal()

# PREGUNTA B: ¿ Las enfermedades crónicas como la HTA aumentan el riesgo de ACV?

# Convertir la columna HTA a factor
data$hypertension <- as.factor(data$hypertension)

# Tabla de contingencia: HTA vs ACV
hypertension_stroke_table <- prop.table(table(data$hypertension, data$stroke), margin = 1) * 100
print(hypertension_stroke_table)

# Graficar
hypertension_stroke_df <- as.data.frame(hypertension_stroke_table)
colnames(hypertension_stroke_df) <- c("Hypertension", "Stroke", "Percentage")

ggplot(hypertension_stroke_df, aes(x = Hypertension, y = Percentage, fill = Stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de ACV según Hipertensión (%)",
       x = "Hipertensión (0 = No, 1 = Sí)", y = "Porcentaje") +
  scale_fill_manual(values = c("green", "yellow"), labels = c("No ACV", "ACV")) +
  theme_minimal()

# PREGUNTA C:¿ Hay algun tipo de trabajo de que se asocie más a padecer un ACV?

# Convertir la columna work_type a factor
data$work_type <- as.factor(data$work_type)

# Tabla de contingencia: tipo de trabajo vs ACV
work_type_stroke_table <- prop.table(table(data$work_type, data$stroke), margin = 1) * 100
print(work_type_stroke_table)

# Graficar
work_type_stroke_df <- as.data.frame(work_type_stroke_table)
colnames(work_type_stroke_df) <- c("WorkType", "Stroke", "Percentage")

ggplot(work_type_stroke_df, aes(x = WorkType, y = Percentage, fill = Stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de ACV según Tipo de Trabajo (%)",
       x = "Tipo de Trabajo", y = "Porcentaje") +
  scale_fill_manual(values = c("purple", "black"), labels = c("No ACV", "ACV")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PREGUNTA D: ¿ Estar/haber estado casado genera un aumento de probabilidad de un ACV?

# Convertir la columna ever_married a factor
data$ever_married <- as.factor(data$ever_married)

# Tabla de contingencia: estado civil vs ACV
married_stroke_table <- prop.table(table(data$ever_married, data$stroke), margin = 1) * 100
print(married_stroke_table)

# Graficar
married_stroke_df <- as.data.frame(married_stroke_table)
colnames(married_stroke_df) <- c("MaritalStatus", "Stroke", "Percentage")

ggplot(married_stroke_df, aes(x = MaritalStatus, y = Percentage, fill = Stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de ACV según Estado Civil (%)",
       x = "Estado Civil (No = Nunca Casado, Yes = Casado o Estuvo Casado)", 
       y = "Porcentaje") +
  scale_fill_manual(values = c("red", "grey"), labels = c("No ACV", "ACV")) +
  theme_minimal()

# PREGUNTA E: ¿ Qué porcentaje de fumadores vs no fumadores padecieron ACV?

# Convertir la columna smoking_status a factor
data$smoking_status <- as.factor(data$smoking_status)

# Tabla de contingencia: TBQ vs ACV
smoking_stroke_table <- prop.table(table(data$smoking_status, data$stroke), margin = 1) * 100
print(smoking_stroke_table)

# Graficar
smoking_stroke_df <- as.data.frame(smoking_stroke_table)
colnames(smoking_stroke_df) <- c("SmokingStatus", "Stroke", "Percentage")

ggplot(smoking_stroke_df, aes(x = SmokingStatus, y = Percentage, fill = Stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de ACV según Estado de Tabaquismo (%)",
       x = "Estado de Tabaquismo", y = "Porcentaje") +
  scale_fill_manual(values = c("orange", "brown"), labels = c("No ACV", "ACV")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RESPUESTAS DE LAS PREGUNTAS:
# A- ¿ Existe una diferencia de ACV de acuerdo al género?
# Podemos observar en la gráfica que hay un leve aumento de casos para los varones en comparación a las mujeres que han padecido un ACV.
# B- ¿ Las enfermedades crónicas como la HTA aumentan el riesgo de ACV?
# Aquí, se observa que hay un mayor aumento de casos que tuvieron un ACV acompañado de una HTA previa diagnosticada.
# C- ¿ Hay algun rubro de que se asocie más a padecer un ACV?
# Analizando el tipo de trabajo realizado vemos que los autoempleados, es decír, sus propios jefes, son más propensos a tener un ACV en comparación a otros rubros.
# D- ¿ Estar/haber estado casado genera un aumento de probabilidad de un ACV?
# Llamativamente (o no) estar o haber estado casado aumentaria el riesgo a tener un ACV.
# E- ¿ Qué porcentaje de fumadores vs no fumadores padecieron ACV?
# Quienes han fumado y dejaron y quienes son tabaquistas tienen mayor riesgo a tener un ACV frente a quienes no (aunque la diferencia es minima).

# ------------------------- The END ? ----------------------


