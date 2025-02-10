
### RUTINA DE R CON LOS CÓDIGOS PARA OBTENER CADA FIGURA DE LA TESIS Y SU POSTERIOR ANÁLISIS ESTADÍSTICO ####

###############                                      ###############
###############            FIGURAS EXTRA             ###############
###############                                      ###############

##### HISTOGRAMA DE DURACIÓN DE LA FASE C1 DE LAS RESPUESTAS DE ESCAPE #####

# Cargar librerías
library(readxl)
library(ggplot2)
library(dplyr)

# Cargar el archivo Excel
file_path <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Aislamiento Parcial.xlsx"
datos <- read_excel(file_path)

# Convertir la duración en frames a milisegundos
datos <- datos %>%
  mutate(Duracion_ms = (`Duracion en frames` / 437) * 1000)

# Contar la cantidad de respuestas para cada valor de Duracion_ms
datos_resumidos <- datos %>%
  group_by(Duracion_ms) %>%
  summarise(Respuestas = n())

# Crear el gráfico de línea suavizada con los datos resumidos
ggplot(datos_resumidos, aes(x = Duracion_ms, y = Respuestas)) +
  geom_smooth(color = "blue", size = 1.5, method = "loess", span = 0.25, se = FALSE) +
  labs(
    x = "Duración (ms)",
    y = "Respuestas"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),         # Líneas negras en los ejes
    axis.ticks = element_line(color = "black"),        # Líneas negras en las marcas de los ejes
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),                # Elimina las líneas de cuadrícula principales
    panel.grid.minor = element_blank()                 # Elimina las líneas de cuadrícula secundarias
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(breaks = seq(0, 45, by = 4), expand = expansion(mult = c(0, 0))) +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 100))   # Asegura que el eje Y comience en cero


###############                                                  ###############
###############             EXPERIMENTO ARENA SOCIAL             ###############
###############                                                  ###############

###############                                      ###############
###############              5 MINUTOS               ###############
###############                                      ###############

# En este caso el pez focal está solo, no hay peces en la CS (cámara social).


#####                                                     #####
##### ÍNDICE DE PREFERENCIA VISUAL SOCIAL (IPV) GENERAL   #####
#####                                                     #####

library(ggplot2)
library(readxl)

ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Social.xlsx"
datos <- read_excel(ruta_archivo, sheet = "IPV 5 min")

# Crear el violin plot
ggplot(datos, aes(x = Condicion, y = IPV, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Para mostrar la media
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Línea horizontal en y = 0
  labs(
    x = "Condición del Individuo",
    y = "IPV") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "#FFB3BA", "S" = "#FFDFBA")) +  # Colores pastel
  theme(
    axis.title = element_text(size = 16),  # Tamaño del título de los ejes
    axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
    axis.line = element_line(size = 0.5, colour = "black")  # Línea negra en los ejes con grosor de 1
  )

# Resumen estadístico de los datos
resumen <- datos %>%
  group_by(Condicion) %>%  # Agrupar por condición
  summarise(
    Media = mean(IPV, na.rm = TRUE),               # Media
    Desviacion_Estandar = sd(IPV, na.rm = TRUE),   # Desvío estándar
    Minimo = min(IPV, na.rm = TRUE),                # Mínimo
    Maximo = max(IPV, na.rm = TRUE),                # Máximo
    Mediana = median(IPV, na.rm = TRUE),            # Mediana
    Q1 = quantile(IPV, 0.25, na.rm = TRUE),        # Primer cuartil
    Q3 = quantile(IPV, 0.75, na.rm = TRUE),         # Tercer cuartil
    n = n(),  # Número de observaciones
    Error_Estandar = Desviacion_Estandar / sqrt(n)
  )

# Mostrar el resumen
print(resumen)

# Para ver si hay diferencias significativas para el grafico general

# Filtrar los datos para los grupos Aislado y Social
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Realizar el test t de Student para comparar las medias
resultado_test <- t.test(IPV ~ Condicion, data = datos_filtrados)

# Mostrar los resultados del test
print(resultado_test)

#### PRUEBA PARA VER SI ES DISTINTO DE CERO #######

# Prueba t para verificar si los valores de IPV son significativamente distintos de cero en cada grupo

# Crear una función para realizar la prueba t por grupo
prueba_t_distinto_de_cero <- function(grupo) {
  t.test(grupo$IPV, mu = 0)
}

# Aplicar la prueba t para cada grupo (A y S)
resultado_A <- prueba_t_distinto_de_cero(filter(datos, Condicion == "A"))
resultado_S <- prueba_t_distinto_de_cero(filter(datos, Condicion == "S"))

# Mostrar los resultados
cat("Resultados para el grupo A:\n")
print(resultado_A)

cat("\nResultados para el grupo S:\n")
print(resultado_S)


#####                                                     #####
#####      DISTANCIA RECORRIDA (mm) GENERAL A VS S        #####
#####                                                     #####


# Cargar las librerías necesarias
library(ggplot2)
library(readxl)

# Leer los datos desde el archivo Excel
ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Social.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Hoja5")

# Crear el violin plot para la Distancia (mm) por Condición
ggplot(datos, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE) +  # Crear el violin plot sin recortar
  geom_jitter(width = 0.2, alpha = 0.5) +  # Puntos de dispersión para cada observación
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Media en rojo
  labs(
    x = "Condición",
    y = "Distancia (mm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "#66c2a5", "S" = "#3288bd")) +  # Colores personalizados para cada condición
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.5),  # Ejes en negro
    panel.border = element_blank()  # Eliminar el borde del panel
  )

### RESUMEN CON TEST DE T Y VARIABLES #####

# Cargar las librerías necesarias
library(dplyr)
library(readxl)

# Leer los datos desde el archivo Excel
ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Azar/Analisis de los datos -Tesis/Social/5 min/Secciones.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Distancia 5 min")

# Calcular el resumen estadístico
resumen <- datos %>%
  group_by(Condicion) %>%
  summarise(
    Media = mean(`Distancia (mm)`, na.rm = TRUE),
    Error_Estandar = sd(`Distancia (mm)`, na.rm = TRUE) / sqrt(n()),
    IC_Lower = Media - qt(0.975, df = n() - 1) * Error_Estandar,  # Límite inferior del IC
    IC_Upper = Media + qt(0.975, df = n() - 1) * Error_Estandar   # Límite superior del IC
  )

# Mostrar el resumen
print(resumen)

# Test t de Student para comparar la distancia entre las condiciones A y S
# Asegurarse de que "Condicion" esté en formato de factor, para que t.test entienda las categorías
datos$Condicion <- as.factor(datos$Condicion)

# Realizar el test t de Student
resultado_test <- t.test(`Distancia (mm)` ~ Condicion, data = datos, var.equal = FALSE)

# Mostrar el resultado del test
print(resultado_test)

#####                                                      #####
#####      DISTANCIA RECORRIDA (mm) x SEMANA A VS S        #####
#####                                                      #####

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(readxl)

# Leer los datos desde el archivo Excel
ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Azar/Analisis de los datos -Tesis/Social/5 min/Secciones.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Distancia 5 min")

# Asegurarse de que "Condicion" y "Semana" estén en formato de factor para que ggplot los trate como categorías
datos$Condicion <- as.factor(datos$Condicion)
datos$Semana <- as.factor(datos$Semana)

# Crear el violin plot
ggplot(datos, aes(x = Semana, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.4, size = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 2, color = "black", position = position_dodge(width = 0.8)) +
  labs(
    x = "Semana",
    y = "Distancia (mm)",
    fill = "Condición"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "#FFB3BA", "S" = "#FFDFBA")) +  # Colores personalizados
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5, colour = "black")  # Marcar ejes en negro
  )

#### RESUMEN CON VARIABLES Y TEST DE T #####

resumen_semana <- datos %>%
  group_by(Condicion, Semana) %>%
  summarise(
    Media = mean(`Distancia (mm)`, na.rm = TRUE),
    Error_Estandar = sd(`Distancia (mm)`, na.rm = TRUE) / sqrt(n()),
    IC_Lower = Media - qt(0.975, df = n() - 1) * Error_Estandar,  # Límite inferior del IC
    IC_Upper = Media + qt(0.975, df = n() - 1) * Error_Estandar,   # Límite superior del IC
    n = n()
  )

# Mostrar el resumen
print(resumen_semana)

# Crear una lista vacía para guardar los resultados de cada semana
resultados_test <- list()

# Realizar el test t para cada semana
for (semana in unique(datos$Semana)) {
  datos_semana <- filter(datos, Semana == semana)
  
  resultado_test <- t.test(`Distancia (mm)` ~ Condicion, data = datos_semana, var.equal = FALSE)
  resultados_test[[paste("Semana", semana)]] <- resultado_test
}

# Mostrar los resultados del test t para cada semana
resultados_test


####### ANOVA DE 2 FACTORES ##################

# Realizar un ANOVA de dos factores
modelo_anova <- aov(`Distancia (mm)` ~ Condicion * Semana, data = datos)

# Ver el resumen del modelo
summary(modelo_anova)


###############                                      ###############
###############             10 MINUTOS               ###############
###############                                      ###############

# En este caso hay peces en la CS (cámara social).

#####                                                     #####
##### ÍNDICE DE PREFERENCIA VISUAL SOCIAL (IPV) GENERAL   #####
#####                                                     #####

library(ggplot2)
library(readxl)
library(dplyr)

ruta_archivo <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Social.xlsx"
datos <- read_excel(ruta_archivo, sheet = "IPV 10 min")

# Filtra los datos para incluir solo las condiciones A y S
datos_filtrados <- datos %>% filter(Condicion %in% c("A", "S"))

# Crear el violin plot
ggplot(datos_filtrados, aes(x = Condicion, y = IPV, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Para mostrar la media
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Línea horizontal en y = 0
  labs(
    x = "Condition",
    y = "IPV") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "#FFB3BA", "S" = "#FFDFBA")) +  # Solo colores para A y S
  theme(
    axis.title = element_text(size = 16),  # Tamaño del título de los ejes
    axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
    axis.line = element_line(size = 0.5, colour = "black")  # Línea negra en los ejes con grosor de 1
  )

# Resumen estadístico de los datos
resumen <- datos %>%
  group_by(Condicion) %>%  # Agrupar por condición
  summarise(
    Media = mean(IPV, na.rm = TRUE),               # Media
    Desviacion_Estandar = sd(IPV, na.rm = TRUE),   # Desvío estándar
    Error_Estandar = Desviacion_Estandar / sqrt(sum(!is.na(IPV))), # Error estándar
    Minimo = min(IPV, na.rm = TRUE),                # Mínimo
    Maximo = max(IPV, na.rm = TRUE),                # Máximo
    Mediana = median(IPV, na.rm = TRUE),            # Mediana
    Q1 = quantile(IPV, 0.25, na.rm = TRUE),        # Primer cuartil
    Q3 = quantile(IPV, 0.75, na.rm = TRUE),         # Tercer cuartil
    n = sum(!is.na(IPV))  # Número de observaciones no NA
  )

# Mostrar el resumen
print(resumen)

##### TEST PARA VER SI ES DISTINTO DE CERO ###############

# Asegúrate de que dplyr y ggplot2 estén cargados
library(dplyr)

# Filtra los datos para las condiciones A y S
datos_filtrados <- datos %>% filter(Condicion %in% c("A", "S"))

# Test t para cada grupo (A y S) comparando la media con cero
test_A <- t.test(datos_filtrados$IPV[datos_filtrados$Condicion == "A"], mu = 0)
test_S <- t.test(datos_filtrados$IPV[datos_filtrados$Condicion == "S"], mu = 0)

# Mostrar los resultados de los tests
print("Test para el grupo A:")
print(test_A)

print("Test para el grupo S:")
print(test_S)

##### TEST DE T PARA VER SI A vs S tienen diferencias significativas ###############

# Filtrar los datos para los grupos Aislado y Social
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Realizar el test t de Student para comparar las medias
resultado_test <- t.test(IPV ~ Condicion, data = datos_filtrados)

# Mostrar los resultados del test
print(resultado_test)


#####                                                      #####
##### ÍNDICE DE PREFERENCIA VISUAL SOCIAL (IPV) x SEMANA   #####
#####                                                      #####

# Filtrar datos para solo incluir Aislado y Social
datos_filtrados <- subset(datos, Condicion %in% c("A", "S"))

# Crear el violin plot
ggplot(datos_filtrados, aes(x = Condicion, y = IPV, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Para mostrar la media
  facet_wrap(~ Semana) +  # Para separar por semanas
  labs(
    x = "Condition",
    y = "IPV") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),  # Tamaño del título de los ejes
    axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
    axis.line = element_line(size = 0.5, colour = "black")  # Línea negra en los ejes
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)  # Línea en el 0


# Resumen estadístico desagregado por semana y condición
resumen_por_semanas <- datos %>%
  filter(Condicion %in% c("A", "S")) %>%  # Incluir solo las condiciones I y S
  group_by(Semana, Condicion) %>%  # Agrupar por Semana y Condición
  summarise(
    Media = mean(IPV, na.rm = TRUE),               # Media
    Desviacion_Estandar = sd(IPV, na.rm = TRUE),   # Desviación estándar
    Error_Estandar = Desviacion_Estandar / sqrt(sum(!is.na(IPV))), # Error estándar
    Minimo = min(IPV, na.rm = TRUE),                # Mínimo
    Maximo = max(IPV, na.rm = TRUE),                # Máximo
    Mediana = median(IPV, na.rm = TRUE),            # Mediana
    Q1 = quantile(IPV, 0.25, na.rm = TRUE),        # Primer cuartil
    Q3 = quantile(IPV, 0.75, na.rm = TRUE),         # Tercer cuartil
    n = sum(!is.na(IPV))  # Número de observaciones no NA
  )

# Mostrar el resumen
print(resumen_por_semanas)


################################### ANOVA DE 2 FACTORES  ##############################

# Asegúrate de que las variables "Condicion" y "Semana" son factores
datos$Condicion <- as.factor(datos$Condicion)
datos$Semana <- as.factor(datos$Semana)

# Realizar el ANOVA de 2 factores
anova_resultado <- aov(IPV ~ Condicion * Semana, data = datos)

# Ver los resultados del ANOVA
summary(anova_resultado)

# Ver los resultados de los efectos principales y la interacción
anova_resultado

################################### CONTRASTES DE TUKEY ##############################

# Este análisis es para chequear si hay diferencias entre los Aislados-Sociales, Aislados-Parcialmente aislados, Sociales-Parcialmente aislados.

# Realizar el ANOVA de 2 factores (ya lo tienes)
anova_resultado <- aov(IPV ~ Condicion * Semana, data = datos)

# Aplicar el test de Tukey para los efectos principales (Condición)
tukey_resultado <- TukeyHSD(anova_resultado, "Condicion")

# Ver los resultados de los contrastes de Tukey
print(tukey_resultado)

# Si deseas guardar los resultados en un archivo, puedes hacerlo así:
write.table(tukey_resultado$Condicion, file = "tukey_contrastes_condicion.txt", sep = "\t", quote = FALSE, row.names = TRUE)

################################### CONTRASTES POR SEMANA ##############################

# Si quieres hacer los contrastes entre condiciones dentro de cada semana, necesitas dividir los datos.
# Esto se hace realizando ANOVA separados para cada semana.

library(dplyr)

# Dividir por semanas y realizar ANOVA + Tukey para cada grupo
resultados_por_semana <- datos %>%
  filter(Condicion %in% c("A", "S")) %>%
  group_by(Semana) %>%
  group_map(~ {
    anova_semana <- aov(IPV ~ Condicion, data = .x)
    TukeyHSD(anova_semana)
  })

# Mostrar resultados de Tukey por semana
for (i in seq_along(resultados_por_semana)) {
  cat("\nResultados de Tukey para Semana", unique(datos$Semana)[i], ":\n")
  print(resultados_por_semana[[i]])
}

####################### Test para ver si alguna semana es distinto de cero ########################

# Asegúrate de que dplyr esté cargado
library(dplyr)

# Filtrar los datos para las condiciones I y S
datos_filtrados <- datos %>% filter(Condicion %in% c("A", "S"))

# Realizar un test t para cada semana y cada condición
resultados_tests <- datos_filtrados %>%
  group_by(Semana, Condicion) %>%  # Agrupar por Semana y Condición
  summarise(
    test_resultado = list(t.test(IPV, mu = 0)),  # Realizar el test t comparando IPV con cero
    .groups = "drop"
  )

# Mostrar los resultados de los tests
resultados_tests %>%
  mutate(
    p_value = sapply(test_resultado, function(x) x$p.value),  # Extraer el p-value de cada test
    estadistico_t = sapply(test_resultado, function(x) x$statistic),  # Extraer el estadístico t de cada test
    conf_int_lower = sapply(test_resultado, function(x) x$conf.int[1]),  # Intervalo de confianza inferior
    conf_int_upper = sapply(test_resultado, function(x) x$conf.int[2])   # Intervalo de confianza superior
  ) %>%
  select(Semana, Condicion, p_value, estadistico_t, conf_int_lower, conf_int_upper)  # Seleccionar columnas relevantes



#####                                                     #####
#####      DISTANCIA RECORRIDA (mm) GENERAL A VS S        #####
#####                                                     #####

# Cargar el paquete
library(readxl)
library(ggplot2)
library(dplyr)

# Leer los datos del archivo Excel
ruta_archivo <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Social.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Distancia 10 min")

# Verifica que los datos se cargaron correctamente
head(datos)

# Convertir Condicion a factor y Distancia_al_vidrio a número
datos$Condicion <- as.factor(datos$Condicion)
datos$Distancia_al_vidrio <- as.numeric(datos$Distancia_al_vidrio)
datos$`Distancia (mm)` <- as.numeric(datos$`Distancia (mm)`)
datos$`Distancia (cm)` <- as.numeric(datos$`Distancia (cm)`)

# Filtrar los datos para incluir solo las condiciones A y S
datos_filtrados <- datos %>% filter(Condicion %in% c("A", "S", "AP"))

# Crear el violin plot solo con las condiciones A y S
ggplot(datos_filtrados, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Para mostrar la media
  labs(
    x = "Condición",
    y = "Distancia (mm)") +
  theme_minimal() +  # Estilo minimalista
  theme(axis.title = element_text(size = 16),  # Tamaño del título de los ejes
        axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
        axis.line = element_line(color = "black", size = 0.5))  # Líneas negras en los ejes

################## RESUMEN CON VARIABLES Y LOS P-VALORES ##################################

# Resumir los datos: Calcular el promedio, desvío estándar, mediana y tamaño de muestra por condición
data_summary <- datos_filtrados %>%
  group_by(Condicion) %>%
  summarise(
    Promedio_Distancia = mean(`Distancia (mm)`, na.rm = TRUE),  # Promedio de distancia
    Mediana_Distancia = median(`Distancia (mm)`, na.rm = TRUE),  # Mediana de distancia
    SD_Distancia = sd(`Distancia (mm)`, na.rm = TRUE),          # Desvío estándar de distancia
    N = n(),                                                    # Tamaño de la muestra (número de observaciones)
    SE = sd(`Distancia (mm)`, na.rm = TRUE) / sqrt(N)
  )

# Mostrar el resumen
print(data_summary)

# Realizar un test t para comparar las condiciones A y S
test_t_result <- t.test(`Distancia (mm)` ~ Condicion, data = datos_filtrados)

# Mostrar resultados del test t
print(test_t_result)

##################### ANOVA DE 2 FACTORES PARA LA DISTANCIA A LA BARRERA POR SEMANA #####################

# Realizar ANOVA de 2 factores
anova_resultado <- aov(`Distancia (mm)` ~ Semana * Condicion, data = datos_filtrados)

# Resumen de los resultados del ANOVA
summary(anova_resultado)


######################### GRAFICO DE LA DISTANCIA A LA BARRERA POR SEMANAS ########################

# Filtrar los datos para los grupos Aislado y Social
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Realizar el test t de Student para comparar las medias
resultado_test <- t.test(`Distancia (mm)` ~ Condicion, data = datos_filtrados)

# Mostrar los resultados del test
print(resultado_test)

#####                                                      #####
#####      DISTANCIA RECORRIDA (mm) x SEMANA A VS S        #####
#####                                                      #####

# Filtrar datos para solo incluir Aislado y Social
datos_filtrados <- subset(datos, Condicion %in% c("A", "S"))

# Crear el violin plot
ggplot(datos_filtrados, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +  # Para mostrar la media
  facet_wrap(~ Semana) +  # Para separar por semanas
  labs(
    x = "Condición",
    y = "Distancia (mm)") +
  theme_minimal() +  # Estilo minimalista
  theme(axis.title = element_text(size = 16),  # Tamaño del título de los ejes
        axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
        axis.line = element_line(color = "black", size = 0.5))  # Líneas negras en los ejes

################ RESUMEN CON VARIABLES PARA CADA SEMANA Y LOS P-VALORES CORRESPONDIENTES ##########################

# Diferencias significativas

# Lista para almacenar los resultados de los tests t
resultados_ttest <- list()

# Realizar un test t por cada semana
for (semana in c("2", "3", "4")) {
  
  # Filtrar los datos para la semana actual y las condiciones Aislado y Social
  datos_filtrados <- datos %>% 
    filter(Semana == semana & Condicion %in% c("A", "S"))
  
  # Realizar el test t para comparar Aislado y Social en la semana actual
  ttest_result <- t.test(`Distancia (mm)` ~ Condicion, data = datos_filtrados, var.equal = TRUE)
  
  # Almacenar los resultados
  resultados_ttest[[paste("Semana", semana)]] <- ttest_result
}

# Mostrar los resultados del test t para cada semana
for (semana in c("2", "3", "4")) {
  print(paste("Resultados del test t para la Semana", semana))
  print(resultados_ttest[[paste("Semana", semana)]])
  cat("\n")  # Añadir una línea en blanco entre resultados
}


# Crear un resumen para cada semana y condición
resumen <- datos %>%
  group_by(Semana, Condicion) %>%
  summarise(
    Media = mean(`Distancia (mm)`, na.rm = TRUE),
    Desvio_Estandar = sd(`Distancia (mm)`, na.rm = TRUE),
    IC_Lower = Media - qt(0.975, df = n() - 1) * (Desvio_Estandar / sqrt(n())),
    IC_Upper = Media + qt(0.975, df = n() - 1) * (Desvio_Estandar / sqrt(n())),
    .groups = 'drop',  # Para evitar advertencias sobre agrupamientos
    N = n()
  )

# Mostrar el resumen
print(resumen)



###############                          ###############
###############   EXPERIMENTO DE ESCAPE  ###############
###############                          ###############

#####                                                     #####
##### DIAGRAMA ALUVIAL o SANKEY DE LOS TIPOS DE RESPUESTA #####
#####                                                     #####

# Para este caso solo se contaron los trials, cada individuo fue testeado en 8 trials, por lo que 
# de 100 individuos nosotros vamos a tener 800 trials. A partir de estos observamos si los individuos
# respondian o no. Si respondian luego los clasificamos en el tipo de respuesta: C-start o freezing. Luego
# nos fijamos cuantos de los C-start eran rápidos o lentos. Ojo, puede que hayan algunos animales que 
# hayan hecho freezing, c-star rápido y c-start lento.


#####          ##### 
##### SOCIALES #####
#####          #####

# Ejemplo: de la barra que dice "Total" salen dos ramas. Por eso en source ponemos dos veces "Total". 
# Si pones "Total" al principio de source, entonces al principio de "target" tenes que poner las 
# ramas de "Total", el orden tiene que coincidir en cada parametro.

links <- data.frame(
  source=c("Total","Total", "Responde", "Responde", "C_start", "C_start"), 
  target=c("Responde","No responde", "C_start", "Freezing", "Rapidas", "Lentas"), 
  value=c(0.15, 0.85, 0.808, 0.192, 0.79, 0.21)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE,
                   fontSize = 19,   # Cambiar tamaño de la fuente
                   nodeWidth = 30)  # Cambiar ancho del nodo

p

#####          ##### 
##### AISLADOS #####
#####          #####

# Ejemplo: de la barra que dice "Total" salen dos ramas. Por eso en source ponemos dos veces "Total". 
# Si pones "Total" al principio de source, entonces al principio de "target" tenes que poner las 
# ramas de "Total", el orden tiene que coincidir en cada parametro.

links <- data.frame(
  source=c("Total","Total", "Responde", "Responde", "C_start", "C_start"), 
  target=c("Responde","No responde", "C_start", "Freezing", "Rapidas", "Lentas"), 
  value=c(0.17, 0.83, 0.955, 0.045, 0.76, 0.24)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE,
                   fontSize = 19,   # Cambiar tamaño de la fuente
                   nodeWidth = 30)  # Cambiar ancho del nodo

p


#####                                         #####
#####        DISTANCIA RECORRIDA (mm)         #####
#####                                         #####

# En estos análisis vamos a determinar la distancia recorrida en mm por el pez focal en la arena de escape
# durante los 8 segundos previos al estímulo de los ensayos auditivos unisensoriales. La distancia se calculó
# midiendo el led de los videos de escape usando ImageJ (dado que media 1 cm). Luego de una serie de mediciones
# se estableció que en esos videos 1 cm = 47.01 pixeles. 

library(readxl)
library(dplyr)
library(ggplot2)

# Leer el archivo Excel
file_path <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"

data <- read_excel(file_path) 

# Graficar un violin plot de la distancia recorrida por condición con puntos de datos
ggplot(data, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Violin plot con transparencia para visualizar mejor los puntos
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Boxplot dentro del violin plot
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.7) +  # Puntos individuales de datos
  labs(
    x = "Condición",
    y = "Distancia (mm)") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "blue", "S" = "green", "AP" = "red"))

# Utilizado ayer

# Filtrar los datos para incluir solo las condiciones Aislado (A) y Social (S)
data_filtrado <- data %>% filter(Condicion %in% c("A", "S"))

# Realizar un test t entre las condiciones Aislado (A) y Social (S)
test_t_result <- t.test(`Distancia (mm)` ~ Condicion, data = data_filtrado)

# Mostrar los resultados del test t
print(test_t_result)

# Cargar las librerías necesarias
library(dplyr)

# Calcular estadísticas descriptivas para cada condición
resumen_estadisticas <- data %>%
  group_by(Condicion) %>%
  summarise(
    n = n(),  # Número de observaciones
    Media = mean(`Distancia (mm)`, na.rm = TRUE),  # Media
    Desvio = sd(`Distancia (mm)`, na.rm = TRUE),  # Desviación estándar
    Error_Estandar = Desvio / sqrt(n()),  # Error estándar
    IC_Lower = Media - qt(0.975, df = n() - 1) * Error_Estandar,  # Límite inferior del IC
    IC_Upper = Media + qt(0.975, df = n() - 1) * Error_Estandar,  # Límite superior del IC
    .groups = "drop"
  )

# Mostrar el resumen de estadísticas
print(resumen_estadisticas)

########### PARA QUE ME DE LOS INTERVALOS DE CONFIANZA ###########

# Filtrar los datos por cada condición
aislados <- data %>% filter(Condicion == "A")
sociales <- data %>% filter(Condicion == "S")

# Función para calcular el intervalo de confianza del 95% para una muestra
calcular_ic <- function(x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  n <- length(x)
  error_margin <- qt(0.975, df = n - 1) * sd_x / sqrt(n)
  ic_lower <- mean_x - error_margin
  ic_upper <- mean_x + error_margin
  return(c(ic_lower, ic_upper))
}

# Calcular IC para los aislados
ic_aislados <- calcular_ic(aislados$`Distancia (mm)`)

# Calcular IC para los sociales
ic_sociales <- calcular_ic(sociales$`Distancia (mm)`)

# Mostrar los intervalos de confianza
cat("Intervalo de confianza del 95% para Aislados:", ic_aislados, "\n")
cat("Intervalo de confianza del 95% para Sociales:", ic_sociales, "\n")


#####                                         #####
#####    DISTANCIA RECORRIDA (mm) x SEMANA    #####
#####                                         #####

# Filtrar los datos para solo A (Aislado) y S (Social)
data_AS <- data %>% filter(Condicion %in% c("A", "S"))

# Graficar violin plot con puntos de datos, separado por semana
ggplot(data_AS, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Violin plot con transparencia
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Boxplot dentro del violin plot
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.7) +  # Puntos individuales
  labs(title = "Distancia Recorrida por Condición (Aislado vs Social) y Semana",
       x = "Condición",
       y = "Distancia (mm)") +
  facet_wrap(~ Semana) +  # Separar los gráficos por semana
  theme_minimal() +
  scale_fill_manual(values = c("A" = "blue", "S" = "green"))

### Parte estadística ###

# Instalar y cargar librerías necesarias
library(dplyr)
library(tidyr)

# Filtrar los datos para solo A (Aislado) y S (Social)
data_AS <- data %>% filter(Condicion %in% c("A", "S"))

# Calcular estadísticas descriptivas por Condición y Semana
estadisticas <- data_AS %>%
  group_by(Condicion, Semana) %>%
  summarise(
    n = n(),  # Número de observaciones
    media = mean(`Distancia (mm)`, na.rm = TRUE),  # Media
    desv_est = sd(`Distancia (mm)`, na.rm = TRUE),  # Desviación estándar
    error_est = desv_est / sqrt(n),  # Error estándar
    IC_inf = media - qt(0.975, df = n-1) * error_est,  # Límite inferior del IC
    IC_sup = media + qt(0.975, df = n-1) * error_est   # Límite superior del IC
  ) %>%
  ungroup()

# Mostrar el resultado
print(estadisticas)

# Librerías necesarias
library(dplyr)

# Filtrar solo los datos de las condiciones A (Aislado) y S (Social)
data_AS <- data %>% filter(Condicion %in% c("A", "S"))

# Lista para almacenar los resultados del test t
resultados_t_test <- list()

# Realizar el test t para cada semana
for (semana in unique(data_AS$Semana)) {
  
  # Filtrar los datos por semana
  data_semana <- data_AS %>% filter(Semana == semana)
  
  # Realizar el test t entre Aislado y Social
  t_test_result <- t.test(`Distancia (mm)` ~ Condicion, data = data_semana)
  
  # Guardar el resultado en la lista
  resultados_t_test[[paste0("Semana_", semana)]] <- t_test_result
}

# Mostrar los resultados
resultados_t_test

### Anova de 2 factores ###

# Instalar y cargar librerías necesarias
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(readxl)  # Para leer el archivo de Excel
library(ggplot2) # Para visualización
library(dplyr)   # Para manipulación de datos

file_path <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"

data <- read_excel(file_path)

# Asegurarse de que las variables estén en el tipo de dato correcto
datos$Condicion <- as.factor(datos$Condicion)  # Convertir Condicion (A y S) a factor
datos$Semana <- as.factor(datos$Semana)  # Convertir Edad a factor si es discreta
datos$`Distancia (mm)` <- as.numeric(datos$`Distancia (mm)`)  # Convertir la variable respuesta a numérica

# Realizar ANOVA de dos factores con interacción
anova_model <- aov(`Distancia (mm)` ~ Condicion*Semana, data = datos)

# Mostrar resumen del ANOVA
summary(anova_model)

# Cargar la librería multcomp
library(multcomp)

# Realizar el contraste de Tukey para las interacciones entre Condicion y Semana
post_hoc <- glht(anova_model, linfct = mcp(Condicion = "Tukey"))

# Mostrar los resultados
summary(post_hoc)

# Lista para almacenar los resultados de los contrastes de Tukey por semana
resultados_tukey_por_semana <- list()

# Para cada semana, hacer un contraste de Tukey entre Condicion (Aislado vs Social)
for (semana in unique(datos$Semana)) {
  # Filtrar datos para la semana actual
  data_semana <- datos %>% filter(Semana == semana)
  
  # Realizar ANOVA de 2 factores para cada semana
  anova_model_semana <- aov(`Distancia (mm)` ~ Condicion, data = data_semana)
  
  # Realizar contraste de Tukey para la condición
  post_hoc_semana <- glht(anova_model_semana, linfct = mcp(Condicion = "Tukey"))
  
  # Almacenar los resultados
  resultados_tukey_por_semana[[paste0("Semana_", semana)]] <- summary(post_hoc_semana)
}

# Mostrar los resultados
resultados_tukey_por_semana

#####                                                      #####
#####     PROBABILIDAD DE RESPUESTA AISLADOS VS SOCIALES   #####
#####                                                      #####

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

file_path <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"

datos <- read_excel(file_path, sheet = "Probabilidad de escape")

# Filtrar los datos para excluir la condición "CN"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Calcular la proporción de respuestas por individuo (promedio de 8 respuestas por individuo)
proporciones_por_individuo <- datos_filtrados %>%
  group_by(Individuo, Condicion, Cruza) %>%
  summarise(Proporcion_Respuesta = sum(Respuesta) / n(), .groups = "drop")

# Convertir 'Condicion' a factor para el análisis
proporciones_por_individuo <- proporciones_por_individuo %>%
  mutate(Condicion = as.factor(Condicion))

# Resumen por Condición (Aislado vs Social)
resumen_condicion <- proporciones_por_individuo %>%
  group_by(Condicion) %>%
  summarise(
    n = n(),
    mean = mean(Proporcion_Respuesta),
    sd = sd(Proporcion_Respuesta),
    se = sd / sqrt(n)
  )

# Ver el resumen general por condición
print(resumen_condicion)

# Gráfico: Proporción de respuestas por condición (Aislados vs Sociales)
ggplot(proporciones_por_individuo, aes(x = Condicion, y = Proporcion_Respuesta, fill = Condicion)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Condición",
    y = "Proporción de Respuestas") +
  scale_fill_manual(values = c("#FF6666", "#6699FF")) +
  theme_minimal()

# Test estadístico: t-test para comparar las condiciones A vs S
t_test_result <- t.test(Proporcion_Respuesta ~ Condicion, data = proporciones_por_individuo)
print(t_test_result)

#####                                      #####           
##### PROBABILIDAD DE RESPUESTA POR SEMANA #####
#####                                      #####

# Cargar librerías
library(readxl)
library(ggplot2)
library(dplyr)

file_path <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"

datos <- read_excel(file_path, sheet = "Probabilidad de escape")

# Filtrar los datos para excluir la condición "AP"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Calcular la proporción de respuestas por individuo (considerando la cruza)
proporciones_por_individuo <- datos_filtrados %>%
  group_by(Cruza, Individuo, Condicion, Semana) %>%
  summarise(
    Proporcion_Respuesta = sum(Respuesta) / 8,  # Proporción de respuestas por individuo
    .groups = 'drop'
  )

# Verificar tamaño muestral por semana y condición
conteo_por_grupo <- proporciones_por_individuo %>%
  group_by(Semana, Condicion) %>%
  summarise(
    N = n(),  # Número de individuos por semana y condición
    .groups = 'drop'
  )

print(conteo_por_grupo)  # Este debe coincidir con los tamaños muestrales que mencionaste

# Calcular el promedio y el error estándar por semana y condición (sin mostrar cruza)
probabilidades_por_grupo <- proporciones_por_individuo %>%
  group_by(Semana, Condicion) %>%
  summarise(
    Probabilidad_Respuesta = mean(Proporcion_Respuesta),  # Promedio de proporciones
    SE = sd(Proporcion_Respuesta) / sqrt(n()),  # Error estándar
    .groups = 'drop'
  )

# Crear el gráfico de barras con barras de error
ggplot(probabilidades_por_grupo, aes(x = factor(Semana), y = Probabilidad_Respuesta, fill = Condicion)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Crear barras separadas por condición
  geom_errorbar(aes(ymin = Probabilidad_Respuesta - SE, ymax = Probabilidad_Respuesta + SE), 
                position = position_dodge(0.9), width = 0.25) +  # Añadir barras de error
  labs(
    x = "Semana",
    y = "Probabilidad de respuesta"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),   # Líneas negras en los ejes
    panel.grid.major = element_blank(),          # Eliminar líneas de la cuadrícula
    panel.grid.minor = element_blank()           # Eliminar líneas de la cuadrícula menor
  ) +
  scale_fill_manual(values = c("A" = "#FF6666",  # Color rojo más oscuro para condición "Aislado"
                               "S" = "#6699FF")) # Color azul más oscuro para condición "Social"

# Calcular resumen de datos por semana y condición
resumen <- probabilidades_por_grupo %>%
  summarise(
    Semana = Semana,
    Condicion = Condicion,
    N = conteo_por_grupo$N[conteo_por_grupo$Semana == Semana & conteo_por_grupo$Condicion == Condicion],
    Media = Probabilidad_Respuesta,
    SD = SE * sqrt(N),  # Calcular SD a partir del SE
    SE = SE,
    IC_lower = Media - qt(0.975, df = N - 1) * SE,  # Límite inferior del IC
    IC_upper = Media + qt(0.975, df = N - 1) * SE,  # Límite superior del IC
    .groups = 'drop'
  )

# Mostrar resumen
print(resumen)

#################### ANOVA DE 2 FACTORES CON CONTRASTES DE TUKEY ###########################################

install.packages("emmeans")
install.packages("FSA")

# Cargar librerías necesarias
library(dplyr)
library(car)  # Para ANOVA robusta si es necesario
library(emmeans)  # Para pruebas de Tukey

# Realizar ANOVA de dos factores
anova_model <- aov(Proporcion_Respuesta ~ Semana * Condicion, data = proporciones_por_individuo)

# Resumen del ANOVA
summary(anova_model)

# Verificar supuestos del ANOVA
# 1. Normalidad de los residuos
shapiro.test(residuals(anova_model))

# 2. Homogeneidad de varianzas
leveneTest(Proporcion_Respuesta ~ Semana * Condicion, data = proporciones_por_individuo)

# Contrastes a priori con Tukey (post-hoc)
tukey_results <- emmeans(anova_model, pairwise ~ Semana * Condicion, adjust = "tukey")

# Mostrar resultados de Tukey
summary(tukey_results)

# Extraer las comparaciones post-hoc específicas
pairs(tukey_results)


######################### ANOVA CON LAS COMPARACIONES SIMPLES ###########################

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(emmeans)
library(car)  # Para Levene's test
library(FSA)  # Para Dunn's test

# Cargar el archivo Excel
file_path <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"
datos <- read_excel(file_path, sheet = "Probabilidad de escape")

# Filtrar los datos para excluir la condición "AP"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S"))

# Calcular la proporción de respuestas por individuo
proporciones_por_individuo <- datos_filtrados %>%
  group_by(Individuo, Condicion, Semana, Cruza) %>%
  summarise(Proporcion_Respuesta = sum(Respuesta) / n(), .groups = "drop")

# Convertir 'Semana' a factor para ANOVA
proporciones_por_individuo <- proporciones_por_individuo %>%
  mutate(Semana = as.factor(Semana))

# ANOVA con Semana y Condicion como factores
anova_model <- aov(Proporcion_Respuesta ~ Semana * Condicion, data = proporciones_por_individuo)

# Resumen del ANOVA ajustado
summary(anova_model)

# Verificación de supuestos:
# 1. Normalidad de los residuos
shapiro.test(residuals(anova_model))

# 2. Homogeneidad de varianzas (Levene's Test)
leveneTest(Proporcion_Respuesta ~ Semana * Condicion, data = proporciones_por_individuo)

# ANOVA de Welch como alternativa (si los supuestos no se cumplen)
anova_welch <- oneway.test(Proporcion_Respuesta ~ Semana * Condicion, data = proporciones_por_individuo)
print(anova_welch)

# Comparaciones específicas con emmeans
library(emmeans)

# Obtener las medias marginales ajustadas
emmeans_results <- emmeans(anova_model, ~ Condicion | Semana)

# Comparar "I" vs "S" dentro de cada semana
contrastes_especificos <- contrast(emmeans_results, method = "pairwise") %>%
  as.data.frame() %>%
  filter(grepl("A - S", contrast))  # Filtrar solo las comparaciones A - S

# Mostrar los resultados
print(contrastes_especificos)

#####                                      ##### 
##### CINÉTICA DE LAS RESPUESTAS DE ESCAPE #####
#####                                      ##### 

#####                                      ##### 
##### MÁXIMA VELOCIDAD LINEAL DE LA CABEZA #####
#####                                      ##### 

library(readxl)
library(ggplot2)
library(dplyr)
library(broom)

# Leer el archivo de Excel
ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"
data <- read_excel(ruta_archivo, sheet = "Maxima Velocidad Lineal")

# Visualizar las primeras filas del dataset para verificar
head(data)

# Crear el gráfico de violin plot
ggplot(data, aes(x = Condicion, y = `Velocidad_lineal_maxima_(mm/ms)`, fill = Condicion)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Añadir la curva del violín
  geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "black") +  # Añadir puntos dispersos
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, color = "red", fill = "red") +  # Marcar la media
  stat_summary(fun = "median", geom = "crossbar", width = 0.5, size = 1, color = "white") +  # Marcar la mediana
  labs( 
    x = "Condición", 
    y = "Velocidad lineal maxima (mm/ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "lightblue", "S" = "lightgreen"))  # Colores personalizados para las condiciones

###################### TEST DE T ################

# Realizar el test t de Student
resultado_ttest <- t.test(`Velocidad_lineal_maxima_(mm/ms)` ~ Condicion, data = data)

# Imprimir el resultado del test t
print(resultado_ttest)

############################# RESUMEN ######################

# Función para calcular la media y el intervalo de confianza
calcular_resumen <- function(data) {
  data %>%
    group_by(Condicion) %>%
    summarise(
      N = n(),
      Media = mean(`Velocidad_lineal_maxima_(mm/ms)`, na.rm = TRUE),
      SD = sd(`Velocidad_lineal_maxima_(mm/ms)`, na.rm = TRUE),
      Error_estandar = SD / sqrt(N),
      IC_lower = Media - qt(0.975, df = N - 1) * (SD / sqrt(N)),
      IC_upper = Media + qt(0.975, df = N - 1) * (SD / sqrt(N))
    )
}

# Calcular el resumen
resultado_resumen <- calcular_resumen(data)

# Imprimir el resultado
print(resultado_resumen)


#####                                       ##### 
##### MÁXIMA VELOCIDAD ANGULAR DE LA CABEZA #####
#####                                       #####

library(readxl)
library(ggplot2)
library(dplyr)
library(broom)

# Leer el archivo de Excel
ruta_archivo <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Arena Escape.xlsx"
data <- read_excel(ruta_archivo, sheet = "Maxima Velocidad Angular")

# Visualizar las primeras filas del dataset para verificar
head(data)

# Crear el gráfico de violin plot
ggplot(data, aes(x = Condicion, y = `Velocidad_angular_maxima`, fill = Condicion)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Añadir la curva del violín
  geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "black") +  # Añadir puntos dispersos
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, color = "red", fill = "red") +  # Marcar la media
  stat_summary(fun = "median", geom = "crossbar", width = 0.5, size = 1, color = "white") +  # Marcar la mediana
  labs( 
    x = "Condición", 
    y = "Maxima velocidad angular (°/ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "lightblue", "S" = "lightgreen"))  # Colores personalizados para las condiciones

###################### TEST DE T ################

# Realizar el test t de Student
resultado_ttest <- t.test(`Velocidad_angular_maxima` ~ Condicion, data = data)

# Imprimir el resultado del test t
print(resultado_ttest)

############################# RESUMEN ######################

# Función para calcular la media y el intervalo de confianza
calcular_resumen <- function(data) {
  data %>%
    group_by(Condicion) %>%
    summarise(
      N = n(),
      Media = mean(`Velocidad_angular_maxima`, na.rm = TRUE),
      SD = sd(`Velocidad_angular_maxima`, na.rm = TRUE),
      Error_estandar = SD / sqrt(N),
      IC_lower = Media - qt(0.975, df = N - 1) * Error_estandar,
      IC_upper = Media + qt(0.975, df = N - 1) * Error_estandar
    )
}


# Calcular el resumen
resultado_resumen <- calcular_resumen(data)

# Imprimir el resultado
print(resultado_resumen)

##### LA PARTE DE KernelPCA SE HIZO EN PYTHON #####

#####                                            #####
##### MATRIZ DE PROBABILIDADES OBSERVADAS (P.O.) #####
#####                                            #####

#####          ##### 
##### SOCIALES #####
#####          #####

library(ggplot2)

matriz <- matrix(c(NA, 0.05, 0.15, 0, 0.083, 0.1, 0, 0.06, 0.25), 
                 nrow = 3, ncol = 3)
rownames(matriz) <- c("0", "136.6 dB", "140.9 dB")
colnames(matriz) <- c("0", "0.09 MI", "0.36 MI")

# Crear un dataframe para los valores adicionales
df <- as.data.frame(as.table(matriz))
df$ValorExtra <- c(NA, 0.067, 0.15, 0.217, 0.267, 0.217, 0.083, 0.217, 0.367)  # Valores extras para mostrar debajo

p <- ggplot() +
  geom_tile(data = df,
            aes(x = Var2, y = Var1, fill = Freq), color = "white") +
  # Números principales
  geom_text(data = df,
            aes(x = Var2, y = Var1, label = Freq), 
            vjust = 0, size = 10) +  # Posición ajustada para dejar espacio debajo
  # Números adicionales en rojo
  geom_text(data = df,
            aes(x = Var2, y = Var1, label = ValorExtra), 
            vjust = 2, size = 10, color = "darkred") +  # Ajuste de posición y color
  scale_fill_gradient(low = "#BFD8E8", high = "#4D90E5") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 20),
    axis.text.y = element_text(angle = 0, hjust = 1 , vjust = 1, size = 20), 
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 23, vjust = -1),
    axis.title.y = element_text(color = "black", size = 23, vjust = 2),
    legend.text = element_text(size = 20),  # Ajustar tamaño del texto de la barra
    legend.title = element_text(size = 20) # Ajustar tamaño del título de la barra
  ) +
  coord_fixed(ratio = 1) +
  labs(x = "Estímulo Visual", y = "Estímulo Auditivo")

print(p)

#####          ##### 
##### AISLADOS #####
#####          #####

library(ggplot2)

matriz <- matrix(c(NA, 0.1, 0.17, 0, 0.07, 0.14, 0.06, 0.11, 0.08), 
                 nrow = 3, ncol = 3)
rownames(matriz) <- c("0", "136.6 dB", "140.9 dB")
colnames(matriz) <- c("0", "0.09 MI", "0.36 MI")

# Crear un dataframe para los valores adicionales
df <- as.data.frame(as.table(matriz))
df$ValorExtra <- c(NA, 0.13, 0.174, 0.217, 0.246, 0.275, 0.261, 0.261, 0.290)  # Valores extras para mostrar debajo

p <- ggplot() +
  geom_tile(data = df,
            aes(x = Var2, y = Var1, fill = Freq), color = "white") +
  # Números principales
  geom_text(data = df,
            aes(x = Var2, y = Var1, label = Freq), 
            vjust = 0, size = 10) +  # Posición ajustada para dejar espacio debajo
  # Números adicionales en rojo
  geom_text(data = df,
            aes(x = Var2, y = Var1, label = ValorExtra), 
            vjust = 2, size = 10, color = "darkred") +  # Ajuste de posición y color
  scale_fill_gradient(low = "#BFD8E8", high = "#4D90E5") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 20),
    axis.text.y = element_text(angle = 0, hjust = 1 , vjust = 1, size = 20), 
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 23, vjust = -1),
    axis.title.y = element_text(color = "black", size = 23, vjust = 2),
    legend.text = element_text(size = 20),  # Ajustar tamaño del texto de la barra
    legend.title = element_text(size = 20) # Ajustar tamaño del título de la barra
  ) +
  coord_fixed(ratio = 1) +
  labs(x = "Estímulo Visual", y = "Estímulo Auditivo")

print(p)

#####                                      #####
##### MATRIZ DE INTEGRACIÓN MULTISENSORIAL #####
#####                                      #####

# Para calcular el Índice de Integración Multisensorial (IM) utilizamos las frecuencias o probabilidades observadas
# y las frecuencias esperadas. Las frecuencias observadas las calculamos en el ítem anterior, las frecuencias 
# esperadas las calculamos según la ley de probabilidades independientes (asumimos que no hay integración). Estas
# las calculamos como :la probabilidad de respuesta frente al estímulo visual + la probabilidad de respuesta 
# frente al estímulo auditivo - la probabilidad de respuesta frente a estos estímulos combinados. 
# Luego el índice IM lo calculamos como: (PO - PE) / (PO + PE).

#####          ##### 
##### SOCIALES #####
#####          ##### 

library(ggplot2)

matriz <- matrix(c(0.248, -0.2, 0.091, 0.25), nrow = 2, ncol = 2)
rownames(matriz) <- c("136.6 dB", "140.9 dB")
colnames(matriz) <- c("0.09 MI", "0.36 MI")

# Crear un dataframe para los valores adicionales
df <- as.data.frame(as.table(matriz))
df$ValorExtra <- c(-0.004, - 0.213, 0.2, 0.248)  # Valores extras para mostrar debajo

p <- ggplot() +
  geom_tile(data = as.data.frame(as.table(matriz)),
            aes(x = Var2, y = Var1, fill = Freq), color = "white") +
  # Números principales
  geom_text(data = df, aes(x = Var2, y = Var1, label = Freq), vjust = 0, size = 10) +  # Posición ajustada para dejar espacio debajo
  # Números adicionales en rojo
  #geom_text(data = df, aes(x = Var2, y = Var1, label = ValorExtra), vjust = 2, size = 10, color = "darkblue") +
  scale_fill_gradient2(low = "#AF76A3", mid = "#CCCCCC", high = "#FFFF66", midpoint = 0) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 20),
    axis.text.y = element_text(angle = 0, hjust = 1 , vjust = 1, size = 20), 
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 23, vjust = -1),
    axis.title.y = element_text(color = "black", size = 23, vjust = 2),
    legend.text = element_text(size = 20),  # Ajustar tamaño del texto de la barra
    legend.title = element_text(size = 20) # Ajustar tamaño del título de la barra
  ) +
  coord_fixed(ratio = 1) +
  labs(x = "Estímulo Visual", y = "Estímulo Auditivo")

print(p)

#####          ##### 
##### AISLADOS #####
#####          ##### 

library(ggplot2)

matriz <- matrix(c(-0.176, -0.097, -0.167, -0.466), nrow = 2, ncol = 2)
rownames(matriz) <- c("136.6 dB", "140.9 dB")
colnames(matriz) <- c("0.09 MI", "0.36 MI")

# Crear un dataframe para los valores adicionales
df <- as.data.frame(as.table(matriz))
df$ValorExtra <- c(-0.129, - 0.124, -0.156, -0.147)  # Valores extras para mostrar debajo, son los que están dentro de la ventana temporal de estimulación.

p <- ggplot() +
  geom_tile(data = as.data.frame(as.table(matriz)),
            aes(x = Var2, y = Var1, fill = Freq), color = "white") +
  # Números principales
  geom_text(data = df, aes(x = Var2, y = Var1, label = Freq), vjust = 0, size = 10) +  # Posición ajustada para dejar espacio debajo
  # Números adicionales en rojo
  #geom_text(data = df, aes(x = Var2, y = Var1, label = ValorExtra), vjust = 2, size = 10, color = "darkblue") +
  scale_fill_gradient2(low = "#AF76A3", mid = "#CCCCCC", high = "#FFFF66", midpoint = 0) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 20),
    axis.text.y = element_text(angle = 0, hjust = 1 , vjust = 1, size = 20), 
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 23, vjust = -1),
    axis.title.y = element_text(color = "black", size = 23, vjust = 2),
    legend.text = element_text(size = 20),  # Ajustar tamaño del texto de la barra
    legend.title = element_text(size = 20) # Ajustar tamaño del título de la barra
  ) +
  coord_fixed(ratio = 1) +
  labs(x = "Estímulo Visual", y = "Estímulo Auditivo")

print(p)


###############                                       ###############
###############   EXPERIMENTO DE AISLAMIENTO PARCIAL  ###############
###############                                       ###############

# En este caso se utilizaron 3 grupos experimentales de entre 23-33 dpf: 
# 1) Animales sociales que estuvieron 3 semanas en contexto social.
# 2) Animales aislados que estuvieron 3 semanas en contexto de aislamiento.
# 3) Animales sociales que estuvieron 2 semanas en contexto social y luego 1 semana en aislamiento.

#####                                     #####
##### ÍNDICE DE PREFERENCIA VISUAL SOCIAL #####
#####                                     #####

# Cargar las librerías necesarias
library(readxl)  # Para leer archivos Excel
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización

# Cargar los datos desde la hoja específica
ruta_archivo <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Aislamiento Parcial.xlsx"
datos <- read_excel(ruta_archivo, sheet = "IPV")

# Verifica los primeros registros para confirmar la carga de datos
head(datos)

# Filtra los datos para incluir las condiciones "A", "S" y "AP"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S", "AP")) %>%
  mutate(Condicion = factor(Condicion, levels = c("S", "AP", "A")))  # Reorganizar el orden

# Crear el violin plot
ggplot(datos_filtrados, aes(x = Condicion, y = IPV, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 5) +  # Para mostrar la media
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Línea horizontal en y = 0
  labs(
    x = "Condition",
    y = "IPV"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("S" = "#FFDFBA", "AP" = "#BAE1FF", "A" = "#FFB3BA")) +  # Colores según el nuevo orden
  theme(
    axis.title = element_text(size = 16),  # Tamaño del título de los ejes
    axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
    axis.line = element_line(size = 0.5, colour = "black")  # Línea negra en los ejes con grosor de 1
  )

# Resumen estadístico de los datos con error estándar
resumen <- datos_filtrados %>%
  group_by(Condicion) %>%  # Agrupar por condición
  summarise(
    Media = mean(IPV, na.rm = TRUE),               # Media
    Desviacion_Estandar = sd(IPV, na.rm = TRUE),   # Desvío estándar
    Error_Estandar = Desviacion_Estandar / sqrt(n()),  # Error estándar
    Minimo = min(IPV, na.rm = TRUE),               # Mínimo
    Maximo = max(IPV, na.rm = TRUE),               # Máximo
    Mediana = median(IPV, na.rm = TRUE),           # Mediana
    Q1 = quantile(IPV, 0.25, na.rm = TRUE),        # Primer cuartil
    Q3 = quantile(IPV, 0.75, na.rm = TRUE),        # Tercer cuartil
    n = n()                                        # Número de observaciones
  )

# Mostrar el resumen
print(resumen)

##### TEST PARA VER SI ES DISTINTO DE CERO ###############

# Test t para cada grupo (A, S y AP) comparando la media con cero
test_I <- t.test(datos_filtrados$IPV[datos_filtrados$Condicion == "A"], mu = 0)
test_S <- t.test(datos_filtrados$IPV[datos_filtrados$Condicion == "S"], mu = 0)
test_CN <- t.test(datos_filtrados$IPV[datos_filtrados$Condicion == "AP"], mu = 0)

# Mostrar los resultados de los tests
print("Test para el grupo A:")
print(test_I)

print("Test para el grupo S:")
print(test_S)

print("Test para el grupo AP:")
print(test_CN)

########## ANOVA DE 1 FACTOR ###############

# Realizar un ANOVA de un factor
anova_result <- aov(IPV ~ Condicion, data = datos_filtrados)

# Mostrar el resumen del ANOVA
summary(anova_result)

# Realizar una prueba post-hoc (Tukey HSD) para comparar las diferencias entre niveles de la condición
tukey_result <- TukeyHSD(anova_result)

# Mostrar los resultados del Tukey HSD
print(tukey_result)


#####                             #####
##### DISTANCIA A LA BARRERA (mm) #####
#####                             #####

# Cargar las librerías necesarias
library(readxl)  # Para leer archivos Excel
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización

# Cargar los datos desde la hoja específica
ruta_archivo <-"C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Aislamiento Parcial.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Distancia a la barrera")

# Verifica los primeros registros para confirmar la carga de datos
head(datos)

# Filtra los datos para incluir las condiciones "A", "S" y "AP"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S", "AP")) %>%
  mutate(Condicion = factor(Condicion, levels = c("S", "AP", "A")))  # Reorganizar el orden

# Crear el violin plot
ggplot(datos_filtrados, aes(x = Condicion, y = `Distancia (mm)`, fill = Condicion)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Para mostrar puntos de datos
  stat_summary(fun = "mean", geom = "point", color = "red", size = 5) +  # Para mostrar la media
  labs(
    x = "Condition",
    y = "Distancia (mm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("S" = "#FFDFBA", "AP" = "#BAE1FF", "A" = "#FFB3BA")) +  # Colores según el nuevo orden
  theme(
    axis.title = element_text(size = 16),  # Tamaño del título de los ejes
    axis.text = element_text(size = 14),   # Tamaño de las etiquetas de los ejes
    axis.line = element_line(size = 0.5, colour = "black")  # Línea negra en los ejes con grosor de 1
  )

# Resumen estadístico de los datos
resumen <- datos_filtrados %>%
  group_by(Condicion) %>%  # Agrupar por condición
  summarise(
    Media = mean(`Distancia (mm)`, na.rm = TRUE),               # Media
    Desviacion_Estandar = sd(`Distancia (mm)`, na.rm = TRUE),   # Desvío estándar
    Error_Estandar = Desviacion_Estandar / sqrt(n()),           # Error estándar
    Minimo = min(`Distancia (mm)`, na.rm = TRUE),               # Mínimo
    Maximo = max(`Distancia (mm)`, na.rm = TRUE),               # Máximo
    Mediana = median(`Distancia (mm)`, na.rm = TRUE),           # Mediana
    Q1 = quantile(`Distancia (mm)`, 0.25, na.rm = TRUE),        # Primer cuartil
    Q3 = quantile(`Distancia (mm)`, 0.75, na.rm = TRUE),        # Tercer cuartil
    n = n()                                                     # Número de observaciones
  )

# Mostrar el resumen
print(resumen)

# Parte estadística

install.packages("car")

# Cargar las librerías necesarias
library(readxl)  # Para leer archivos Excel
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización
library(car)     # Para pruebas adicionales, como la homogeneidad de varianza
library(multcomp) # Para pruebas post hoc como Tukey

# Filtra los datos para incluir las condiciones "A", "S" y "AP"
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S", "AP")) %>%
  mutate(Condicion = factor(Condicion, levels = c("S", "AP", "A")))  # Reorganizar el orden

# Verificar la homogeneidad de varianzas con la prueba de Levene
prueba_levene <- leveneTest(`Distancia (mm)` ~ Condicion, data = datos_filtrados)
print("Prueba de Levene para homogeneidad de varianzas:")
print(prueba_levene)

# Realizar ANOVA
anova_resultado <- aov(`Distancia (mm)` ~ Condicion, data = datos_filtrados)
print("Resultado del ANOVA:")
summary(anova_resultado)

# Si el ANOVA es significativo, realizar la prueba post hoc de Tukey
if (summary(anova_resultado)[[1]][["Pr(>F)"]][1] < 0.05) {
  print("Prueba post hoc de Tukey:")
  prueba_tukey <- TukeyHSD(anova_resultado)
  print(prueba_tukey)
} else {
  print("No hay diferencias significativas entre los grupos según el ANOVA.")
}

# Visualización de las comparaciones post hoc
if (exists("prueba_tukey")) {
  plot(prueba_tukey)
}


#### TEST DE T ENTRE S Y AP ############

# Filtrar los datos para las condiciones S y AP
datos_ttest <- datos_filtrados %>%
  filter(Condicion %in% c("S", "AP"))

# Realizar el test t
ttest_resultado <- t.test(`Distancia (mm)` ~ Condicion, data = datos_ttest, var.equal = TRUE)  # Supone igualdad de varianzas

# Mostrar los resultados del test t
print("Resultado del test t (S vs AP):")
print(ttest_resultado)

# Verificar igualdad de varianzas con la prueba de F
prueba_varianzas <- var.test(`Distancia (mm)` ~ Condicion, data = datos_ttest)
print("Prueba de igualdad de varianzas (F-test):")
print(prueba_varianzas)


#####                           #####
##### PROBABILIDAD DE RESPUESTA #####
#####                           #####

# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(dplyr)

# Cargar el archivo Excel
file_path <- "C:/Users/Usuario/Desktop/Valen Az Tesis de Licenciatura/Rutina R/Aislamiento Parcial.xlsx"

datos <- read_excel(file_path, sheet = "Probabilidad de escape")

# Filtrar los datos para incluir las condiciones "A", "S", "AP" y solo la semana 4
datos_filtrados <- datos %>%
  filter(Condicion %in% c("A", "S", "AP") & Semana == 4)

# Calcular la proporción de respuestas por individuo (considerando la cruza)
proporciones_por_individuo <- datos_filtrados %>%
  group_by(Cruza, Individuo, Condicion) %>%  # Agrupar por Cruza, Individuo y Condición
  summarise(
    Proporcion_Respuesta = sum(Respuesta) / 8,  # Proporción de respuestas por individuo
    .groups = 'drop'
  )

# Verificar tamaño muestral por condición
conteo_por_grupo <- proporciones_por_individuo %>%
  group_by(Condicion) %>%
  summarise(
    N = n(),  # Número de individuos por condición
    .groups = 'drop'
  )

print(conteo_por_grupo)  # Imprimir el tamaño muestral por grupo

# Calcular el promedio y el error estándar por condición
probabilidades_por_grupo <- proporciones_por_individuo %>%
  group_by(Condicion) %>%
  summarise(
    Probabilidad_Respuesta = mean(Proporcion_Respuesta),  # Promedio de proporciones
    SE = sd(Proporcion_Respuesta) / sqrt(n()),  # Error estándar
    .groups = 'drop'
  )

# Ordenar las condiciones para que CN quede en el medio, S a la izquierda e I a la derecha
probabilidades_por_grupo$Condicion <- factor(probabilidades_por_grupo$Condicion, 
                                             levels = c("S", "AP", "A"))

# Crear el gráfico de barras con barras de error
ggplot(probabilidades_por_grupo, aes(x = Condicion, y = Probabilidad_Respuesta, fill = Condicion)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Crear barras separadas por condición
  geom_errorbar(aes(ymin = Probabilidad_Respuesta - SE, ymax = Probabilidad_Respuesta + SE), 
                position = position_dodge(0.9), width = 0.25) +  # Añadir barras de error
  labs(
    x = "Condición",
    y = "Probabilidad de respuesta",
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),   # Líneas negras en los ejes
    panel.grid.major = element_blank(),          # Eliminar líneas de la cuadrícula
    panel.grid.minor = element_blank()           # Eliminar líneas de la cuadrícula menor
  ) +
  scale_fill_manual(values = c("S" = "#6699FF",  # Azul para condición "S"
                               "AP" = "#66FF66", # Verde para condición "CN"
                               "A" = "#FF6666")) # Rojo para condición "I"

# Calcular resumen de datos por condición
resumen <- probabilidades_por_grupo %>%
  summarise(
    Condicion = Condicion,
    N = conteo_por_grupo$N[conteo_por_grupo$Condicion == Condicion],  # Tamaño muestral por condición
    Media = Probabilidad_Respuesta,
    SD = SE * sqrt(N),  # Calcular SD a partir del SE
    SE = SE,
    IC_lower = Media - qt(0.975, df = N - 1) * SE,  # Límite inferior del IC
    IC_upper = Media + qt(0.975, df = N - 1) * SE,  # Límite superior del IC
    .groups = 'drop'
  )

# Mostrar resumen
print(resumen)

# ANOVA para comparar la proporción de respuestas entre las tres condiciones
anova_resultado <- aov(Proporcion_Respuesta ~ Condicion, data = proporciones_por_individuo)

# Ver los resultados del ANOVA
summary(anova_resultado)


