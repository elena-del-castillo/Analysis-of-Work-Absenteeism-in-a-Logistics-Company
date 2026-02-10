# Limpiar el entorno
rm(list = ls())
# Cargar paquetes necesarios
library(readxl)
library(forecast)
library(dplyr)
library(ggplot2)
library(moments)
library(strucchange)
library(shiny)
library(shinydashboard)
library(kableExtra)
library(patchwork)

# CARGA Y TRATAMIENTO DE LAS DISTINAS BASES DE DATOS 
## Leer los datos asegurando que Fecha sea Date
datos_24_25 <- read_excel("BBDD_TFM/absentismo_datos.xlsx", sheet = "24_25") %>% mutate(Fecha = as.Date(Fecha))  # Convertir a formato Date
datos_marzo_junio <- read_excel("BBDD_TFM/absentismo_datos.xlsx", sheet = "marzo_junio") %>% mutate(Fecha = as.Date(Fecha))
datos_2025_hplanif <- read_excel("BBDD_TFM/absentismo_datos.xlsx", sheet = "junio") %>% mutate(Fecha = as.Date(Fecha))
datos_2025_empleado <- read_excel("BBDD_TFM/absentismo_datos.xlsx", sheet = "empleado")

## Creación de columnas (dia_semana, tipo_dia) mediante función
agregar_tipo_dia <- function(df) {df %>% mutate(
      dia_semana = factor(weekdays(Fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE),
      tipo_dia = case_when(
        Fecha %in% as.Date(c("2024-01-01", "2024-01-06", "2024-03-29", "2024-05-01", "2024-08-15", "2024-10-12", "2024-11-01", "2024-12-06", "2024-12-25", "2025-01-01", "2025-01-06", "2025-04-18", "2025-05-01", "2025-08-15", "2025-10-12", "2025-11-01", "2025-12-06", "2025-12-25")) ~ "Festivo Nacional",
        dia_semana %in% c("Saturday", "Sunday") ~ "Fin de semana",
        TRUE ~ "Laborable"))}

datos_24_25 <- agregar_tipo_dia(datos_24_25)
datos_2025_hplanif <- agregar_tipo_dia(datos_2025_hplanif)

## Convertir a numérico
datos_2025_empleado <- datos_2025_empleado %>% 
  mutate(hplanif_empleado = as.numeric(hplanif_empleado), IT_empleado = as.numeric(IT_empleado)) %>% 
  filter(!is.na(hplanif_empleado), !is.na(IT_empleado))  # Eliminar NA's si los hay



# ANÁLISIS EXPLORATORIO DE LOS DATOS (2024 - 2025)
estadisticas_IT <- datos_24_25 %>% summarise(
    # Estadísticas básicas
    N = n(),
    Media_IT = mean(IT, na.rm = TRUE),
    Mediana_IT = median(IT, na.rm = TRUE),
    Desv_Std = sd(IT, na.rm = TRUE),
    Varianza_IT = var(IT, na.rm = TRUE),
    Mín = min(IT, na.rm = TRUE),
    Máx = max(IT, na.rm = TRUE),
    # Cuartiles y percentiles
    Q1 = quantile(IT, 0.25, na.rm = TRUE),
    Q3 = quantile(IT, 0.75, na.rm = TRUE),
    Rango_Interc = Q3 - Q1,
    # Asimetría y curtosis (requiere librería e1071 o moments)
    Asimetría = moments::skewness(IT, na.rm = TRUE),  
    Curtosis = moments::kurtosis(IT, na.rm = TRUE),) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

## Comparación por tipo_dia
estadisticas_por_tipo <- datos_24_25 %>% group_by(tipo_dia) %>% summarise(
    # Estadísticas básicas
    N = n(),
    Media_IT = mean(IT, na.rm = TRUE),
    Mediana_IT = median(IT, na.rm = TRUE),
    Desv_Std = sd(IT, na.rm = TRUE),
    Varianza_IT = var(IT, na.rm = TRUE),
    Mín = min(IT, na.rm = TRUE),
    Máx = max(IT, na.rm = TRUE),
    # Cuartiles y percentiles
    Q1 = quantile(IT, 0.25, na.rm = TRUE),
    Q3 = quantile(IT, 0.75, na.rm = TRUE),
    Rango_Interc = Q3 - Q1,
    # Asimetría y curtosis
    Asimetría = moments::skewness(IT, na.rm = TRUE),  
    Curtosis = moments::kurtosis(IT, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~round(., 2)))  


## Gráfico SSTT 2024 - 2025
matplot(x = datos_24_25$Fecha, y = datos_24_25$IT, type = "l", lty = 1, lwd = 1, col = "blue", xlab = "Fecha", ylab = "Horas de Absentismo IT", main = "Absentismo IT (2024 y 2025)", xaxt = "n",)
axis.Date(1, at = seq(min(datos_24_25$Fecha), max(datos_24_25$Fecha), by = "month"), format = "%b %Y", las = 1) # Eje x personalizado con formato mes y año
grid()

## Hplanific vs habsIT 2025 
matplot( x = datos_2025_hplanif$Fecha, y = cbind(datos_2025_hplanif$IT, datos_2025_hplanif$hplanif), type = "l", lty = 1, lwd = 2, col = c("#1f77b4", "#ff7f0e"), xlab = "Fecha", ylab = "Horas", main = "Horas planficadas vs IT")
legend("topright", legend = c("IT", "horas planificadas"), col = c("#1f77b4", "#ff7f0e"), lty = 1, lwd = 2, cex = 1)
grid()

## Absentismo por tipo de día y dia semana
### Análisis de IT en días laborables vs. festivos 
datos_24_25 %>% group_by(tipo_dia) %>% summarise(
  Total_IT = sum(IT, na.rm = TRUE),
  Total_dias = n(),) %>%
  arrange(desc(Total_IT))

### Gráfico comparativo (boxplot para visualizar distribución)
ggplot(datos_24_25, aes(x = tipo_dia, y = IT, fill = tipo_dia)) + geom_boxplot(outlier.color = "red") +
  labs( title = "Distribución del Absentismo IT por Tipo de Día", x = NULL, y = "Horas de IT") + theme_minimal() + theme(legend.position = "none")

### Días de la semana con más horas planificadas
grafico1 <- ggplot(datos_2025_hplanif, aes(x = dia_semana, y = hplanif, fill = dia_semana)) + 
  geom_bar(stat = "identity") +  labs(title = "Total de horas planificadas por día de la semana", 
       x = NULL, y = "Horas planificadas") + theme_minimal() + theme(legend.position = "none") +  scale_fill_brewer(palette = "Set2")  

### Día de la semana que se produce más absentismo IT
grafico2 <- ggplot(datos_24_25, aes(x = dia_semana, y = IT, fill = dia_semana)) + geom_bar(stat = "identity") + 
  labs(title = "Total de IT por día de la semana", x = NULL, y = "Total IT") + theme_minimal() +  theme(legend.position = "none") 

grafico1 + grafico2

### Días de la semana que más absentismo se produce (teniendo en cuenta las horas planficadas para cada día)
tabla_comparativa <- datos_2025_hplanif %>%
  group_by(dia_semana) %>% summarise(
    Total_Horas_Planificadas = sum(hplanif, na.rm = TRUE),
    Total_IT = sum(IT, na.rm = TRUE),
    Tasa_IT_por_hora = (Total_IT / Total_Horas_Planificadas) * 100  ) %>%
  arrange(desc(Tasa_IT_por_hora))

knitr::kable(tabla_comparativa, 
             caption = "Tasa de IT por horas planificadas por día de la semana",
             col.names = c("Día", "Horas Planif.", "Total IT", "Tasa IT"),
             digits = 2) %>%
  kableExtra::kable_styling("striped", full_width = FALSE) %>%
  kableExtra::column_spec(4, bold = TRUE)  # Destacar la columna de tasa


### Tipos de día que más absentismo se produce (teniendo en cuenta las horas planficadas para cada día)
tabla_comparativa_tipo <- datos_2025_hplanif %>%
  group_by(tipo_dia) %>%
  summarise(
    Total_Horas_Planificadas = sum(hplanif, na.rm = TRUE),
    Total_IT = sum(IT, na.rm = TRUE),
    Tasa_IT_por_hora = (Total_IT / Total_Horas_Planificadas) * 100
  ) %>%
  arrange(desc(Tasa_IT_por_hora))

knitr::kable(tabla_comparativa_tipo, 
             caption = "Tasa de IT por horas planificadas por tipo de día",
             col.names = c("Tipo de Día", "Horas Planif.", "Total IT", "Tasa IT (%)"),
             digits = 2) %>%
  kableExtra::kable_styling("striped", full_width = FALSE) %>%
  kableExtra::column_spec(4, bold = TRUE)  # Destacar la columna de tasa


### Gráfico de densidad comparativo
ggplot(datos_24_25, aes(x = IT, fill = tipo_dia)) + geom_density(alpha = 0.5) +
  labs(title = "Distribución de Densidad por Tipo de Día", x = "Horas de Absentismo", y = "Densidad") +
  theme_minimal() + scale_fill_brewer(palette = "Set1", name = "Tipo de Día")


## Absentismo por Empleado 
### Relación entre Horas Planificadas y Absentismo por Empleado
ggplot(datos_2025_empleado, aes(x = hplanif_empleado, y = IT_empleado)) + geom_point(alpha = 0.6) + geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación Horas Planificadas vs Absentismo por Empleado", x = "Horas Planificadas", y = "Horas de IT") + 
  theme_minimal() + theme(plot.title = element_text(size = 16), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

### Top 10 empleados con mayor absentismo
top_absentistas <- datos_2025_empleado %>% arrange(desc(IT_empleado)) %>% head(10)
### Empleados con IT igual a horas planificadas (bajas completas)
bajas_completas <- datos_2025_empleado %>% filter(IT_empleado == hplanif_empleado)


## Ratio absentismo IT (comparar con el sector)
ratioIT <- (sum(datos_2025_hplanif$IT)/ sum(datos_2025_hplanif$hplanif))*100

## Eficiencia ajustada (impacto en la productividad o cumplimiento de las hplanif)
Eficiencia = (sum(datos_2025_hplanif$hplanif) - sum(datos_2025_hplanif$IT))/ sum(datos_2025_hplanif$hplanif)


## Gráfico SSTT 2025 datos obtenidos en marzo y mayo
error_abs_marzo_mayo <- abs(datos_marzo_junio$IT_marzo - datos_marzo_junio$IT_mayo)
rmse_marzo_mayo <- sqrt(mean((datos_marzo_junio$IT_marzo - datos_marzo_junio$IT_mayo)^2))

matplot(x = datos_marzo_junio$Fecha, y = cbind(datos_marzo_junio$IT_marzo, datos_marzo_junio$IT_mayo, error_abs_marzo_mayo), type = "l", lty = 1, lwd = 2, col = c("#1f77b4", "#ff7f0e", "#2ca02c"), xlab = "Fecha", ylab = "Horas de Absentismo IT", main = "Comparación IT datos Marzo vs Mayo (2025)")
legend("topright", legend = c("IT datos de Marzo", "IT datos de Mayo", "Error Absoluto"), col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
grid()

## Gráfico SSTT 2025 datos obtenidos en marzo y junio
error_abs_marzo_junio <- abs(datos_marzo_junio$IT_marzo - datos_marzo_junio$IT_junio)
rmse_marzo_junio <- sqrt(mean((datos_marzo_junio$IT_marzo - datos_marzo_junio$IT_junio)^2))

matplot(x = datos_marzo_junio$Fecha, y = cbind(datos_marzo_junio$IT_marzo, datos_marzo_junio$IT_mayo, error_abs_marzo_junio), type = "l", lty = 1, lwd = 2, col = c("#1f77b4", "#ff7f0e", "#2ca02c"), xlab = "Fecha", ylab = "Horas de Absentismo IT", main = "Comparación IT datos Marzo vs Junio (2025)")
legend("topright", legend = c("IT datos de Marzo", "IT datos de Junio", "Error Absoluto"),  col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
grid()

## Gráfico SSTT 2025 datos obtenidos en mayo y junio
error_abs_mayo_junio <- abs(datos_marzo_junio$IT_mayo - datos_marzo_junio$IT_junio)
rmse_mayo_junio <- sqrt(mean((datos_marzo_junio$IT_mayo - datos_marzo_junio$IT_junio)^2))

matplot( x = datos_marzo_junio$Fecha, y = cbind(datos_marzo_junio$IT_mayo, datos_marzo_junio$IT_junio, error_abs_mayo_junio), type = "l", lty = 1, lwd = 2, col = c("#1f77b4", "#ff7f0e", "#2ca02c"), xlab = "Fecha", ylab = "Horas de Absentismo IT", main = "Comparación IT datos Mayo vs Junio (2025)")
legend("topright", legend = c("IT datos de Marzo", "IT datos de Junio", "Error Absoluto"), col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
grid()



# PREDICCIONES IT 
# Predicciones horas absentismo por incapacidad temporal
fh <- 30       # Horizonte de predicción 
ini <- 100     # Tamaño de muestra inicial
n <- 731       # Total de observaciones

## Inicializar matrices vacías
forec <- matrix(NA, nrow = fh, ncol = n - fh - ini)
reales <- matrix(NA, nrow = fh, ncol = n - fh - ini)
forecarima <- matrix(NA, nrow = fh, ncol = n - fh - ini)
forecnnar <- matrix(NA, nrow = fh, ncol = n - fh - ini)
forecnaive <- matrix(NA, nrow = fh, ncol = n - fh - ini)  
forecets_trend <- matrix(NA, nrow = fh, ncol = n - fh - ini)  # Para ETS con tendencia

## Generación recursiva de predicciones de absentismo por IT conmúltiples modelos
i <- 1
while (i < (n - fh - ini)) {
  y <- datos_24_25$IT[1:(ini + i)]
  ts_y <- ts(y, frequency = 7)
  # Modelo Naive (basado en el último valor observado)
  naive_model <- naive(ts_y, h = fh)
  fnaive <- naive_model$mean
  # ETS con tendencia aditiva (AAN)
  ets_trend_model <- ets(ts_y, model = "AAN")
  fets_trend <- forecast(ets_trend_model, h = fh)$mean
  # Modelo red neuronal (NNAR)
  nnet_model <- nnetar(ts_y, p = 7, size = 6, repeats = 50, scale.inputs = TRUE)
  fnnet <- forecast(nnet_model, h = fh)$mean
  # ETS (ANA)
  m1 <- ets(ts_y, model = "ANA")
  f1 <- forecast(m1, h = fh)$mean
  # ARIMA automático
  arimamodel <- auto.arima(ts_y)
  farima <- forecast(arimamodel, h = fh)$mean
  
  # Guardar resultados
  forec[, i] <- f1
  forecarima[, i] <- farima
  forecnnar[, i] <- fnnet
  forecnaive[, i] <- fnaive          
  forecets_trend[, i] <- fets_trend  
  reales[, i] <- datos_24_25$IT[(ini + i + 1):(ini + i + fh)]
  i <- i + 1}
## Guardar archivo
save(forec, forecarima, forecnnar, forecnaive, forecets_trend, reales, file = "results4.RData")
## Cargar si es necesario
load("results4.RData")


## errores
errores <- reales - forec
erroresarima <- reales - forecarima
erroresnnar <- reales - forecnnar
erroresnaive <- reales - forecnaive
erroresets_trend <- reales - forecets_trend

## RMSE
RMSE <- sqrt(rowMeans(errores^2, na.rm = TRUE))
RMSEarima <- sqrt(rowMeans(erroresarima^2, na.rm = TRUE))
RMSE_nnar <- sqrt(rowMeans(erroresnnar^2, na.rm = TRUE))
RMSE_naive <- sqrt(rowMeans(erroresnaive^2, na.rm = TRUE))
RMSE_ets_trend <- sqrt(rowMeans(erroresets_trend^2, na.rm = TRUE))

## MdAE
MdAE <- apply(abs(errores), 1, median, na.rm = TRUE)
MdAE_arima <- apply(abs(erroresarima), 1, median, na.rm = TRUE)
MdAE_nnar <- apply(abs(erroresnnar), 1, median, na.rm = TRUE)
MdAE_naive <- apply(abs(erroresnaive), 1, median, na.rm = TRUE)
MdAE_ets_trend <- apply(abs(erroresets_trend), 1, median, na.rm = TRUE)

## Gráficas comparativas
plot(RMSE, type = "l", col = "blue", lwd = 2, 
     ylim = range(c(RMSE, RMSEarima, RMSE_nnar, RMSE_naive, RMSE_ets_trend), na.rm = TRUE),
     ylab = "RMSE", xlab = "Horizonte", main = "Comparación de RMSE")
lines(RMSEarima, col = "violet", lwd = 2)
lines(RMSE_nnar, col = "orange", lwd = 2)
lines(RMSE_naive, col = "green", lwd = 2)
lines(RMSE_ets_trend, col = "red", lwd = 2)
legend("topright", 
       legend = c("ETS(A,N,A)", "ARIMA", "NNAR", "Naive", "ETS(A,A,N)"),
       col = c("blue", "violet", "orange", "green", "red"), lty = 1, lwd = 2, cex = 0.8)  

## Promedios de errores
cat("Promedios de RMSE:\n")
cat(" ETS(ANA):", mean(RMSE, na.rm = TRUE), "\n",
    "ARIMA:", mean(RMSEarima, na.rm = TRUE), "\n",
    "NNAR:", mean(RMSE_nnar, na.rm = TRUE), "\n",
    "Naive:", mean(RMSE_naive, na.rm = TRUE), "\n",
    "ETS(AAN):", mean(RMSE_ets_trend, na.rm = TRUE), "\n\n")


## Gráfico predicciones + original (para el mejor modelo)
### Seleccionar la última ventana de predicción para ETS 
ultima_ventana <- ncol(forec)
predicciones <- forec[, ultima_ventana]
reales_ultimos <- datos_24_25$IT[(nrow(datos_24_25) - length(predicciones) + 1):nrow(datos_24_25)]
### Crear un objeto ts con los datos reales
ts_data <- ts(datos_24_25$IT, frequency = 7)
### Ajustar el modelo ETS final (usando todos los datos)
modelo_final <- ets(ts_data, model = "ANA")
### Generar las predicciones con intervalos (30 días adelante)
pred_final <- forecast(modelo_final, h = 30, level = c(80, 95))
### Preparar datos para el gráfico
ultima_fecha <- max(datos_24_25$Fecha)
fechas_pred <- seq(ultima_fecha + 1, by = "day", length.out = 30)
### Crear dataframe para ggplot
df_pred <- data.frame(Fecha = c(datos_24_25$Fecha, fechas_pred), IT = c(datos_24_25$IT, rep(NA, 30)),
  Prediccion = c(rep(NA, nrow(datos_24_25)), as.numeric(pred_final$mean)),
  Lo_80 = c(rep(NA, nrow(datos_24_25)), as.numeric(pred_final$lower[,1])),
  Hi_80 = c(rep(NA, nrow(datos_24_25)), as.numeric(pred_final$upper[,1])),
  Lo_95 = c(rep(NA, nrow(datos_24_25)), as.numeric(pred_final$lower[,2])),
  Hi_95 = c(rep(NA, nrow(datos_24_25)), as.numeric(pred_final$upper[,2])))
### Gráfico
ggplot(df_pred, aes(x = Fecha)) +
  # Datos históricos
  geom_line(aes(y = IT), color = "black", size = 0.8) +
  # Intervalo de predicción del 95%
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "blue", alpha = 0.2) +
  # Intervalo de predicción del 80%
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "blue", alpha = 0.3) +
  # Predicción puntual
  geom_line(aes(y = Prediccion), color = "red", size = 1) +
  # Línea vertical que marca el inicio de las predicciones
  geom_vline(xintercept = ultima_fecha, linetype = "dashed", color = "gray") +
  # Etiquetas
  labs(title = "Predicciones de Absentismo IT con Intervalos de Predicción",
       subtitle = "Modelo ETS(A,N,A) - Intervalos al 80% y 95%",
       x = "Fecha", y = "Horas de Absentismo IT") +
  theme_minimal() + theme( legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  annotate("text", x = ultima_fecha - 15, y = max(datos_24_25$IT, na.rm = TRUE)*0.9, 
           label = "Inicio predicciones", color = "gray", size = 3) +
  annotate("text", x = ultima_fecha + 15, y = max(pred_final$upper[,2], na.rm = TRUE), 
           label = "Predicción", color = "red", size = 3)


# PREDICCIÓN 2025 JULIO
# Filtrar datos hasta junio 2025 y predecir julio 2025
datos_hasta_junio <- datos_24_25 %>% 
  filter(Fecha <= as.Date("2025-06-30"))

# Crear serie temporal
ts_data_junio <- ts(datos_hasta_junio$IT, frequency = 7)

# Ajustar modelo ETS
modelo_julio <- ets(ts_data_junio, model = "ANA")

# Predecir 31 días (julio completo)
pred_julio <- forecast(modelo_julio, h = 31, level = c(80, 95))

# Preparar fechas
fechas_julio <- seq(as.Date("2025-07-01"), by = "day", length.out = 31)

# Dataframe para ggplot
df_julio <- data.frame(
  Fecha = c(datos_hasta_junio$Fecha, fechas_julio),
  IT = c(datos_hasta_junio$IT, rep(NA, 31)),
  Prediccion = c(rep(NA, nrow(datos_hasta_junio)), as.numeric(pred_julio$mean)),
  Lo_80 = c(rep(NA, nrow(datos_hasta_junio)), as.numeric(pred_julio$lower[,1])),
  Hi_80 = c(rep(NA, nrow(datos_hasta_junio)), as.numeric(pred_julio$upper[,1])),
  Lo_95 = c(rep(NA, nrow(datos_hasta_junio)), as.numeric(pred_julio$lower[,2])),
  Hi_95 = c(rep(NA, nrow(datos_hasta_junio)), as.numeric(pred_julio$upper[,2]))
)

# Gráfico
ggplot(df_julio, aes(x = Fecha)) +
  # Datos históricos
  geom_line(aes(y = IT), color = "black", size = 0.8) +
  # Intervalos de predicción
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "blue", alpha = 0.3) +
  # Predicción puntual
  geom_line(aes(y = Prediccion), color = "red", size = 1) +
  # Línea divisoria
  geom_vline(xintercept = as.Date("2025-06-30"), linetype = "dashed", color = "gray") +
  # Etiquetas y estilo
  labs(title = "Predicción de Absentismo IT para Julio 2025",
       subtitle = "Modelo ETS(A,N,A) - Intervalos al 80% y 95%",
       x = "Fecha", y = "Horas de Absentismo IT") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               limits = c(as.Date("2025-01-01"), as.Date("2025-07-31"))) +
  # Anotaciones
  annotate("text", x = as.Date("2025-04-15"), y = max(datos_hasta_junio$IT, na.rm = TRUE)*0.9, 
           label = "Datos históricos", color = "black", size = 3) +
  annotate("text", x = as.Date("2025-07-15"), y = max(pred_julio$upper[,2], na.rm = TRUE), 
           label = "Predicción Julio", color = "red", size = 3)





################

# DECISION TREE
# Preparar los datos para el árbol de clasificación
datos_arbol <- datos_24_25 %>% select(IT, dia_semana, tipo_dia) %>% 
  mutate(absentismo_alto = as.factor(ifelse(IT > median(IT, na.rm = TRUE), "Alto", "Bajo")))
# Construir el árbol de decisión (clasificación)
arbol <- rpart(absentismo_alto ~ dia_semana + tipo_dia, data = datos_arbol,
               method = "class", control = rpart.control(cp = 0.01))
# Visualizar el árbol de clasificación
rpart.plot(arbol, extra = 104, box.palette = "GnBu", branch.lty = 3, shadow.col = "gray", nn = TRUE)
# Construir el árbol de regresión para predecir el valor numérico de IT
arbol_reg <- rpart(IT ~ dia_semana + tipo_dia, data = datos_24_25,
                   method = "anova", control = rpart.control(cp = 0.01))
# Visualizar el árbol de regresión
rpart.plot(arbol_reg)




#############################################################################################
######## SHINY DASHBOARD 

# UI (Interfaz de Usuario)
ui <- dashboardPage(
  dashboardHeader(title = "Control Absentismo"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen General", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Predicciones", tabName = "predicciones", icon = icon("chart-line")),
      menuItem("Comparativas", tabName = "comparativas", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Pestaña de Resumen General 
      tabItem(tabName = "resumen",
              fluidRow(
                valueBox(round(ratioIT, 2), "Ratio de Absentismo IT (%)", icon = icon("percent"), color = "red"),
                valueBox(round(Eficiencia*100, 2), "Eficiencia Ajustada (%)", icon = icon("thumbs-up"), color = "green"),
                valueBox(sum(datos_24_25$IT), "Total Horas IT (2024-2025)", icon = icon("clock"), color = "blue")
              ),
              fluidRow(
                box(title = "Estadísticas Descriptivas Globales", status = "primary", solidHeader = TRUE,
                    tableOutput("estadisticas_generales"), width = 6),
                box(title = "Estadísticas Descriptivas por Tipo de Día", status = "primary", solidHeader = TRUE,
                    tableOutput("estadisticas_tipo_dia"), width = 6)
              ),
              fluidRow(
                box(title = "Distribución por Tipo de Día", status = "warning", solidHeader = TRUE,
                    plotOutput("boxplot_tipo_dia"), width = 6),
                box(title = "Distribución de Densidad", status = "warning", solidHeader = TRUE,
                    plotOutput("densidad_tipo_dia"), width = 6)
              ),
              fluidRow(
                box(title = "Evolución Temporal del Absentismo (2024-2025)", status = "info", solidHeader = TRUE,
                    plotOutput("plot_evolucion_temporal"), width = 12)
              ),
              fluidRow(
                box(title = "Absentismo por Día de la Semana", status = "info", solidHeader = TRUE,
                    plotOutput("plot_dia_semana"), width = 6),
                box(title = "Tasas por Día de la Semana", status = "info", solidHeader = TRUE,
                    tableOutput("tabla_dias_semana"), width = 6)
              ),
              fluidRow(
                box(title = "Tasas por Tipo de Día", status = "info", solidHeader = TRUE,
                    tableOutput("tabla_tipo_dia"), width = 6)
              )
      ),
      
      # Pestaña de Predicciones
      tabItem(tabName = "predicciones",
              fluidRow(
                box(title = "Comparación de Modelos Predictivos", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_comparacion_modelos"), width = 12)),
              fluidRow(
                box(title = "Predicción de Absentismo (30 días)", status = "info", solidHeader = TRUE,
                    plotOutput("plot_predicciones"), width = 12)),
        fluidRow(
          box(title = "Predicción Julio 2025", status = "success", solidHeader = TRUE,
              plotOutput("plot_prediccion_julio"), width = 12,
              footer = "Predicción basada en datos hasta junio 2025 con modelo ETS(A,N,A)")),
      ),
      
      # Pestaña de Comparativas
      tabItem(tabName = "comparativas",
              fluidRow(
                box(title = "Comparación Marzo vs Mayo", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_marzo_mayo"), width = 6),
                box(title = "Métricas de Error Marzo-Mayo", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("metricas_marzo_mayo"), width = 6)
              ),
              fluidRow(
                box(title = "Comparación Marzo vs Junio", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_marzo_junio"), width = 6),
                box(title = "Métricas de Error Marzo-Junio", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("metricas_marzo_junio"), width = 6)
              ),
              fluidRow(
                box(title = "Comparación Mayo vs Junio", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_mayo_junio"), width = 6),
                box(title = "Métricas de Error Mayo-Junio", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("metricas_mayo_junio"), width = 6)
              )
      )
    )
  )
)

# Server (Lógica del Servidor)
server <- function(input, output) {
  
  # Resumen General
  output$estadisticas_generales <- function() {
    estadisticas_IT %>%
      kable("html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(1, bold = TRUE) %>%
      scroll_box(width = "100%", height = "300px")
  }
  
  output$estadisticas_tipo_dia <- function() {
    estadisticas_por_tipo %>%
      kable("html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(1, bold = TRUE) %>%
      scroll_box(width = "100%", height = "300px")
  }
  
  output$boxplot_tipo_dia <- renderPlot({
    ggplot(datos_24_25, aes(x = tipo_dia, y = IT, fill = tipo_dia)) + 
      geom_boxplot(outlier.color = "red") +
      labs(title = "Distribución del Absentismo IT por Tipo de Día", 
           x = NULL, y = "Horas de IT") + 
      theme_minimal() + 
      theme(legend.position = "none")
  })
  
  output$densidad_tipo_dia <- renderPlot({
    ggplot(datos_24_25, aes(x = IT, fill = tipo_dia)) + 
      geom_density(alpha = 0.5) +
      labs(title = "Distribución de Densidad por Tipo de Día", 
           x = "Horas de Absentismo", y = "Densidad") +
      theme_minimal() + 
      scale_fill_brewer(palette = "Set1", name = "Tipo de Día")
  })
  
  output$plot_evolucion_temporal <- renderPlot({
    matplot(x = datos_24_25$Fecha, y = datos_24_25$IT, 
            type = "l", lty = 1, lwd = 1, col = "blue", 
            xlab = "Fecha", ylab = "Horas de Absentismo IT", 
            main = "Evolución del Absentismo IT (2024 y 2025)", xaxt = "n")
    axis.Date(1, at = seq(min(datos_24_25$Fecha), max(datos_24_25$Fecha), by = "month"), 
              format = "%b %Y", las = 1)
    grid()
  })
  
  output$plot_hplanif_vs_it <- renderPlot({
    matplot(x = datos_2025_hplanif$Fecha, 
            y = cbind(datos_2025_hplanif$IT, datos_2025_hplanif$hplanif), 
            type = "l", lty = 1, lwd = 2, 
            col = c("#1f77b4", "#ff7f0e"), 
            xlab = "Fecha", ylab = "Horas", 
            main = "Horas planificadas vs IT (2025)")
    legend("topright", legend = c("IT", "horas planificadas"), 
           col = c("#1f77b4", "#ff7f0e"), lty = 1, lwd = 2, cex = 1)
    grid()
  })

  output$plot_dia_semana <- renderPlot({
    grafico1 <- ggplot(datos_2025_hplanif, aes(x = dia_semana, y = hplanif, fill = dia_semana)) + 
      geom_bar(stat = "identity") +  
      labs(title = "Horas planificadas por día", x = NULL, y = "Horas") + 
      theme_minimal() + 
      theme(legend.position = "none") +  
      scale_fill_brewer(palette = "Set2")  
    
    grafico2 <- ggplot(datos_24_25, aes(x = dia_semana, y = IT, fill = dia_semana)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Absentismo IT por día", x = NULL, y = "Horas IT") + 
      theme_minimal() +  
      theme(legend.position = "none") 
    
    grafico1 + grafico2
  })
  
  output$tabla_dias_semana <- function() {
    tabla_comparativa %>%
      kable("html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(4, bold = TRUE) %>%
      scroll_box(width = "100%", height = "200px")
  }
  
  output$tabla_tipo_dia <- function() {
    tabla_comparativa_tipo %>%
      kable("html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(4, bold = TRUE) %>%
      scroll_box(width = "100%", height = "200px")
  }
  
  # Predicciones
  output$plot_comparacion_modelos <- renderPlot({
    plot(RMSE, type = "l", col = "blue", lwd = 2, 
         ylim = range(c(RMSE, RMSEarima, RMSE_nnar, RMSE_naive, RMSE_ets_trend), na.rm = TRUE),
         ylab = "RMSE", xlab = "Horizonte", main = "Comparación de Modelos por RMSE")
    lines(RMSEarima, col = "violet", lwd = 2)
    lines(RMSE_nnar, col = "orange", lwd = 2)
    lines(RMSE_naive, col = "green", lwd = 2)
    lines(RMSE_ets_trend, col = "red", lwd = 2)
    legend("topright", 
           legend = c("ETS(A,N,A)", "ARIMA", "NNAR", "Naive", "ETS(A,A,N)"),
           col = c("blue", "violet", "orange", "green", "red"), lty = 1, lwd = 2, cex = 0.8)
  })
  
  output$plot_predicciones <- renderPlot({
    ggplot(df_pred, aes(x = Fecha)) +
      geom_line(aes(y = IT), color = "black", size = 0.8) +
      geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "blue", alpha = 0.2) +
      geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = Prediccion), color = "red", size = 1) +
      geom_vline(xintercept = ultima_fecha, linetype = "dashed", color = "gray") +
      labs(title = "Predicciones de Absentismo IT (30 días)",
           subtitle = "Modelo ETS(A,N,A) con intervalos de confianza",
           x = "Fecha", y = "Horas de Absentismo IT") +
      theme_minimal() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  })
  
  output$plot_prediccion_julio <- renderPlot({
    ggplot(df_julio, aes(x = Fecha)) +
      geom_line(aes(y = IT), color = "black", size = 0.8) +
      geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "blue", alpha = 0.2) +
      geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = Prediccion), color = "red", size = 1) +
      geom_vline(xintercept = as.Date("2025-06-30"), linetype = "dashed", color = "gray") +
      labs(title = "Predicción de Absentismo IT para Julio 2025",
           subtitle = "Modelo ETS(A,N,A) - Intervalos al 80% y 95%",
           x = "Fecha", y = "Horas de Absentismo IT") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y",
                   limits = c(as.Date("2025-05-01"), as.Date("2025-07-31"))) +
      annotate("text", x = as.Date("2025-05-15"), y = max(datos_hasta_junio$IT, na.rm = TRUE)*0.9, 
               label = "Datos históricos", color = "black", size = 4) +
      annotate("text", x = as.Date("2025-07-15"), y = max(pred_julio$upper[,2], na.rm = TRUE), 
               label = "Predicción Julio", color = "red", size = 4)
  })
  
  # Comparativas
  output$plot_marzo_mayo <- renderPlot({
    matplot(x = datos_marzo_junio$Fecha, 
            y = cbind(datos_marzo_junio$IT_marzo, datos_marzo_junio$IT_mayo, error_abs_marzo_mayo), 
            type = "l", lty = 1, lwd = 2, 
            col = c("#1f77b4", "#ff7f0e", "#2ca02c"), 
            xlab = "Fecha", ylab = "Horas de Absentismo IT", 
            main = "Comparación IT Marzo vs Mayo")
    legend("topright", 
           legend = c("IT Marzo", "IT Mayo", "Error Absoluto"), 
           col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
    grid()
  })
  
  output$metricas_marzo_mayo <- renderPrint({
    cat("Métricas de Error Marzo-Mayo:\n")
    cat("RMSE:", round(rmse_marzo_mayo, 2), "\n")
    cat("Error Absoluto Medio:", round(mean(error_abs_marzo_mayo), 2), "\n")
    cat("Error Absoluto Máximo:", round(max(error_abs_marzo_mayo), 2), "\n")
    cat("Error Absoluto Mínimo:", round(min(error_abs_marzo_mayo), 2), "\n")
  })
  
  output$plot_marzo_junio <- renderPlot({
    matplot(x = datos_marzo_junio$Fecha, 
            y = cbind(datos_marzo_junio$IT_marzo, datos_marzo_junio$IT_junio, error_abs_marzo_junio), 
            type = "l", lty = 1, lwd = 2, 
            col = c("#1f77b4", "#ff7f0e", "#2ca02c"), 
            xlab = "Fecha", ylab = "Horas de Absentismo IT", 
            main = "Comparación IT Marzo vs Junio")
    legend("topright", 
           legend = c("IT Marzo", "IT Junio", "Error Absoluto"), 
           col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
    grid()
  })
  
  output$metricas_marzo_junio <- renderPrint({
    cat("Métricas de Error Marzo-Junio:\n")
    cat("RMSE:", round(rmse_marzo_junio, 2), "\n")
    cat("Error Absoluto Medio:", round(mean(error_abs_marzo_junio), 2), "\n")
    cat("Error Absoluto Máximo:", round(max(error_abs_marzo_junio), 2), "\n")
    cat("Error Absoluto Mínimo:", round(min(error_abs_marzo_junio), 2), "\n")
  })
  
  output$plot_mayo_junio <- renderPlot({
    matplot(x = datos_marzo_junio$Fecha, 
            y = cbind(datos_marzo_junio$IT_mayo, datos_marzo_junio$IT_junio, error_abs_mayo_junio), 
            type = "l", lty = 1, lwd = 2, 
            col = c("#1f77b4", "#ff7f0e", "#2ca02c"), 
            xlab = "Fecha", ylab = "Horas de Absentismo IT", 
            main = "Comparación IT Mayo vs Junio")
    legend("topright", 
           legend = c("IT Mayo", "IT Junio", "Error Absoluto"), 
           col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1, lwd = 2, cex = 0.7)
    grid()
  })
  
  output$metricas_mayo_junio <- renderPrint({
    cat("Métricas de Error Mayo-Junio:\n")
    cat("RMSE:", round(rmse_mayo_junio, 2), "\n")
    cat("Error Absoluto Medio:", round(mean(error_abs_mayo_junio), 2), "\n")
    cat("Error Absoluto Máximo:", round(max(error_abs_mayo_junio), 2), "\n")
    cat("Error Absoluto Mínimo:", round(min(error_abs_mayo_junio), 2), "\n")
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
