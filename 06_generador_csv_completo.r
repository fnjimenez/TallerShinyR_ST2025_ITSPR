# ===================================================================
# GENERADOR COMPLETO DE DATASETS PARA TALLER SHINY EXPRESS
# ITSPR - Semana Tecnológica 2025
# ===================================================================

# Instrucciones:
# 1. Ejecutar este script completo en R o Posit.Cloud
# 2. Los 3 archivos CSV se crearán automáticamente
# 3. Verificar que los archivos existen en el directorio de trabajo

library(dplyr)
library(lubridate)

# Establecer semilla para reproducibilidad
set.seed(123)

cat("🚀 GENERANDO DATASETS PARA EL TALLER...\n\n")

# ===================================================================
# DATASET 1: LÍNEA DE PRODUCCIÓN A (1,000 registros)
# ===================================================================

cat("📊 Generando Dataset 1: Línea de Producción A...\n")

# Generar secuencia de fechas (1000 horas)
fechas <- seq(
  from = as.POSIXct("2025-09-15 08:00"),
  by = "hour",
  length.out = 1000
)

# Función para determinar turno
determinar_turno <- function(hora) {
  h <- hour(hora)
  case_when(
    h >= 6 & h < 14 ~ "Matutino",
    h >= 14 & h < 22 ~ "Vespertino",
    TRUE ~ "Nocturno"
  )
}

# Causas de downtime
causas <- c(
  "Falla Mecánica", 
  "Falta de Material", 
  "Cambio de Producto",
  "Ajuste de Calidad", 
  "Falla Eléctrica", 
  "Mantenimiento Preventivo",
  "Espera de Operador"
)

# Generar dataset
linea_produccion <- data.frame(
  fecha = fechas,
  turno = sapply(fechas, determinar_turno),
  unidades_planeadas = sample(90:110, 1000, replace = TRUE)
) %>%
  mutate(
    unidades_producidas = pmin(unidades_planeadas, sample(80:108, 1000, replace = TRUE)),
    unidades_defectuosas = pmax(0, round(unidades_producidas * runif(1000, 0.01, 0.08))),
    tiempo_downtime = sample(0:45, 1000, replace = TRUE),
    causa_downtime = ifelse(tiempo_downtime == 0, 
                            "Sin Downtime",
                            sample(causas, 1000, replace = TRUE))
  )

# Guardar CSV
write.csv(linea_produccion, "linea_produccion_a.csv", row.names = FALSE)
cat("✅ Archivo creado: linea_produccion_a.csv\n")
cat("   - Registros:", nrow(linea_produccion), "\n")
cat("   - Período:", min(linea_produccion$fecha), "a", max(linea_produccion$fecha), "\n\n")

# ===================================================================
# DATASET 2: REGISTRO DE MANTENIMIENTO (500 registros)
# ===================================================================

cat("🔧 Generando Dataset 2: Registro de Mantenimiento...\n")

# Equipos disponibles
equipos <- c(
  "Prensa-01", "Prensa-02", 
  "Robot-A1", "Robot-A2",
  "Transportador-T1", "Transportador-T2", 
  "Ensambladora-E1", "Ensambladora-E2"
)

# Tipos de fallas
tipos_falla <- c(
  "Falla Hidráulica",
  "Falla Eléctrica",
  "Desgaste de Componente",
  "Sobrecalentamiento",
  "Falla de Sensor",
  "Desalineación",
  "Fuga de Aceite",
  "Error de Software"
)

# Fecha base
fecha_base <- as.POSIXct("2025-01-01 00:00:00")

# Generar dataset de mantenimiento
registro_mantenimiento <- data.frame(
  equipo_id = sample(equipos, 500, replace = TRUE),
  fecha_falla = fecha_base + sample(0:(280*24*3600), 500),
  tipo_falla = sample(tipos_falla, 500, replace = TRUE),
  tiempo_reparacion = sample(15:240, 500, replace = TRUE),
  costo = sample(500:15000, 500, replace = TRUE)
) %>%
  mutate(
    fecha_reparacion = fecha_falla + minutes(tiempo_reparacion)
  ) %>%
  select(equipo_id, fecha_falla, fecha_reparacion, tipo_falla, tiempo_reparacion, costo) %>%
  arrange(fecha_falla)

# Guardar CSV
write.csv(registro_mantenimiento, "registro_mantenimiento.csv", row.names = FALSE)
cat("✅ Archivo creado: registro_mantenimiento.csv\n")
cat("   - Registros:", nrow(registro_mantenimiento), "\n")
cat("   - MTTR promedio:", round(mean(registro_mantenimiento$tiempo_reparacion), 1), "minutos\n")
cat("   - Costo promedio:", round(mean(registro_mantenimiento$costo), 0), "pesos\n\n")

# ===================================================================
# DATASET 3: CONTROL DE CALIDAD (2,000 registros)
# ===================================================================

cat("✅ Generando Dataset 3: Control de Calidad...\n")

# Productos
productos <- c("Producto-A", "Producto-B", "Producto-C")

# Especificaciones por producto
specs <- list(
  "Producto-A" = c(min = 98, max = 102),
  "Producto-B" = c(min = 45, max = 55),
  "Producto-C" = c(min = 19.5, max = 20.5)
)

# Función para generar mediciones
generar_mediciones <- function(producto, dentro_spec = TRUE) {
  spec <- specs[[producto]]
  
  if (dentro_spec) {
    # 96.5% dentro de especificación
    c(
      runif(1, spec["min"] + 0.2, spec["max"] - 0.2),
      runif(1, spec["min"] + 0.2, spec["max"] - 0.2),
      runif(1, spec["min"] + 0.2, spec["max"] - 0.2)
    )
  } else {
    # Fuera de especificación
    c(
      runif(1, spec["min"] - 2, spec["max"] + 2),
      runif(1, spec["min"] - 2, spec["max"] + 2),
      runif(1, spec["min"] - 2, spec["max"] + 2)
    )
  }
}

# Generar dataset base
control_calidad <- data.frame(
  lote = paste0("L-", 1000:2999),
  fecha = as.Date("2025-01-01") + sample(0:280, 2000, replace = TRUE),
  producto = sample(productos, 2000, replace = TRUE)
) %>%
  rowwise() %>%
  mutate(
    dentro_spec = runif(1) < 0.965,
    temp_med = list(generar_mediciones(producto, dentro_spec)),
    medicion_1 = round(temp_med[[1]], 2),
    medicion_2 = round(temp_med[[2]], 2),
    medicion_3 = round(temp_med[[3]], 2)
  ) %>%
  ungroup() %>%
  mutate(
    especificacion_min = case_when(
      producto == "Producto-A" ~ 98,
      producto == "Producto-B" ~ 45,
      producto == "Producto-C" ~ 19.5
    ),
    especificacion_max = case_when(
      producto == "Producto-A" ~ 102,
      producto == "Producto-B" ~ 55,
      producto == "Producto-C" ~ 20.5
    ),
    resultado = ifelse(
      medicion_1 >= especificacion_min & medicion_1 <= especificacion_max &
      medicion_2 >= especificacion_min & medicion_2 <= especificacion_max &
      medicion_3 >= especificacion_min & medicion_3 <= especificacion_max,
      "Aprobado", 
      "Rechazado"
    )
  ) %>%
  select(-dentro_spec, -temp_med) %>%
  arrange(fecha, lote)

# Guardar CSV
write.csv(control_calidad, "control_calidad.csv", row.names = FALSE)
cat("✅ Archivo creado: control_calidad.csv\n")
cat("   - Registros:", nrow(control_calidad), "\n")
cat("   - Aprobados:", sum(control_calidad$resultado == "Aprobado"), "\n")
cat("   - Rechazados:", sum(control_calidad$resultado == "Rechazado"), "\n")
cat("   - FPY:", round(sum(control_calidad$resultado == "Aprobado")/nrow(control_calidad)*100, 2), "%\n\n")

# ===================================================================
# RESUMEN FINAL
# ===================================================================

cat("══════════════════════════════════════════════════════\n")
cat("✅ GENERACIÓN COMPLETADA CON ÉXITO\n")
cat("══════════════════════════════════════════════════════\n\n")

cat("📁 Archivos creados:\n")
cat("   1. linea_produccion_a.csv     (1,000 registros)\n")
cat("   2. registro_mantenimiento.csv   (500 registros)\n")
cat("   3. control_calidad.csv        (2,000 registros)\n\n")

cat("📍 Ubicación: ", getwd(), "\n\n")

cat("🎯 Siguiente paso:\n")
cat("   - Verificar que los archivos existan\n")
cat("   - Abrir en Excel/Calc para revisar\n")
cat("   - Usar en los ejercicios del taller\n\n")

cat("💡 Para verificar archivos, ejecuta:\n")
cat("   list.files(pattern = '\\\\.csv$')\n\n")

# Verificación automática
archivos_creados <- list.files(pattern = "\\.csv$")
if (length(archivos_creados) == 3) {
  cat("✅ VERIFICACIÓN: Los 3 archivos CSV están presentes\n")
} else {
  cat("⚠️ ADVERTENCIA: Solo se detectaron", length(archivos_creados), "archivos CSV\n")
}

cat("\n🚀 ¡Datasets listos para el taller del 15 de octubre!\n")
cat("══════════════════════════════════════════════════════\n")