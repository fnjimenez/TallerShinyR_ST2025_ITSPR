# ===============================================================
# TALLER: DASHBOARDS SHINY EXPRESS - KPIs DE MANUFACTURA
# ITSPR - 15 de octubre de 2025
# Instructor: Mtro. Fausto Noé Jiménez
# ===============================================================

# INSTALACIÓN DE PAQUETES (ejecutar una sola vez)
# ===============================================================
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "lubridate",
  "scales",
  "shinyWidgets"
))

# ===============================================================
# EJERCICIO 1: PRIMERA APP INTERACTIVA
# Módulo 2 (08:45 - 09:15)
# Objetivo: Comprender la estructura básica de Shiny Express
# ===============================================================

# Archivo: ejercicio_1_basico.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Mi Primera App Industrial",
  sidebar = sidebar(
    h4("Control de Producción"),
    sliderInput("produccion", 
                "Unidades Producidas:", 
                min = 0, 
                max = 1000, 
                value = 500,
                step = 10),
    sliderInput("meta", 
                "Meta de Producción:", 
                min = 0, 
                max = 1000, 
                value = 800,
                step = 10)
  ),
  
  # Panel principal
  layout_columns(
    value_box(
      title = "Producción Actual",
      value = textOutput("produccion_texto"),
      showcase = bs_icon("gear-fill"),
      theme = "primary"
    ),
    value_box(
      title = "% de Meta Alcanzada",
      value = textOutput("porcentaje"),
      showcase = bs_icon("graph-up"),
      theme = value_box_theme(bg = "#28a745")
    ),
    value_box(
      title = "Estado",
      value = textOutput("estado"),
      showcase = bs_icon("clipboard-check"),
      theme = value_box_theme(bg = "#17a2b8")
    )
  ),
  
  card(
    card_header("Visualización de Progreso"),
    plotOutput("grafico_progreso")
  )
)

server <- function(input, output, session) {
  
  output$produccion_texto <- renderText({
    paste(input$produccion, "unidades")
  })
  
  output$porcentaje <- renderText({
    pct <- round((input$produccion / input$meta) * 100, 1)
    paste0(pct, "%")
  })
  
  output$estado <- renderText({
    pct <- (input$produccion / input$meta) * 100
    if (pct >= 100) {
      "✅ Meta Cumplida"
    } else if (pct >= 80) {
      "⚠️ Cerca de Meta"
    } else {
      "❌ Bajo Meta"
    }
  })
  
  output$grafico_progreso <- renderPlot({
    library(ggplot2)
    
    df <- data.frame(
      categoria = c("Producido", "Restante"),
      valor = c(min(input$produccion, input$meta), 
                max(0, input$meta - input$produccion))
    )
    
    ggplot(df, aes(x = "", y = valor, fill = categoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = c("Producido" = "#28a745", "Restante" = "#dc3545")) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
      ) +
      labs(fill = "Estado de Producción")
  })
}

shinyApp(ui, server)


# ===============================================================
# EJERCICIO 2: DASHBOARD DE CONTROL DE CALIDAD BÁSICO
# Módulo 2 (09:15 - 09:45)
# Objetivo: Trabajar con componentes interactivos
# ===============================================================

# Archivo: ejercicio_2_calidad.R

library(shiny)
library(bslib)
library(ggplot2)

ui <- page_navbar(
  title = "Control de Calidad",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel(
    title = "Inspección",
    layout_sidebar(
      sidebar = sidebar(
        h4("Parámetros de Calidad"),
        numericInput("inspeccionadas", "Piezas Inspeccionadas:", 100, min = 1),
        numericInput("defectuosas", "Piezas Defectuosas:", 5, min = 0),
        sliderInput("limite_aceptable", "% Límite Aceptable:", 
                    min = 0, max = 20, value = 5, step = 0.5),
        hr(),
        selectInput("turno", "Turno:",
                    choices = c("Matutino", "Vespertino", "Nocturno"))
      ),
      
      layout_columns(
        value_box(
          title = "Tasa de Defectos",
          value = textOutput("tasa_defectos"),
          showcase = bs_icon("exclamation-triangle-fill")
        ),
        value_box(
          title = "First Pass Yield",
          value = textOutput("fpy"),
          showcase = bs_icon("check-circle-fill")
        )
      ),
      
      card(
        card_header("Análisis de Calidad"),
        plotOutput("grafico_calidad", height = "300px")
      ),
      
      card(
        card_header("Resultado de Inspección"),
        verbatimTextOutput("resultado_inspeccion")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Cálculos reactivos
  tasa <- reactive({
    (input$defectuosas / input$inspeccionadas) * 100
  })
  
  fpy_valor <- reactive({
    ((input$inspeccionadas - input$defectuosas) / input$inspeccionadas) * 100
  })
  
  output$tasa_defectos <- renderText({
    paste0(round(tasa(), 2), "%")
  })
  
  output$fpy <- renderText({
    paste0(round(fpy_valor(), 2), "%")
  })
  
  output$grafico_calidad <- renderPlot({
    df <- data.frame(
      categoria = c("Buenas", "Defectuosas"),
      cantidad = c(input$inspeccionadas - input$defectuosas, input$defectuosas)
    )
    
    ggplot(df, aes(x = categoria, y = cantidad, fill = categoria)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = cantidad), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Buenas" = "#28a745", "Defectuosas" = "#dc3545")) +
      labs(title = paste("Turno:", input$turno),
           x = "", y = "Cantidad de Piezas") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$resultado_inspeccion <- renderText({
    resultado <- if (tasa() <= input$limite_aceptable) {
      paste0("✅ LOTE APROBADO\n\n",
             "La tasa de defectos (", round(tasa(), 2), "%) está dentro del límite aceptable (",
             input$limite_aceptable, "%).\n\n",
             "Piezas buenas: ", input$inspeccionadas - input$defectuosas, "\n",
             "Piezas defectuosas: ", input$defectuosas, "\n",
             "FPY: ", round(fpy_valor(), 2), "%")
    } else {
      paste0("❌ LOTE RECHAZADO\n\n",
             "La tasa de defectos (", round(tasa(), 2), "%) EXCEDE el límite aceptable (",
             input$limite_aceptable, "%).\n\n",
             "Acción requerida: Inspección 100% del lote\n",
             "Investigar causa raíz del problema")
    }
    resultado
  })
}

shinyApp(ui, server)


# ===============================================================
# EJERCICIO 3: CÁLCULO DE OEE EN TIEMPO REAL
# Módulo 3 (10:30 - 11:15)
# Objetivo: Implementar KPIs fundamentales de manufactura
# ===============================================================

# Archivo: ejercicio_3_oee.R

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

# Función para calcular OEE
calcular_oee <- function(disponibilidad, rendimiento, calidad) {
  oee <- disponibilidad * rendimiento * calidad
  
  categoria <- case_when(
    oee >= 0.85 ~ "World Class",
    oee >= 0.65 ~ "Bueno",
    oee >= 0.40 ~ "Regular",
    TRUE ~ "Deficiente"
  )
  
  color <- case_when(
    oee >= 0.85 ~ "#28a745",  # Verde
    oee >= 0.65 ~ "#ffc107",  # Amarillo
    oee >= 0.40 ~ "#fd7e14",  # Naranja
    TRUE ~ "#dc3545"          # Rojo
  )
  
  list(
    oee = oee * 100,
    categoria = categoria,
    color = color,
    disponibilidad = disponibilidad * 100,
    rendimiento = rendimiento * 100,
    calidad = calidad * 100
  )
}

ui <- page_sidebar(
  title = "Calculadora OEE - Overall Equipment Effectiveness",
  theme = bs_theme(bootswatch = "cosmo"),
  
  sidebar = sidebar(
    h4("⚙️ Parámetros de Producción"),
    
    h5("1. Disponibilidad"),
    numericInput("tiempo_planeado", "Tiempo Planeado (min):", 480, min = 1),
    numericInput("tiempo_downtime", "Tiempo de Paro (min):", 45, min = 0),
    
    hr(),
    h5("2. Rendimiento"),
    numericInput("unidades_producidas", "Unidades Producidas:", 450, min = 0),
    numericInput("tiempo_ciclo_ideal", "Tiempo de Ciclo Ideal (min):", 1, min = 0.1, step = 0.1),
    
    hr(),
    h5("3. Calidad"),
    numericInput("unidades_buenas", "Unidades Buenas:", 430, min = 0),
    
    hr(),
    actionButton("calcular", "Calcular OEE", class = "btn-primary", width = "100%")
  ),
  
  # Panel principal
  layout_columns(
    value_box(
      title = "OEE Total",
      value = textOutput("oee_valor"),
      showcase = bs_icon("speedometer2"),
      theme = "primary",
      height = "150px"
    ),
    value_box(
      title = "Clasificación",
      value = textOutput("oee_categoria"),
      showcase = bs_icon("award-fill"),
      height = "150px"
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Componentes OEE",
      layout_columns(
        value_box(
          title = "Disponibilidad",
          value = textOutput("disponibilidad_pct"),
          showcase = bs_icon("clock-fill"),
          theme = "info"
        ),
        value_box(
          title = "Rendimiento",
          value = textOutput("rendimiento_pct"),
          showcase = bs_icon("speedometer"),
          theme = "success"
        ),
        value_box(
          title = "Calidad",
          value = textOutput("calidad_pct"),
          showcase = bs_icon("gem"),
          theme = "warning"
        )
      ),
      plotlyOutput("grafico_componentes")
    ),
    
    nav_panel(
      "Análisis Detallado",
      card(
        card_header("Resultados del Cálculo"),
        verbatimTextOutput("analisis_detallado")
      ),
      card(
        card_header("Recomendaciones"),
        uiOutput("recomendaciones")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Cálculos reactivos
  resultados <- eventReactive(input$calcular, {
    # Calcular componentes
    tiempo_operativo <- input$tiempo_planeado - input$tiempo_downtime
    disponibilidad <- tiempo_operativo / input$tiempo_planeado
    
    tiempo_ciclo_real <- tiempo_operativo / input$unidades_producidas
    rendimiento <- input$tiempo_ciclo_ideal / tiempo_ciclo_real
    
    calidad <- input$unidades_buenas / input$unidades_producidas
    
    # Calcular OEE
    calcular_oee(disponibilidad, rendimiento, calidad)
  })
  
  # Outputs
  output$oee_valor <- renderText({
    paste0(round(resultados()$oee, 1), "%")
  })
  
  output$oee_categoria <- renderText({
    resultados()$categoria
  })
  
  output$disponibilidad_pct <- renderText({
    paste0(round(resultados()$disponibilidad, 1), "%")
  })
  
  output$rendimiento_pct <- renderText({
    paste0(round(resultados()$rendimiento, 1), "%")
  })
  
  output$calidad_pct <- renderText({
    paste0(round(resultados()$calidad, 1), "%")
  })
  
  output$grafico_componentes <- renderPlotly({
    df <- data.frame(
      Componente = c("Disponibilidad", "Rendimiento", "Calidad", "OEE Total"),
      Valor = c(resultados()$disponibilidad, 
                resultados()$rendimiento, 
                resultados()$calidad,
                resultados()$oee)
    )
    
    plot_ly(df, x = ~Componente, y = ~Valor, type = 'bar',
            marker = list(color = c('#17a2b8', '#28a745', '#ffc107', resultados()$color))) %>%
      layout(
        title = "Desglose de OEE",
        yaxis = list(title = "Porcentaje (%)", range = c(0, 100)),
        xaxis = list(title = "")
      )
  })
  
  output$analisis_detallado <- renderText({
    paste0(
      "═══════════════════════════════════════════\n",
      "REPORTE DE EFICIENCIA GENERAL DEL EQUIPO (OEE)\n",
      "═══════════════════════════════════════════\n\n",
      "📊 RESULTADO FINAL:\n",
      "   OEE = ", round(resultados()$oee, 2), "% (", resultados()$categoria, ")\n\n",
      "📈 COMPONENTES:\n",
      "   Disponibilidad = ", round(resultados()$disponibilidad, 2), "%\n",
      "   Rendimiento    = ", round(resultados()$rendimiento, 2), "%\n",
      "   Calidad        = ", round(resultados()$calidad, 2), "%\n\n",
      "⏰ TIEMPOS:\n",
      "   Planeado      = ", input$tiempo_planeado, " min\n",
      "   Downtime      = ", input$tiempo_downtime, " min\n",
      "   Operativo     = ", input$tiempo_planeado - input$tiempo_downtime, " min\n\n",
      "📦 PRODUCCIÓN:\n",
      "   Producidas    = ", input$unidades_producidas, " unidades\n",
      "   Buenas        = ", input$unidades_buenas, " unidades\n",
      "   Defectuosas   = ", input$unidades_producidas - input$unidades_buenas, " unidades\n\n",
      "🎯 BENCHMARKS:\n",
      "   World Class   ≥ 85%\n",
      "   Bueno         ≥ 65%\n",
      "   Regular       ≥ 40%\n",
      "   Deficiente    < 40%\n"
    )
  })
  
  output$recomendaciones <- renderUI({
    oee <- resultados()$oee
    disp <- resultados()$disponibilidad
    rend <- resultados()$rendimiento
    cal <- resultados()$calidad
    
    recomendaciones <- c()
    
    if (disp < 90) {
      recomendaciones <- c(recomendaciones, 
        "🔧 <b>DISPONIBILIDAD BAJA:</b> Implementar TPM (Mantenimiento Productivo Total)")
    }
    if (rend < 95) {
      recomendaciones <- c(recomendaciones,
        "⚡ <b>RENDIMIENTO BAJO:</b> Analizar cuellos de botella y optimizar tiempos de ciclo")
    }
    if (cal < 99) {
      recomendaciones <- c(recomendaciones,
        "✅ <b>CALIDAD MEJORABLE:</b> Aplicar metodología Six Sigma y control estadístico")
    }
    if (oee >= 85) {
      recomendaciones <- c(recomendaciones,
        "🏆 <b>EXCELENTE:</b> Mantener este nivel y documentar mejores prácticas")
    }
    
    HTML(paste0("<ul><li>", paste(recomendaciones, collapse = "</li><li>"), "</li></ul>"))
  })
}

shinyApp(ui, server)


# ===============================================================
# EJERCICIO 4: DASHBOARD COMPLETO INTEGRADOR
# Módulo 4 (11:45 - 12:30)
# Objetivo: Crear un dashboard integral con múltiples KPIs
# ===============================================================

# Archivo: ejercicio_4_dashboard_completo.R

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)

# Generar datos de ejemplo
generar_datos <- function() {
  set.seed(123)
  
  horas <- seq(from = as.POSIXct("2025-10-15 08:00"), 
               to = as.POSIXct("2025-10-15 16:00"), 
               by = "hour")
  
  data.frame(
    hora = horas,
    oee = runif(length(horas), 70, 95),
    disponibilidad = runif(length(horas), 85, 98),
    rendimiento = runif(length(horas), 80, 95),
    calidad = runif(length(horas), 92, 99),
    unidades = sample(80:120, length(horas), replace = TRUE),
    defectos = sample(0:8, length(horas), replace = TRUE)
  )
}

# Datos de incidencias
generar_incidencias <- function() {
  data.frame(
    id = 1:10,
    hora = sample(8:16, 10, replace = TRUE),
    causa = sample(c("Falla Mecánica", "Falta de Material", 
                     "Cambio de Producto", "Ajuste de Calidad", "Falla Eléctrica"), 
                   10, replace = TRUE),
    duracion = sample(5:30, 10, replace = TRUE),
    linea = sample(c("Línea A", "Línea B", "Línea C"), 10, replace = TRUE)
  )
}

ui <- page_navbar(
  title = "Dashboard de Manufactura - Línea de Producción A",
  theme = bs_theme(
    bootswatch = "pulse",
    primary = "#0066cc"
  ),
  
  nav_panel(
    title = "Panel Principal",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Filtros de Control"),
        dateInput("fecha", "Fecha:", value = Sys.Date()),
        selectInput("turno", "Turno:",
                    choices = c("Todos", "Matutino", "Vespertino", "Nocturno"),
                    selected = "Todos"),
        selectInput("linea", "Línea de Producción:",
                    choices = c("Línea A", "Línea B", "Línea C"),
                    selected = "Línea A"),
        hr(),
        h5("Actualización Automática"),
        checkboxInput("auto_refresh", "Activar actualización", value = FALSE),
        conditionalPanel(
          condition = "input.auto_refresh == true",
          sliderInput("intervalo", "Intervalo (seg):", 
                      min = 5, max = 60, value = 10)
        ),
        hr(),
        actionButton("refresh", "🔄 Actualizar Datos", 
                     class = "btn-primary", width = "100%")
      ),
      
      # KPIs Principales
      layout_columns(
        value_box(
          title = "OEE Actual",
          value = textOutput("kpi_oee"),
          showcase = bs_icon("speedometer2"),
          theme = "primary"
        ),
        value_box(
          title = "MTBF",
          value = textOutput("kpi_mtbf"),
          showcase = bs_icon("clock-history"),
          theme = "info"
        ),
        value_box(
          title = "FPY",
          value = textOutput("kpi_fpy"),
          showcase = bs_icon("check-circle"),
          theme = "success"
        ),
        value_box(
          title = "Producción Hora",
          value = textOutput("kpi_produccion"),
          showcase = bs_icon("box-seam"),
          theme = "warning"
        )
      ),
      
      # Gráficos
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Tendencia de OEE por Hora"),
          plotlyOutput("grafico_tendencia", height = "300px")
        ),
        card(
          card_header("Nivel de Cumplimiento"),
          plotOutput("gauge_oee", height = "300px")
        )
      ),
      
      layout_columns(
        card(
          card_header("Causas de Downtime (Pareto)"),
          plotlyOutput("pareto_downtime", height = "300px")
        ),
        card(
          card_header("Componentes OEE"),
          plotOutput("componentes_oee", height = "300px")
        )
      )
    )
  ),
  
  nav_panel(
    title = "Incidencias",
    card(
      card_header("Registro de Incidencias del Día"),
      DTOutput("tabla_incidencias")
    )
  ),
  
  nav_panel(
    title = "Análisis",
    layout_columns(
      card(
        card_header("Resumen Estadístico"),
        verbatimTextOutput("resumen_stats")
      ),
      card(
        card_header("Alertas y Recomendaciones"),
        uiOutput("alertas")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Datos reactivos
  datos <- reactiveVal(generar_datos())
  incidencias <- reactiveVal(generar_incidencias())
  
  # Actualización automática
  observe({
    if (input$auto_refresh) {
      invalidateLater(input$intervalo * 1000)
      datos(generar_datos())
      incidencias(generar_incidencias())
    }
  })
  
  # Actualización manual
  observeEvent(input$refresh, {
    datos(generar_datos())
    incidencias(generar_incidencias())
  })
  
  # KPIs
  output$kpi_oee <- renderText({
    paste0(round(mean(datos()$oee), 1), "%")
  })
  
  output$kpi_mtbf <- renderText({
    paste0(round(mean(480 / nrow(incidencias())), 1), " hrs")
  })
  
  output$kpi_fpy <- renderText({
    total_unidades <- sum(datos()$unidades)
    total_defectos <- sum(datos()$defectos)
    fpy <- ((total_unidades - total_defectos) / total_unidades) * 100
    paste0(round(fpy, 1), "%")
  })
  
  output$kpi_produccion <- renderText({
    paste0(round(mean(datos()$unidades), 0), " und/h")
  })
  
  # Gráfico de tendencia
  output$grafico_tendencia <- renderPlotly({
    df <- datos()
    
    plot_ly(df, x = ~hora, y = ~oee, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#0066cc', width = 3),
            marker = list(size = 8, color = '#0066cc')) %>%
      add_trace(y = 85, name = 'Meta World Class', 
                line = list(dash = 'dash', color = 'green')) %>%
      layout(
        xaxis = list(title = "Hora"),
        yaxis = list(title = "OEE (%)", range = c(0, 100)),
        hovermode = 'x unified'
      )
  })
  
  # Gauge OEE
  output$gauge_oee <- renderPlot({
    oee_actual <- mean(datos()$oee)
    
    ggplot(data.frame(x = 1), aes(x, y = oee_actual)) +
      geom_col(width = 0.5, fill = ifelse(oee_actual >= 85, "#28a745", 
                                          ifelse(oee_actual >= 65, "#ffc107", "#dc3545"))) +
      coord_polar(theta = "y") +
      ylim(0, 100) +
      geom_text(aes(label = paste0(round(oee_actual, 1), "%")), 
                size = 12, fontface = "bold") +
      theme_void() +
      theme(plot.margin = margin(20, 20, 20, 20))
  })
  
  # Pareto de downtime
  output$pareto_downtime <- renderPlotly({
    df <- incidencias() %>%
      group_by(causa) %>%
      summarise(tiempo_total = sum(duracion)) %>%
      arrange(desc(tiempo_total)) %>%
      mutate(
        porcentaje = tiempo_total / sum(tiempo_total) * 100,
        acumulado = cumsum(porcentaje)
      )
    
    plot_ly(df) %>%
      add_bars(x = ~causa, y = ~tiempo_total, name = "Tiempo (min)",
               marker = list(color = '#dc3545')) %>%
      add_lines(x = ~causa, y = ~acumulado, name = "% Acumulado",
                yaxis = "y2", line = list(color = '#0066cc', width = 3)) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Tiempo (min)"),
        yaxis2 = list(title = "% Acumulado", overlaying = "y", side = "right"),
        hovermode = 'x unified'
      )
  })
  
  # Componentes OEE
  output$componentes_oee <- renderPlot({
    df <- datos() %>%
      summarise(
        Disponibilidad = mean(disponibilidad),
        Rendimiento = mean(rendimiento),
        Calidad = mean(calidad)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Componente", values_to = "Valor")
    
    ggplot(df, aes(x = Componente, y = Valor, fill = Componente)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Valor, 1), "%")), 
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("#17a2b8", "#28a745", "#ffc107")) +
      ylim(0, 100) +
      theme_minimal() +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 12, face = "bold"))
  })
  
  # Tabla de incidencias
  output$tabla_incidencias <- renderDT({
    incidencias() %>%
      arrange(desc(duracion)) %>%
      datatable(
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE
      ) %>%
      formatStyle('duracion',
                  backgroundColor = styleInterval(c(10, 20), 
                                                  c('#d4edda', '#fff3cd', '#f8d7da')))
  })
  
  # Resumen estadístico
  output$resumen_stats <- renderText({
    df <- datos()
    paste0(
      "══════════════════════════════════════\n",
      "RESUMEN ESTADÍSTICO DEL TURNO\n",
      "══════════════════════════════════════\n\n",
      "📊 OEE:\n",
      "   Media    = ", round(mean(df$oee), 2), "%\n",
      "   Máximo   = ", round(max(df$oee), 2), "%\n",
      "   Mínimo   = ", round(min(df$oee), 2), "%\n",
      "   Desv.Est = ", round(sd(df$oee), 2), "\n\n",
      "📦 PRODUCCIÓN:\n",
      "   Total    = ", sum(df$unidades), " unidades\n",
      "   Promedio = ", round(mean(df$unidades), 1), " und/hora\n",
      "   Defectos = ", sum(df$defectos), " unidades\n\n",
      "⏱️ INCIDENCIAS:\n",
      "   Total    = ", nrow(incidencias()), " eventos\n",
      "   Tiempo   = ", sum(incidencias()$duracion), " minutos\n",
      "   MTTR     = ", round(mean(incidencias()$duracion), 1), " min\n"
    )
  })
  
  # Alertas
  output$alertas <- renderUI({
    oee_actual <- mean(datos()$oee)
    fpy <- ((sum(datos()$unidades) - sum(datos()$defectos)) / sum(datos()$unidades)) * 100
    
    alertas <- list()
    
    if (oee_actual < 65) {
      alertas <- c(alertas, list(
        div(class = "alert alert-danger", 
            strong("⚠️ OEE BAJO:"), " El OEE está por debajo del estándar. Requiere atención inmediata.")
      ))
    }
    
    if (fpy < 95) {
      alertas <- c(alertas, list(
        div(class = "alert alert-warning",
            strong("⚠️ CALIDAD:"), " El FPY está por debajo de 95%. Revisar proceso.")
      ))
    }
    
    if (nrow(incidencias()) > 8) {
      alertas <- c(alertas, list(
        div(class = "alert alert-warning",
            strong("⚠️ ALTO NÚMERO DE INCIDENCIAS:"), " Más de 8 eventos registrados.")
      ))
    }
    
    if (oee_actual >= 85) {
      alertas <- c(alertas, list(
        div(class = "alert alert-success",
            strong("✅ EXCELENTE:"), " OEE World Class alcanzado. ¡Felicidades al equipo!")
      ))
    }
    
    if (length(alertas) == 0) {
      alertas <- list(div(class = "alert alert-info", "ℹ️ Sin alertas por el momento"))
    }
    
    tagList(alertas)
  })
}

shinyApp(ui, server)