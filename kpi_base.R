# Instalación de librerías si no las tienes:
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "DT"))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# 1. Simulación de Datos
set.seed(123)
datos_seguros <- data.frame(
  Mes = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
  Primas_Emitidas = runif(12, 500000, 800000),
  Siniestros = runif(12, 300000, 500000),
  Gastos_Op = runif(12, 100000, 150000)
) %>%
  mutate(
    Siniestralidad = (Siniestros / Primas_Emitidas) * 100,
    Ratio_Gastos = (Gastos_Op / Primas_Emitidas) * 100,
    Ratio_Combinado = Siniestralidad + Ratio_Gastos
  )

# 2. Interfaz de Usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "KPIs Compañía de Seguros"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Principal", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Tabla de Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("box_siniestralidad"),
                valueBoxOutput("box_combinado"),
                valueBoxOutput("box_primas")
              ),
              fluidRow(
                box(title = "Evolución de Ratios", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_ratios"), width = 8),
                box(title = "Distribución de Costos", status = "warning", solidHeader = TRUE,
                    plotOutput("plot_pie"), width = 4)
              )
      ),
      tabItem(tabName = "datos",
              DTOutput("tabla_kpis")
      )
    )
  )
)

# 3. Lógica del Servidor (Server)
server <- function(input, output) {
  
  # KPI: Siniestralidad Media
  output$box_siniestralidad <- renderValueBox({
    avg_sin <- mean(datos_seguros$Siniestralidad)
    valueBox(paste0(round(avg_sin, 1), "%"), "Siniestralidad Promedio", 
             icon = icon("ambulance"), color = "red")
  })
  
  # KPI: Ratio Combinado Medio
  output$box_combinado <- renderValueBox({
    avg_comb <- mean(datos_seguros$Ratio_Combinado)
    valueBox(paste0(round(avg_comb, 1), "%"), "Ratio Combinado", 
             icon = icon("balance-scale"), color = if(avg_comb > 100) "orange" else "green")
  })
  
  # KPI: Total Primas
  output$box_primas <- renderValueBox({
    total_p <- sum(datos_seguros$Primas_Emitidas) / 1e6
    valueBox(paste0("$", round(total_p, 2), "M"), "Total Primas Emitidas", 
             icon = icon("money-bill-wave"), color = "blue")
  })
  
  # Gráfico de Líneas
  output$plot_ratios <- renderPlot({
    ggplot(datos_seguros, aes(x = Mes)) +
      geom_line(aes(y = Siniestralidad, color = "Siniestralidad"), size = 1.2) +
      geom_line(aes(y = Ratio_Combinado, color = "Ratio Combinado"), size = 1.2) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
      labs(y = "Porcentaje (%)", x = "Mes", color = "Indicador") +
      theme_minimal()
  })
  
  # Gráfico de Pastel (Último mes)
  output$plot_pie <- renderPlot({
    ultimo <- tail(datos_seguros, 1)
    df_pie <- data.frame(
      Cat = c("Siniestros", "Gastos", "Margen Técnico"),
      Val = c(ultimo$Siniestros, ultimo$Gastos_Op, ultimo$Primas_Emitidas - (ultimo$Siniestros + ultimo$Gastos_Op))
    )
    ggplot(df_pie, aes(x = "", y = Val, fill = Cat)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Composición de la Prima")
  })
  
  output$tabla_kpis <- renderDT({
    datatable(datos_seguros, options = list(pageLength = 12))
  })
}

shinyApp(ui, server)