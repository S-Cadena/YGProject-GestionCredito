#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
source('DataUpload.R')

# Define UI for application that draws a charts and tables
ui <- fluidPage(
  
  useShinydashboard(),

  # Application title
  titlePanel('Gestión de Crédito Mensual Al Día'),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'sctMonto',
        label = strong('Monto'),
        choices = my_num_cols,
        selected = 'INCREMENTO'
      ),
      pickerInput(
        inputId = 'pckTipoIncremento',
        label = strong('Tipo de incremento'),
        choices = my_mont_type_lst,
        selected = my_mont_type_lst,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      dateRangeInput(
        inputId = 'drgRangoFecha',
        label = strong('Rango de fecha'),
        start = my_date_start,
        end = my_date_end,
        min = my_date_min,
        max = my_date_max,
        format = 'dd-M-yy',
        language = 'es',
        separator = ' - '
      ),
      
      pickerInput(
        inputId = 'pckSemana',
        label = strong('Semana'),
        choices = my_week_lst,
        selected = my_week_lst,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = 'pckAno',
        label = strong('Año de la semana'),
        choices = my_year_lst,
        selected = my_year_lst,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        valueBoxOutput(outputId = 'MontoSumBox'),
        valueBoxOutput(outputId = 'MontoMeanBox'),
        valueBoxOutput(outputId = 'MovimientosSumBox')
      ),
      plotlyOutput(outputId = 'SeasonlPlot'),
      dataTableOutput('IndexTable')
    )
    
  )
  
)

# Define server logic required to draw charts and tables
server <- function(input, output) {

  my_date_rang <- reactive({
    seq(
      input$drgRangoFecha[1],
      input$drgRangoFecha[2],
      by = 'day'
    )
  })
  
  fecha_ds <- reactive({
    my_dataset %>% filter(
      `TIPO INCREMENTO` %in% input$pckTipoIncremento,
      FECHA %in% my_date_rang(),
      Semana %in% input$pckSemana,
      Ano %in% input$pckAno
    ) %>% group_by(
      FECHA, Dia, Semana, Ano
    ) %>% summarize(
      across(
        where('is.numeric'),
        ~.x %>% sum(na.rm = TRUE)
      ),
      MOVIMIENTOS = n()
    ) %>% arrange(
      FECHA
    ) %>% ungroup(
    )
  })
  
  index_ds <- reactive({
    
    x <- my_dataset %>% filter(
      FECHA %in% my_date_rang(),
      Semana %in% input$pckSemana,
      Ano %in% input$pckAno
    ) %>% group_by(
      `TIPO INCREMENTO`
    ) %>% select(
      where('is.numeric')
    ) %>% summarise(
      across(
        contains('INCREMENTO'),
        ~.x %>% sum(na.rm = TRUE)
      ),
      TX = n()
    ) %>% transmute(
      `TIPO DE INCREMENTO` = `TIPO INCREMENTO`,
      MENSUAL = INCREMENTO,
      `MENSUAL TX` = TX,
      `MENSUAL [PER DAY]` = MENSUAL / length(my_date_rang()),
      `MENSUAL TX [PER DAY]` = TX / length(my_date_rang())
    )
    
    y <- my_dataset %>% filter(
      FECHA == my_report_day
    ) %>% group_by(
      `TIPO INCREMENTO`
    ) %>% select(
      where('is.numeric')
    ) %>% summarise(
      across(
        contains('INCREMENTO'),
        ~.x %>% sum(na.rm = TRUE)
      ),
      TX = n()
    ) %>% transmute(
      `TIPO DE INCREMENTO` = `TIPO INCREMENTO`,
      DIARIO = INCREMENTO,
      `DIARIO TX` = TX
    )
    
    z <- x %>% inner_join(
      y,
      by = 'TIPO DE INCREMENTO'
    ) %>% mutate(
      `INDEX` = round(DIARIO / `MENSUAL [PER DAY]`, 2),
      `INDEX TX` = round(`DIARIO TX` / `MENSUAL TX [PER DAY]`, 2)
    )
    
    z %>% datatable(
    ) %>% formatCurrency(
      columns = 'MENSUAL'
    )
    
  })
  
  output$MontoSumBox <- renderValueBox({
    
    m <- fecha_ds() 
    m <- m[[input$sctMonto]]
    m <- m %>% sum() %>% scales::dollar()
    valueBox(
      value = m,
      subtitle = input$sctMonto %>% str_to_title() %>% str_c(': Monto Total')
    )
  })
  
  output$MontoMeanBox <- renderValueBox({
    m <- fecha_ds()
    m <- m[[input$sctMonto]]
    m <- m %>% mean() %>% scales::dollar()
    valueBox(
      value = ifelse(
        m %>% is.na(),
        scales::dollar(0),
        m
      ),
      subtitle = input$sctMonto %>% str_to_title() %>% str_c(': Monto Promedio')
    )
  })
  
  output$MovimientosSumBox <- renderValueBox({
    m <- fecha_ds()
    m <- m[['MOVIMIENTOS']]
    m <- m %>% sum() %>% scales::comma()
    valueBox(
      value = m,
      subtitle = input$sctMonto %>% str_to_title() %>% str_c(': Total de Movimientos')
    )
  })
  
  output$SeasonlPlot <- renderPlotly({
    fecha_ds() %>% plot_ly(
      x = ~Dia,
      y = ~get(input$sctMonto),
      type = 'scatter',
      mode = 'lines+markers',
      color = ~Semana,
      colors = my_week_pal,
      hoverinfo = 'text',
      text = ~str_c(
        '</br> Fecha: ', FECHA,
        '</br> Día: ', Dia,
        '</br> ', str_to_title(input$sctMonto), ': ', scales::dollar(get(input$sctMonto))
      )
    ) %>% layout(
      xaxis = list(title = 'Día de la semana'),
      yaxis = list(title = str_to_title(input$sctMonto))
    )
  })
  
  output$IndexTable <- renderDataTable(
    index_ds()
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
