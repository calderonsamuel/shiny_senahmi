library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(DT)
library(writexl)
library(tidyr)
library(readxl)

# setwd("C:\\Users\\Win7\\Documents\\samuel\\R\\Pruebas\\Shiny\\ver1-0")

estaciones <- read_xlsx("bundle/estaciones.xlsx")%>%
    mutate(files = paste0("bundle/", region, "_", estacion, ".csv"))

files <- estaciones$files

ui <- fluidPage(title = "Shiny app",
                
                theme = shinytheme("sandstone"),
                
                # Application title
                titlePanel("Descargar datos del SENAHMI", windowTitle = "titulo_pagina"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        
                        h3("Gráfico de dispersión"),
                        
                        h5("Esta sección permite crear un gráfico de dispersión, de una variable a seleccionar, a través del tiempo.", 
                           style="text-align:justified"),
                        h5("Se pueden especificar también las regiones, el periodo de tiempo a graficar (entre el 3 de noviembre de 1928 y el 31 de octubre del 2015) y elegir el título del gráfico.", 
                           style="text-align:justified"),
                        h5("Elige los datos que necesitas y presiona el botón Graficar.", 
                           style="text-align:justified"),
                        h5("En la pestaña Tabla se puede encontrar en formato de tabla la data graficada y descargarla.", 
                           style="text-align:justified"),
                        
                        checkboxGroupInput(inputId = "y", 
                                    label = "Variable",
                                    choices = c("Precipitacion Acumulada" = "prec_acum", 
                                                "Temperatura máxima" = "temp_max", 
                                                "Temperatura mínima" = "temp_min"), 
                                    selected = "prec_acum"),
                        
                        # Select variable for color "z" 
                        selectInput(inputId = "z",
                                    label = "Región",
                                    choices = sort(unique(estaciones$region)),
                                    multiple = TRUE, selected = "Lima"),
                        
                        #Select datarange
                        dateRangeInput("date", strong("Rango de fechas"), 
                                       start = "1999-01-01", end = "2001-12-31",
                                       min = "1928-11-03", max = "2015-10-31",
                                       startview = "decade",
                                       separator = "hasta"),
                        
                        # Enter text for plot title
                        textInput(inputId = "plot_title", 
                                  label = "Título del gráfico", 
                                  placeholder = "Introduce texto para el título", 
                                  value = ""),
                        
                        #Botón para actualizar datos
                        wellPanel(
                            actionButton(inputId = "refresh",
                                         label = "Graficar")
                        )
                    ),
                    
                    
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    id = "tabsetpanel",
                                    # Show a plot of the generated distribution                
                                    tabPanel(title = "Gráfico", 
                                             plotOutput(outputId = "scatterplot")),
                                             
                                    tabPanel(title = "Tabla",
                                             
                                             radioButtons(inputId = "filetype",
                                                         label = "Formato de descarga",
                                                         choices = c(
                                                             "TXT" = "txt",
                                                             "CSV" = "csv",
                                                             "Excel" = "xlsx"
                                                         ),
                                                         selected = "txt"),
                                             
                                             downloadButton("download_data",
                                                            label = "Descargar"),
                                             
                                             hr(),
                                             DT::dataTableOutput(outputId = "tabla")
                                    ),
                                    
                                    tabPanel(title = "FAQ",
                                             h2("Preguntas frecuentes"),
                                             hr(),
                                             h3("General"),
                                             hr(),
                                             h4("¿Cómo se creó esta app?"),
                                             p("La app se escribió en R utilizando el paquete shiny. 
                                               Combina la capacidad de análisis de R con la interactividad de Shiny"),
                                             h4("¿Para qué sirve la app?"),
                                             p("Puede ser utilizada como una alternativa a la descarga de datos proporcionada por la web se Senahmi. 
                                               En esta app no se requiere inscribirse como usuario ni ingresar un captcha para cada descarga."),
                                             h4("¿Qué información puedo encontrar en esta app?"),
                                             p("La información de precipitación acumulada, temperatura máxima y temperatura mínima registrada en el 
                                               día de la observación por cada una de las 293 estaciones del Senahmi a nivel nacional. 
                                               La observación más antigua se remonta al año 1928 y la más reciente al año 2015, siendo un total 
                                               de 87 años observados. Los valores perdidos de la data no han pasado por ningún proceso de imputación."),
                                             h4("¿Cómo puedo acceder a los resúmenes mensuales de las variables?"),
                                             p("Lamentablemente por el momento no es posible acceder a esta información. 
                                               En una futura versión de la app esto podría ser posible."),
                                             h4("¿Cuáles son las recomendaciones para descargar los datos?"),
                                             p("Antes de realizar la descarga se recomienda ver el total de entradas de la tabla generada. 
                                               De entre los tres formatos disponibles, se recomienda utilizar CSV por su compatibilidad con 
                                               los softwares estadísticos más usados. Se incluyó el formato Excel por tener un uso bastante 
                                               extendido; sin embargo, sólo se recomienda usar este formato si el total de entradas de la tabla 
                                               es como máximo 1 millón."),
                                             h4("¿Cómo puedo descargar el gráfico que generé?"),
                                             p("Por el momento esto se puede hacer haciendo click derecho en la imagen y seleccionando Guardar imagen."),
                                             h4("¿Cómo participó Senahmi en la creación de esta app?"),
                                             p("Senahmi no tuvo ninguna participación en la creación de esta app ni la respalda necesariamente. 
                                               Es producto de un trabajo individual que buscaba poner en práctica las habilidades de programación 
                                               con R del autor aprendidas en verano del 2019."),
                                             hr(),
                                             h3("Problemas"),
                                             hr(),
                                             h4("¿Por qué la app tarda tanto en cargar mi gráfico o tabla?"),
                                             p("Debido a la diversidad de opciones que la app permite escoger la información es procesada a través 
                                               de varios filtros antes de convertirse en gráfico y tabla. Mientras menor sea la cantidad de variables, 
                                               regiones y rango de fecha que se introducen en las opciones, mejor será el desempeño. La velocidad de 
                                               conexión a internet y la memoria RAM del equipo también podría estar relacionada a la lentitud de la app."),
                                             h4("¿Por qué no aparece información en todos los años de mi gráfico?"),
                                             p("La data del Senahmi tiene una gran cantidad de valores perdidos, o periodos de tiempo en los que alguna 
                                               estación no recogió información de ciertas variables. Es por ello que en ciertos periodos no hay información 
                                               a presentar."),
                                             h4("¿Por qué me sale un mensaje de error en lugar de un gráfico?"),
                                             p("La app funciona a través de instrucciones lógicas. Un mensaje de error se puede producir cuando (1) la 
                                               fecha de inicio seleccionada es posterior a la fecha de término, (2) cuando no se seleccionó ninguna de las 
                                               variables disponibles o (3) cuando la variable, región y rango de fechas seleccionadas no tienen información 
                                               disponible."),
                                             h4("¿Por qué no puedo borrar la región que seleccioné?"),
                                             p("En algunos dispositivos móviles se puede presentar este problema. Si esto sucede se recomienda simplemente 
                                               actualizar la página.")
                                    ),
                                    
                                    tabPanel(title = "Autor",
                                             hr(),
                                             h4("Samuel Enrique Calderon Serrano"),
                                             br("Bach. en Ciencia Política de la U. Antonio Ruiz de Montoya - Lima"),
                                             h6(br("Correo:"), "samuel.calderon@uarm.pe")
                                    )
                        )
                    )
                )
)

server <- function(input, output) {
    
    #En el servidor todas las variables son input reactivos al botón de actualizar, se leen sólo los csv 
    # de sólo las regiones seleccionadas por el usuario y se filtra según rango de fechas y variable
    
    data_filtrada <- eventReactive(
        input$refresh,
        {
            req(input$z)
            
            indice <- estaciones %>%
                dplyr::filter(region %in% input$z)
            
            data <- read_csv(file = indice$files[1], col_types = "Dcccd")
            if(length(indice$files) == 1){
                data <- data
            } else {
            for (i in 2:length(indice$files)) {
                data <- bind_rows(data, read_csv(file = indice$files[i], col_types = "Dcccd"))
                                            }
                }
            data %>%
                dplyr::filter(variable %in% input$y, fecha >= input$date[1] & fecha <= input$date[2])
            
        })
    
    plot_title <- eventReactive(
        input$refresh,{
            input$plot_title
        }
    )
    
    output$scatterplot <- renderPlot({
        
        plot <- ggplot(data = data_filtrada(), aes(x = fecha, y = valor, col = variable)) +
            geom_point(alpha = 0.4, size = 3)+
            facet_wrap(~region) +
            labs(x = "Fecha",
                 y = "Valor",
                 title = plot_title(),
                 color = "Región",
                 caption = "Elaboración propia con base en datos del Senahmi") +
            # ggtitle(plot_title()) +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5),
                  plot.caption = element_text(hjust = 0))
        
        plot
    })
    
    output$tabla <- DT::renderDataTable({
        datatable(data = spread(data_filtrada(), variable, valor),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(10, 25, 40),
                                 language = list(lengthMenu = "Mostrar _MENU_ entradas",
                                                 info ="Mostrando _START_ al _END_ de _TOTAL_ entradas",
                                                 search = "Buscar:")
                  ), 
                  rownames = FALSE)
    })
    
    #La función formato elige una función para escribir las tablas según la elección del usuario para su descarga
    formato <- function(data, filetype, file){
        if(filetype == "txt"){
            write_delim(x = data, 
                        path = file, 
                        col_names = TRUE, 
                        na = "")
        } else if(filetype == "csv"){
            write_csv(x = data, 
                      path = file, 
                      col_names = TRUE, 
                      na = "")
        } else if(filetype == "xlsx"){
            write_xlsx(x = data, path = file, format_headers = FALSE)
        }
    }
    
    nombre <- reactive({
        paste("data_senahmi.", input$filetype, sep = "")})
    
    output$download_data <- downloadHandler(filename = nombre,
                                            content = function(file) {
                                                data <- data_filtrada()
                                                formato(data, input$filetype, file)
                                            })
}

# Run the application 
shinyApp(ui = ui, server = server)