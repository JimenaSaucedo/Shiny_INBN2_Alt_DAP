library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
df <- read.table("C:\\Users\\jimena\\Dropbox\\Datos 2INBN (1)\\Tablas y Scripts 2INBN\\Tablas actualizadas\\MONTE\\MON_Individuos_SEPTIEMBRE_2020.csv",sep = ",", header = T)
df2 <- read.table("C:/Users/jimena/Dropbox/Datos 2INBN (1)/Tablas y Scripts 2INBN/Tablas actualizadas/MONTE/ESTADISTICAS/AREA BASAL/tabla_AB_MON.csv", sep =",", header = T)
sp <- df2 %>% distinct(especie_corregida) %>% 
  arrange(especie_corregida) %>% 
  mutate(especie_corregida = as.character(especie_corregida))



# Pagina Shiny ejemplos ---------------------------------------------------

# if (!require('shiny')) install.packages("shiny")
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "117-shinythemes") #traer el ej. desde la pagina

# Scatter altura vs dap ---------------------------------------------------


ui <- fluidPage(
  title = 'Altura vs DAP',
  theme = shinytheme("cyborg"),
  h2('Segundo Inventario de Bosques Nativos de Argentina'),
  h3('Región Monte'), #subtitulo letra mas chica
  p('Altura',
  strong('vs'),
  'Dap'),
  hr(), #linea separadora 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'especie_corregida',
                    label = 'Seleccione la especie:',
                    choices = sp,
                    selected = 'Bulnesia retama'),
    ),
      mainPanel(
        plotOutput(outputId = 'plot_salida')
      )
    )
  )

server <- function(input, output) { 
  output$plot_salida <- renderPlot({
    df %>%
      filter(especie_corregida==input$especie_corregida) %>%
      filter(DAP_final_m!= 0) %>% 
      ggplot(aes(x=DAP_final_m, y=altura_total_est))+
      geom_point(show.legend=F, color = 'darkgreen', size = 2.5)+
      labs(x = 'DAP (m)',
      y = 'Altura (m)')+
      theme_bw()
  })
}


shinyApp(ui, server)



# prueba server scatter plot + linea tendendia sin IC y tamaño en función del AB ----------------------------------------------------------------

ui <- fluidPage(
  title = 'Altura vs DAP',
  theme = shinytheme("cyborg"),
  h2('Segundo Inventario de Bosques Nativos de Argentina'),
  h3('Región Monte'), #subtitulo letra mas chica
  p('Altura',
    strong('vs'),
    'Dap'),
  hr(), #linea separadora 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'especie_corregida',
                  label = 'Seleccione la especie:',
                  choices = sp,
                  selected = 'Bulnesia retama'),
    ),
    mainPanel(
      plotOutput(outputId = 'plot_salida')
    )
  )
)

server <- function(input, output) { 
  output$plot_salida <- renderPlot({
    df2 %>%
      filter(especie_corregida==input$especie_corregida) %>%
      filter(DAP_final_m!= 0) %>% 
      ggplot(aes(x=DAP_final_m, y=altura_total_est, size = seccion_ha_ABdap))+
      geom_point(show.legend=F, color = 'darkgreen')+
      labs(x = 'DAP (m)',
           y = 'Altura (m)')+
      geom_smooth(method = lm,color="black", se = FALSE)+
      theme_bw()
  })
}

shinyApp(ui, server)


# Gráfico barras volumen por clase diamétrica por especie -----------------

ui <- fluidPage(
  title = 'Volumen x clase diamétrica x especie',
  titlePanel("Volumen/ha por clase diamétrica"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'xxxxxx',
                  label = 'Seleccione la especie',
                  choices = xxx,
                  selected = ''),
    ),
    mainPanel(
      plotOutput(outputId = 'gráfico_barras')
    )
  )
)
server <- function(input, outpup){
  outpup$grafico_barras <- renderPlot({
    df %>% 
      filter(especie_corregida==input$especie_corregida) %>%
      filter(DAP_final_m!= 0) %>% 
      
  }
    
  )
}
