#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Autor: Davi Laerte

library(shiny)
library(shinydashboard)
library(tidyverse)

dados_series <- read.csv("https://raw.githubusercontent.com/nazareno/imdb-series/master/data/series_from_imdb.csv", sep = ",")
series_summarizadas <- dados_series %>% group_by(series_name) %>%
  summarise(media_rating = mean(UserRating), numero_votos_media = mean(UserVotes), numero_episodios = n()) %>%
  arrange(desc(numero_votos_media))
nomes_series <- as.character(series_summarizadas$series_name)

ui <- dashboardPage(
  dashboardHeader(title = "Analisando Séries"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Gráfico com as Séries Mais Populares escolhidas",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("plot1", height = 600)),
      box(
        title = "Analisando as Séries Mais Populares",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        HTML("<h4>Análise das 10 séries mais populares levando em conta o número médio de votos que cada uma teve segundo o
             <a href='http://www.imdb.com'>IMDB</a>.</h4></br></br><b> Colocação(<i>em termos de popularidade</i>) das séries mostradas no Gráfico:</b>"),
        htmlOutput("texto")),
      
      box(
        title = "Filtro das séries mais Populares",
        status = "warning",
        solidHeader = TRUE,
        sliderInput("slider", "Intervalo da Colocação:", min = 1, max = 10, value = c(0,10))
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    series_summarizadas %>%
      filter(series_name %in% nomes_series[input$slider[1]:input$slider[2]]) %>%
      arrange(desc(numero_votos_media))  %>%
      ggplot(aes(x = series_name, y = media_rating, fill = numero_episodios)) +
      labs(x = "Nomes das Séries") +
      labs(y = "Média de avaliação")+
      labs(fill = "Número de Episodios") +
      geom_col()
  })
  
  output$texto <- renderText({
    colocacoes <- c('<b>1</b>','<b>2</b>','<b>3</b>','<b>4</b>','<b>5</b>','<b>6</b>','<b>7</b>','<b>8</b>','<b>9</b>','<b>10</b>')
    paste(colocacoes[input$slider[1]:input$slider[2]], nomes_series[input$slider[1]:input$slider[2]], collapse = "</br>")
  })
}
shinyApp(ui, server)