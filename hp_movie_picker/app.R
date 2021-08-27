library(shiny)
library(ggplot2)
library(tibble)

ui <- fluidPage(
  titlePanel(fluidRow(column(12, "Harry Potter Movie Picker"))),
  fluidRow(column(12, "Made @ SLU, for SLU people, by TommyZ")),
  br(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(12, actionButton("button", "Pick Movie"))),
      br(),
      fluidRow(column(12, "Movie Picked:")),
      hr(),
      fluidRow(column(12, textOutput("text")))
    ),
    mainPanel(
      fluidRow(column(12, plotOutput("plot")))
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues()
  
  output$text <- renderText({
    values$val
  })
  
  output$plot <- renderPlot({
    if (!is.null(values$data)) {
      df <- as.data.frame(values$data)
      names(df) <- "data"
      df %>%
        as_tibble(., colnames="data") %>%
        ggplot(., aes(x=data, fill=data)) +
        geom_bar() +
        theme_classic() +
        scale_fill_viridis_d() +
        labs(title="Barplot of Movies Picked (Historical)", x="Movie", y="Count") +
        theme(
          axis.text.x = element_text(angle = -45, vjust = 1, hjust=0),
          legend.position = "bottom",
          legend.direction = "vertical"
          )
    }
  })
  
  observeEvent(input$button, {
    movies <- c(
      "1. Harry Potter and the Philosopher's Stone (2001)",
      "2. Harry Potter and the Chamber of Secrets (2002)",
      "3. Harry Potter and the Prisoner of Azkaban (2004)",
      "4. Harry Potter and the Goblet of Fire (2005)",
      "5. Harry Potter and the Order of the Phoenix (2007)",
      "6. Harry Potter and the Half-Blood Prince (2009)",
      "7. Harry Potter and the Deathly Hallows – Part 1 (2010)",
      "8. Harry Potter and the Deathly Hallows – Part 2 (2011)"
    )
    values$val <- sample(movies, 1)
    values$data <- c(values$data, values$val)
  })
}

shinyApp(ui, server)
