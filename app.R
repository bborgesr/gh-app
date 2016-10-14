ui <- fluidPage(
  selectInput("flt", "Filter by title:", 
              choices = unique(rstudio$Title), 
              selected = c("Software Engineer", "Founder", "CTO", 
                           "Master Instructor", "Chief Scientist"),
              multiple = TRUE),
  selectInput("yaxis", "Select the y-axis", 
              choices = c("Repos", "Stars", "Followers", "Following", "Contributions"),
              selected = "Contributions"),
  actionButton("sort", "Sort"),
  plotOutput("plt", hover = hoverOpts(id = "plt_hover"))
)

server <- function(input, output, session) {
  
  oldSort <- new.env()
  oldSort$n <- 0
  
  rv <- reactiveValues(
    data = data,
    selected = NULL
  )
  
  observe({
    rv$data <- rv$data %>% filter(Title %in% input$flt)
    rv$data$yaxis <- switch(input$yaxis,
                            Repos = rv$data$Repos,
                            Stars = rv$data$Stars,
                            Followers = rv$data$Followers,
                            Following = rv$data$Following,
                            Contributions = rv$data$Contributions)
    if (input$sort != oldSort$n) {
      username <- rv$data$GitHubUsername[rv$selected]
      print(username)
      rv$data <- rv$data %>% arrange(yaxis)
      rv$selected <- which(rv$data$GitHubUsername == username)
      rv$data$GitHubUsername <- factor(rv$data$GitHubUsername, 
                                       levels = rv$data$GitHubUsername)
      oldSort$n <- input$sort
      # rv$selected <- NULL
    }
    if (!is.null(input$plt_hover$x)) {
      #rv$data$Color <- rep("#9d2", nrow(rv$data))
      rv$selected <- round(input$plt_hover$x)
      #rv$data[selected, "Color"] <- "#ddd"
    # } else {
      #rv$selected <- NULL
    }
  })
  
  output$plt <- renderPlot({
    data <- rv$data
    data[rv$selected, "Color"] <- "#ddd"
    ggplot(data, aes(x = GitHubUsername, y = yaxis, fill = Color)) + 
      geom_bar(stat="identity")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
