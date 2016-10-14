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
    data = data
  )
  
  observe({
    data <- data %>% filter(Title %in% input$flt)
    data$yaxis <- 
      switch(input$yaxis,
             Repos = data$Repos,
             Stars = data$Stars,
             Followers = data$Followers,
             Following = data$Following,
             Contributions = data$Contributions)
    rv$data <- data
  })
  
  observe({
    data <- rv$data
    if (input$sort != oldSort$n) {
      data <- data %>% arrange(yaxis)
      data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
      oldSort$n <- input$sort
    }
      if (!is.null(input$plt_hover$x)) {
        selected <- round(input$plt_hover$x)
        data[selected, "Color"] <- "#ddd"
      }
      rv$data <- data
  })
  
  output$plt <- renderPlot({
    ggplot(rv$data, aes(x = GitHubUsername, y = yaxis, fill = Color)) + 
      geom_bar(stat="identity")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
