ui <- fluidPage(
  h1(""),
  tags$iframe(src = "PROPIR2025", width = "100%", height = "500px")
)

server <- function(input, output) {

}

shinyApp(ui, server)