
#' Shiny App for Interactive RDF Graph Visualization
#' @description
#' A Shiny app to visualize RDF triples using plot_rdf_triples_generic, with sliders to adjust visNetwork physics parameters interactively.
#'
#' @examples
#' library(shiny)
#' library(visNetwork)
#' library(dplyr)
#' source("plot_rdf_triples_generic_dplyr_physics_args.R") # Source the function
#' triples <- tibble::tibble(
#'   subject = c("id1", "id1", "id1", "id1", "id1", "id2"),
#'   predicate = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Sepal.Length"),
#'   object = c("5.1", "3.5", "1.4", "0.2", "setosa", "4.9"),
#'   type = c("double", "double", "double", "double", "tag", "double")
#' )
#' runApp(shinyApp(ui, server))
#'
library(shiny)
library(visNetwork)
library(dplyr)

ui <- fluidPage(
  titlePanel("Interactive RDF Graph Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_frame", "Select RDF Triples Data Frame:",
                  choices = NULL),
      h4("Physics Parameters"),
      sliderInput("gravitationalConstant", "Gravitational Constant (-5000 to 0):",
                  min = -5000, max = 0, value = -2000, step = 100),
      sliderInput("centralGravity", "Central Gravity (0 to 1):",
                  min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("springLength", "Spring Length (10 to 500):",
                  min = 10, max = 500, value = 95, step = 5),
      sliderInput("springConstant", "Spring Constant (0 to 0.5):",
                  min = 0, max = 0.5, value = 0.0, step = 0.01),
      sliderInput("damping", "Damping (0 to 1):",
                  min = 0, max = 1, value = 0.09, step = 0.01),
      sliderInput("avoidOverlap", "Avoid Overlap (0 to 1):",
                  min = 0, max = 1, value = 0.1, step = 0.01),
      checkboxInput("show_edge_labels", "Show Edge Labels", value = TRUE),
      checkboxInput("stabilize", "Stabilize Layout", value = TRUE),
      checkboxInput("shorten_ids", "Shorten IDs", value = TRUE)
    ),
    mainPanel(
      visNetworkOutput("graph", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Update data frame choices dynamically
  observe({
    # Get all objects in the global environment
    env_objects <- ls(envir = .GlobalEnv)
    # Filter for data frames
    df_choices <- env_objects[sapply(env_objects, function(x) {
      is.data.frame(get(x, envir = .GlobalEnv))
    })]
    # Update selectInput choices
    updateSelectInput(session, "data_frame", choices = df_choices)
  })

  # Reactive expression to get the selected data frame
  selected_triples <- reactive({
    req(input$data_frame)
    get(input$data_frame, envir = .GlobalEnv)
  })

  # Render graph (initial and when inputs change)
  output$graph <- renderVisNetwork({
    req(selected_triples())
    plot_rdf_triples_generic(
      selected_triples(),
      show_edge_labels = input$show_edge_labels,
      stabilize = input$stabilize,
      shorten_ids = input$shorten_ids,
      gravitationalConstant = input$gravitationalConstant,
      centralGravity = input$centralGravity,
      springLength = input$springLength,
      springConstant = input$springConstant,
      damping = input$damping,
      avoidOverlap = input$avoidOverlap
    )
  })

  # Update graph when show_edge_labels changes
  observeEvent(input$show_edge_labels, {
    output$graph <- renderVisNetwork({
      req(selected_triples())
      plot_rdf_triples_generic(
        selected_triples(),
        show_edge_labels = input$show_edge_labels,
        stabilize = input$stabilize,
        shorten_ids = input$shorten_ids,
        gravitationalConstant = input$gravitationalConstant,
        centralGravity = input$centralGravity,
        springLength = input$springLength,
        springConstant = input$springConstant,
        damping = input$damping,
        avoidOverlap = input$avoidOverlap
      )
    })
  })

  # Update physics parameters dynamically
  observe({
    visNetworkProxy("graph") %>%
      visPhysics(
        solver = "barnesHut",
        stabilization = input$stabilize,
        barnesHut = list(
          gravitationalConstant = input$gravitationalConstant,
          centralGravity = input$centralGravity,
          springLength = input$springLength,
          springConstant = input$springConstant,
          damping = input$damping,
          avoidOverlap = input$avoidOverlap
        )
      )
  })
}

# Run the app
# shinyApp(ui, server)

