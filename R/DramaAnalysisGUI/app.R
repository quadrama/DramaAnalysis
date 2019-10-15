library(shiny)

ui <- navbarPage(
  "DramaAnalysisGUI",
  tabPanel(
    "Data", 
    fluidPage(
      h2("Input Data"),
      p("Load Drama by ID:"),
      fluidRow(
        column(
          width = 4,
          textInput("id_input", NULL, "test:rksp.0")
          ),
        column(
          width = 4,
          actionButton("load_id", "Load by ID", 
                       style="color: #fff; background-color: #0099ff")
          )
        ),
      p("or load Drama from TEI-file:"),
      fluidRow(
        column(
          width = 4,
          fileInput("tei_input", NULL)
          ), 
        column(
          width = 4, 
          actionButton("load_tei", "Load by TEI-file", 
                       style="color: #fff; background-color: #0099ff")
          )
        ),
      uiOutput("display_drama")
      )
    ),
  tabPanel("Characters"),
  tabPanel("Utterances"),
  tabPanel("Configuration"),
  tabPanel("Word Fields")
) 


server <- function(input, output) {
  
  # renders data tables in data tab
  render_input_data <- function(d) {
    output$drama_text <- renderDataTable(d$text)
    output$drama_meta <- renderDataTable(d$meta)
    output$drama_segments <- renderDataTable(d$segments)
    output$drama_mentions <- renderDataTable(d$mentions)
    output$drama_characters <- renderDataTable(d$characters)
    output$drama_stageDirections <- renderDataTable(d$stageDirections)
  }
  
  # renders the tabsetPanel in data tab
  display_drama_tabs <- function() {
    renderUI({
      tabsetPanel(
        tabPanel("text", dataTableOutput("drama_text")),
        tabPanel("meta", dataTableOutput("drama_meta")),
        tabPanel("segments", dataTableOutput("drama_segments")),
        tabPanel("mentions", dataTableOutput("drama_mentions")),
        tabPanel("characters", dataTableOutput("drama_characters")),
        tabPanel("stage directions", dataTableOutput("drama_stageDirections"))
      )
    })
  }
  
  # listens to id-input button in data tab
  observeEvent(input$load_id, {
    drama <- loadDrama(input$id_input)
    render_input_data(drama)
    output$display_drama <- display_drama_tabs()
  })
  
  # listens to tei-input button in data tab
  observeEvent(input$load_tei, {
    drama <- loadDramaTEI(input$tei_input$datapath, dataDirectory="")
    render_input_data(drama)
    output$display_drama <- display_drama_tabs()
  })
  
}
  
shinyApp(ui = ui, server = server)