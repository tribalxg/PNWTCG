library(shiny)
library(visNetwork)

ui <- fluidPage(
  tagList(
    div(
      fileInput("nodes_file", "Load a saved nodes file"),
      fileInput("edges_file", "Load a saved edges file")
    ),
    div(
      downloadButton("save_file", "Save Network")
    ),
    div(
      visNetworkOutput("network")
    )
  )
)

server <- function(input, output) {

  nodes_blank = data.frame(
    id = character(),
    label = character()
  )
  edges_blank = data.frame(
    id = character(),
    from = character(),
    to = character()
  )
  
  net <- reactiveValues(
    nodes_base = nodes_blank,
    edges_base = edges_blank,
    nodes = nodes_blank,
    edges = edges_blank
  )

  output$network <- renderVisNetwork({
    visNetwork(net$nodes_base, net$edges_base) |> 
      visOptions(manipulation = list(enabled = TRUE))
  })

  # if a node is changed in the network, update dataset
  observeEvent(input$network_graphChange, {

    if (input$network_graphChange$cmd == 'deleteElements') {
      net$nodes <- net$nodes[!net$nodes$id %in% unlist(input$network_graphChange$nodes),]
      net$edges <- net$edges[!net$edges$id %in% unlist(input$network_graphChange$edges),]
      return()
    }

    sharednames <- colnames(net$nodes)[colnames(net$nodes) %in% names(input$network_graphChange)]
    if (input$network_graphChange$cmd == 'addNode') {
      net$nodes[nrow(net$nodes)+1,sharednames] <- unlist(input$network_graphChange[sharednames])
    }
    if (input$network_graphChange$cmd == 'addEdge') {
      net$edges[nrow(net$edges)+1,sharednames] <- unlist(input$network_graphChange[sharednames])
    }
    if (input$network_graphChange$cmd == 'editNode') {
      net$nodes[net$nodes$id == input$network_graphChange$id, sharednames] <- unlist(input$network_graphChange[sharednames])
    }
    if (input$network_graphChange$cmd == 'editEdge') {
      net$edges[net$edges$id == input$network_graphChange$id, sharednames] <- unlist(input$network_graphChange[sharednames])
    }

    })

  observe({
    req(input$nodes_file, input$edges_file)
    net$nodes_base <- read.table(input$nodes_file$datapath, header=TRUE, sep='\t')
    net$edges_base <- read.table(input$edges_file$datapath, header=TRUE, sep='\t')
    net$nodes <- net$nodes_base
    net$edges <- net$edges_base
  })

  output$save_file <- downloadHandler(
    filename = function() {
      paste0("network_", Sys.Date(), ".zip")
    },
    content = function(file) {

      tmpdir <- tempdir()
      write.table(net$nodes, file.path(tmpdir, 'nodes.tsv'), quote = FALSE, sep = '\t', row.names = FALSE)
      write.table(net$edges, file.path(tmpdir, 'edges.tsv'), quote = FALSE, sep = '\t', row.names = FALSE)

      zip::zip(
        zipfile = file,
        files   = list.files(tmpdir, full.names = TRUE),
        mode    = "cherry-pick"
      )
    }
  )

}

shinyApp(ui = ui, server = server)
