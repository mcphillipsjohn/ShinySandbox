library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

header <- dashboardHeader(title = "demo")
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
              menuItem("admin", tabName = "admin", icon = icon("adjust")),
              downloadButton("downloadResults","Download Results")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'admin', class = 'active', 
      fluidRow(
        box(
          dataTableOutput('userTable'), width = 6
        )
      )
    )
  )
)


ui <- dashboardPage(title = 'admin function test', header, sidebar, body, skin='blue')

server <- function(input, output, session){
  
  dat <- data.frame(userName = c("John","Mary","Mike"), start = c("06/08/2019","01/01/2019","23/10/2019"), stringsAsFactors = FALSE)
  
  output$userTable <- renderDataTable({
    DT::datatable(isolate(dat),
                  editable = TRUE,
                  rownames = FALSE)
  })
  
  ###Tracking Changes###
  rvs <- reactiveValues(
    data = NA #dynamic data object
  )
  
  observe({
    rvs$data <- dat
  })
  
  proxy = dataTableProxy('userTable')
  observe({
    DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
  })
  
  observeEvent(input$userTable_cell_edit, {
    rvs$data <<- editData(rvs$data, input$userTable_cell_edit, rownames = FALSE)
  })
  
  # observeEvent(
  #   input$do,{
  #     write.csv(rvs$data,'userTest.csv', row.names = FALSE)
  #   })
  
  output$downloadResults <- downloadHandler(
    filename = function(){paste("userTest.csv.csv", sep = "")},
    content = function(file){write.csv(rvs$data, file, row.names = FALSE)}
  )
  
}

shinyApp(ui = ui, server = server)