library(lubridate)

if (interactive()){
  ui <- fluidPage(
    # checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
    # selectizeInput('in2', 'Select a state', choices = state.name),
    # plotOutput('plot')
  
    actionButton("addRecord", "Add Record"),
    
    dateInput("datepicker", "Date", datesdisabled = c("2021-03-02", "2021-03-01"))
  )
  
  server <- function(input, output, session){
    ENTITIES <- data.frame(
      Location = c(
        "Mustang",
        "Abu Dhabi",
        "UK",
        #"Israel",
        #"NNorge AS",
        "Norge TCS",
        "Galway",
        "Egypt",
        "Mustang STS",
        "Houston",
        "Brazil",
        "Australia",
        "Sdn Bhd",
        "Indonesia",
        "PTAC",
        "BSG Americas",
        "Fired Heaters US",
        "E&IS US",
        "Milan",
        "FW Eng AG",
        "AMEC Australia"
      ),
      Entity = c(
        "Mustang Engineering Ltd - STS",
        "Wood Group Kenny Abu Dhabi - STS - new",
        "Wood Group UK Ltd - STS",
        #"Wood Group UK Ltd - Israel Branch",
        #"Wood Group Kenny Norge AS",
        "WG Norge TCS",
        "Wood Group Kenny Ireland Limited",
        "PSN (UK) Ltd - Egypt Branch",
        "WG Mustang - STS",
        "Wood Group Kenny Inc",
        "Wood Group Kenny do Brazil",
        "Wood Group Kenny Australia Pty Ltd",
        "Wood Group Kenny Sdn Bhd",
        "PT Wood Group Indonesia",
        "WG UK Ltd - PTAC",
        "BSG - Americas",
        "Fired Heaters US",
        "Wood E and I Solutions Inc - US",
        "Amec FW Italiana Pharma",
        "FW Engineering AG",
        "AMEC Bio Process and CE Australia"
      )
    )
    
    addWarning <- reactiveValues(message = NULL)
    
    observeEvent(input$addRecord, {
      showModal(addRecordModal())
    })
    
    addRecordModal <- function(failed = FALSE) {
      # Create modal UI.
      modalDialog(
        div(
          tags$i(class = "fas fa-plus fa-fw"),
          "Add Billability Data",
          class = "title-modal"
        ),
        div(
          selectInput(
            inputId = "entityName",
            label = "Office",
            selected = NULL,
            choices = c("Select" = "", ENTITIES$Entity),
            width = 500
          ),
          style = "margin-left: 15px;"
        ),
        div(
          dateInput(
            inputId = "weekEnding",
            label = "Week Ending",
            value = Sys.Date() - wday(Sys.Date() + 1), #the previous friday
            daysofweekdisabled = c(0:4, 6)
          ),
          style = "margin-left: 15px;"  
        ),
        br(),
        div(
          numericInput(
            inputId = "entityDirectBillableHrs",
            label = "Direct",
            value = NULL
          ),
          style = "margin-left: 15px;"  
        ),
        
        if (failed)
          # Show the error message
          div(addWarning$message, style = "color: red; margin-left:15px;"),
        footer = tagList(
          actionButton("cancelRecord", "Cancel"),
          actionButton("saveRecord", "Add Record")
        ), size = 'm'
      )
    }
    
    verifyInputModal <- function(message){
      addWarning$message <- HTML(paste(message))
      modalDialog(
        div(
          tags$i(class = "fas fa-plus fa-fw"),
          "Add Billability Data",
          class = "title-modal"
        ),
        div(addWarning$message, style = "color: red; margin-left:15px;"),
        footer = tagList(
          actionButton("verifyYes", "Yes, Save Input"),
          actionButton("verifyNo", "No, Amend Date")
        ), size = 'm'
      )
    }
    
    observeEvent(input$saveRecord, {
      addWarning$message <- NULL
      if (input$entityName == "") {
        addWarning$message <- HTML(paste("You need to add an entity name..."))  
      }
      # if (input$weekEnding == Sys.Date()){
      #   addWarning$message <- HTML(paste("Week ending date is today, are you sure this is ok?"))
      # }
      
      if (is.null(addWarning$message)){
        #print(data)
        if (input$weekEnding == Sys.Date()){ 
          showModal(verifyInputModal("Week ending date is today, are you sure this is ok?"))
        }
          removeModal()
      } else {
        showModal(addRecordModal(failed = TRUE))
      }
    })
    
    # Close & cancel the record.
    observeEvent(input$cancelRecord, {
      updateSelectInput(session, inputId = "entityName", selected = NULL)
      removeModal()
    })
    
    observeEvent(input$verifyYes, {
      return(TRUE)
    })
    
    observeEvent(input$verifyNo, {
      return(FALSE)
    })
    
    # Only show billability & utilization inputs when an office is selected
    # output$showFinanceInputs <- renderUI({
    #   if(is.null(input$entityName) | input$entityName == ""){
    #     return(NULL)
    #   } else {
    #     data <- getEntityData(entityData$data, "Grab")
    #     req(input$entityName)
    #     req(input$weekEnding)
    #     data <- data %>% 
    #       filter(Entity == input$entityName) %>%
    #       select(Entity, Week_Ending, Direct_Billable_Hrs, Indirect_Billable_Hrs, SGA_Billable_Hrs, 
    #              Direct_Available_Hrs, Indirect_Available_Hrs, SGA_Available_Hrs, Comment) %>%
    #       arrange(Week_Ending) %>%
    #       filter(Week_Ending == input$weekEnding)
    #     
    #     return(
    #       div(
    #         div("Billable Hours", class = "sub-title-modal"),
    #         div(
    #           numericInput(
    #             inputId = "entityDirectBillableHrs",
    #             label = "Direct",
    #             value = data$Direct_Billable_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div(
    #           numericInput(
    #             inputId = "entityIndirectBillableHrs",
    #             label = "Indirect",
    #             value = data$Indirect_Billable_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div(
    #           numericInput(
    #             inputId = "entitySGABillableHrs",
    #             label = "SG&A",
    #             value = data$SGA_Billable_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div("Available Hours", class = "sub-title-modal"),
    #         div(
    #           numericInput(
    #             inputId = "entityDirectAvailableHrs",
    #             label = "Direct",
    #             value = data$Direct_Available_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div(
    #           numericInput(
    #             inputId = "entityIndirectAvailableHrs",
    #             label = "Indirect",
    #             value = data$Indirect_Available_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div(
    #           numericInput(
    #             inputId = "entitySgaAvailableHrs",
    #             label = "SG&A",
    #             value = data$SGA_Available_Hrs[1]
    #           ),
    #           style = "margin-left: 15px;"  
    #         ),
    #         div("Comments", class = "sub-title-modal"),
    #         div(
    #           textAreaInput(
    #             inputId = "comment",
    #             label = NULL,
    #             value = data$Comment[1],
    #             height = '100px'
    #           ),
    #           style = "margin-left: 15px;"  
    #         )
    #       )
    #     )
    #   }
    # })  
  }
  
  shinyApp(ui, server)
}