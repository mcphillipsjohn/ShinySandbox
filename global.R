library(shiny)
library(shinydashboard)
library(shinycssloaders) # for loading spinners
library(shinyWidgets) # for toggle switch
library(shinyalert)
library(shinyjs)
# Establish DB connection ----
library(pool)
library(DBI)
library(odbc)
library(rjson)

"%+%" <- function(...) paste0(...)
CONFIG_FILE_NAME <- "deployConfigs.json"

credentials_config <- fromJSON(file = CONFIG_FILE_NAME)

# return the logged-in user (local or shinyapps.io)
getLoggedInUser <- function(session) {
  if (runningOnShinyApps()) {
    return(session$user) # shiny apps email address
  } else {
    if (Sys.getenv()["USERNAME"] == "jpayn") {
      return("jason.payne@woodplc.com")
    } else {
      return(paste0(tolower(Sys.getenv()["USERNAME"]), "@woodplc.com")) # local windows username @ machine name
    }
  }
}

runningOnShinyApps <- function() {
  if (Sys.getenv('SHELL') != "") { # running on linux
    return(TRUE)
  } else if (Sys.getenv('SHINY_PORT') == "") { 
    return(FALSE)  # this app IS NOT running on shinyapps.io
  } else {
    return(TRUE)  # this IS running on shinyapps.io
  }
}

# Create a custom value / info box
createInfoBox <- function(value, iconName, text) {
  return(
    div(div(div(value, class = "label-big"), span(iconName, class = "label-icon"), class = "label-box"), div(text, class = "label-box-explainer"))
  )
}

# Database Connection Functions
# =============================

formConnectionString <- function(dbName = NA) {
  PORT <- 1433
  SERVER <- "xenon-sql-prod.database.windows.net"
  USERNAME <- "wood-admin"
  
  # connect to the database, use a different driver if the application is on shinyapps.io
  if (runningOnShinyApps()) {
    databaseName = "FinanceSalesHR"
    DRIVER <- "FreeTDS;TDS_Version=7.2"
    SERVER_STR <- SERVER %+% ";Port=" %+% as.character(PORT)
    PASSWORD <- credentials_config$databaseKey # use this as db password in connection strings
    
  } else {
    # local
    databaseName = "FinanceSalesHR"
    DRIVER <- "{SQL Server}" #{ODBC Driver 13 for SQL Server} <- azure suggests this longer one, prod
    SERVER_STR <- "tcp:" %+% SERVER %+% "," %+% as.character(PORT)
    vault <- key_vault("https://xenon-keyvault-prod-001.vault.azure.net/") # replace with name of your vault name
    secret <- vault$secrets$get("sqlpwd") # replace with your secret name
    PASSWORD <- as.character(secret$value) # use this as db password in connection strings
  }
  
  # if arg for database name passed in, use that instead
  if (!is.na(dbName)) {
    databaseName <- dbName
  }
  
  connectionString <- "Driver="   %+% DRIVER     %+%  ";" %+%
    "Server="   %+% SERVER_STR %+%  ";" %+%
    "Database=" %+% databaseName   %+%  ";" %+%
    "Uid="      %+% USERNAME   %+%  ";" %+%
    "Pwd={"     %+% PASSWORD   %+% "};" %+% # TODO move password to be stored in non-source-controlled file
    "Encrypt=yes;" %+%
    "TrustServerCertificate=no;" %+%
    "Connection Timeout=30;"
  
  return(connectionString)
}

formConnection <- function(database, server, username, password) {
  return(DBI::dbConnect(odbc::odbc(), .connection_string = formConnectionString()))
}

# get database pool
getDbPool <- function(dbName = NA) {
  if (is.na(dbName)) {
    stop("you must specify a database name")
  }
  return(pool::dbPool(odbc::odbc(), .connection_string = formConnectionString()))
}

saveByChunk <- function(df, table_name, chunkSize = 1000, connection_pool){
  con <- pool::poolCheckout(connection_pool)
  j = 1
  withProgress(message = 'Saving data chunks...', value = 0, {
    # Split the data frame into chunks of chunk size or less
    print(paste0("Writing ", nrow(df), " rows to database."))
    chunkList <- split(df, (seq(nrow(df))-1) %/% chunkSize)
    # Now write each data chunk to the database 
    for (i in 1:length(chunkList)){
      # Show a progress loading bar in bottom right for each shape added...
      incProgress(1 / length(chunkList), detail = paste(i, " of ", length(chunkList)))
      print(paste0("Processing Batch ", i, " of ", length(chunkList)))
      DBI::dbWriteTable(con, SQL(table_name), value = chunkList[[i]], append = TRUE, row.names = FALSE)
    }
  })
  poolReturn(con)
}