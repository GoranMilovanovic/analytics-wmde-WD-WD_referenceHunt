### ---------------------------------------------------------------------------
### --- WD_gameReferenceHunt Project
### --- Script: server.R, v. 0.0.1
### --- WMDE 2020.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### ---
### --- WD_gameReferenceHunt Project is free software: 
### --- you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WD_gameReferenceHunt Project is distributed in the 
### --- hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WD_gameReferenceHunt Project. 
### --- If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup
library(DT)
library(dplyr)
library(httr)
library(curl)
library(stringr)
library(jsonlite)

### --- Fetch files and update stamp from production:
### --- https://wd-ref-island.toolforge.org/stats.php

### --- Server

### --- shinyServer
shinyServer(function(input, output, session) {
  
  ### --- GET DATA
  
  # - get current data files + pre-process:
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    ### --- Download matches
    incProgress(1/5, detail = "Accepted matches.")
    acceptedFrame <- read.csv('https://wd-ref-island.toolforge.org/stats.php?dump=ACCEPTED', 
                              check.names = F, 
                              stringsAsFactors = F)
    incProgress(2/5, detail = "Rejected matches.")
    rejectedFrame <- read.csv('https://wd-ref-island.toolforge.org/stats.php?dump=REJECTED', 
                              check.names = F, 
                              stringsAsFactors = F)
    ### --- Process matches
    incProgress(3/5, detail = "Process matches.")
    # - determine datatype
    acceptedFrame$datatype <- sapply(acceptedFrame$`Value (JSON)`, function(x) {
      as.character(names(fromJSON(x))[1])
    })
    acceptedFrame$datatype <- as.character(acceptedFrame$datatype)
    acceptedFrame$datatype[acceptedFrame$datatype == "character(0)"] <- NA
    acceptedFrame <- acceptedFrame[complete.cases(acceptedFrame), ]
    rejectedFrame$datatype <- sapply(rejectedFrame$`Value (JSON)`, function(x) {
      as.character(names(fromJSON(x))[1])
    })
    rejectedFrame$datatype <- as.character(rejectedFrame$datatype)
    rejectedFrame$datatype[rejectedFrame$datatype == "character(0)"] <- NA
    rejectedFrame <- rejectedFrame[complete.cases(rejectedFrame), ]
    ### --- WDQS for External Identifiers
    incProgress(4/5, detail = "Contact WDQS for External Identifiers.")
    # endPoint <- 'https://query.wikidata.org/sparql?query='
    # query <- 'SELECT ?item ?itemLabel WHERE { 
    #               ?item wdt:P31/wdt:P279* wd:Q19847637. 
    #               SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    #           }'
    # exIds <- httr::GET(URLencode(paste0(endPoint, query, "&format=json")))
    ### --- Process WDQS data for External Identifiers
    # if (exIds$status_code == 200) {
    #   incProgress(5/5, detail = "PARSE WDQS JSON response.")
    #   exIds <- rawToChar(exIds$content)
    #   exIds <- fromJSON(exIds, simplifyDataFrame = T)
    #   exIds <- exIds$results$bindings
    #   exIds <- data.frame(item = exIds$item$value,
    #                       itemLabel = exIds$itemLabel$value, 
    #                       stringsAsFactors = F)
    #   exIds$item <- gsub("http://www.wikidata.org/entity/", "", exIds$item)
    # }
  })
  
  ### --- ANALYTICS
  
  # - per external ID
  
  # - per datatype
  acceptedRate_datatype <- acceptedFrame %>% 
    select(datatype) %>% 
    group_by(datatype) %>% 
    summarise(count = n())
  rejectedRate_datatype <- rejectedFrame %>% 
    select(datatype) %>% 
    group_by(datatype) %>% 
    summarise(count = n())
  datatype_ratio <- left_join(acceptedRate_datatype, 
                              rejectedRate_datatype, 
                              by = "datatype")
  colnames(datatype_ratio)[2:3] <- c("accepted", "rejected")
  datatype_ratio$accepted[is.na(datatype_ratio$accepted)] <- 0
  datatype_ratio$rejected[is.na(datatype_ratio$rejected)] <- 0
  datatype_ratio$ratio <- datatype_ratio$accepted/datatype_ratio$rejected
  datatype_ratio$percent_accepted <- datatype_ratio$accepted/(datatype_ratio$accepted + datatype_ratio$rejected)*100
  datatype_ratio$total_decisions <- datatype_ratio$accepted + datatype_ratio$rejected
  datatype_ratio$ratio <- round(datatype_ratio$ratio, 2)
  datatype_ratio$percent_accepted <- round(datatype_ratio$percent_accepted, 2)
  datatype_ratio <- arrange(datatype_ratio, desc(total_decisions))
  
  # - per property
  acceptedRate_property <- acceptedFrame %>% 
    select(Property) %>% 
    group_by(Property) %>% 
    summarise(count = n())
  rejectedRate_property <- rejectedFrame %>% 
    select(Property) %>% 
    group_by(Property) %>% 
    summarise(count = n())
  property_ratio <- left_join(acceptedRate_property, 
                              rejectedRate_property, 
                              by = "Property")
  colnames(property_ratio)[2:3] <- c("accepted", "rejected")
  property_ratio$accepted[is.na(property_ratio$accepted)] <- 0
  property_ratio$rejected[is.na(property_ratio$rejected)] <- 0
  property_ratio$ratio <- property_ratio$accepted/property_ratio$rejected
  property_ratio$percent_accepted <- 
    property_ratio$accepted/(property_ratio$accepted + property_ratio$rejected)*100
  property_ratio$total_decisions <- property_ratio$accepted + property_ratio$rejected
  property_ratio$ratio <- round(property_ratio$ratio, 2)
  property_ratio$percent_accepted <- round(property_ratio$percent_accepted, 2)
  property_ratio <- arrange(property_ratio, desc(total_decisions))
  
  # - per Property * Item * Extracted Data
  aFrame <- acceptedFrame %>% 
    select(Item, Property, `Extracted Data (JSON)`)
  aFrame <- aFrame %>% 
    group_by(Item, Property, `Extracted Data (JSON)`) %>% 
    summarise(accepted = n())
  rFrame <- rejectedFrame %>% 
    select(Item, Property, `Extracted Data (JSON)`)
  rFrame <- rFrame %>% 
    group_by(Item, Property, `Extracted Data (JSON)`) %>% 
    summarise(rejected = n())
  full_ratio <- full_join(aFrame, rFrame)
  full_ratio$accepted[is.na(full_ratio$accepted)] <- 0
  full_ratio$rejected[is.na(full_ratio$rejected)] <- 0
  full_ratio$ratio <- full_ratio$accepted/full_ratio$rejected
  full_ratio$percent_accepted <- 
    full_ratio$accepted/(full_ratio$accepted + full_ratio$rejected)*100
  full_ratio$total_decisions <- full_ratio$accepted + full_ratio$rejected
  full_ratio$is_accepted <- ifelse(
    (full_ratio$total_decisions >= 5 & full_ratio$percent_accepted >= 95),
    TRUE, FALSE)
  full_ratio$ratio <- round(full_ratio$ratio, 2)
  full_ratio$percent_accepted <- round(full_ratio$percent_accepted, 2)
  full_ratio <- arrange(full_ratio, desc(total_decisions))
  
  # - full_ratio_show: for DT
  full_ratio_show <- full_ratio
  full_ratio_show$`Extracted Data (JSON)` <- ifelse(nchar(full_ratio_show$`Extracted Data (JSON)`) >= 50, 
                                               paste0(substr(full_ratio_show$`Extracted Data (JSON)`, 1, 50), 
                                               " (...truncated...)"),
                                               full_ratio_show$`Extracted Data (JSON)`)
  
  ### --- TABLES
  
  ### --- output$datatype_ratio
  output$datatype_ratio <- DT::renderDataTable({
    DT::datatable(datatype_ratio, 
              options = list(
                pageLength = 100,
                width = '100%',
                columnDefs = list(list(className = 'dt-left', targets = "_all"))
              ),
              rownames = FALSE
    )
  })
  
  # - Download: datatype_ratio
  output$datatype_ratio_download <- downloadHandler(
    filename = function() {
      'datatype_ratio.csv'},
    content = function(file) {
      write.csv(datatype_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- output$full_ratio
  output$full_ratio <- DT::renderDataTable({
    DT::datatable(full_ratio_show, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: full_ratio
  output$full_ratio_download <- downloadHandler(
    filename = function() {
      'Item-Property_Value.csv'},
    content = function(file) {
      write.csv(full_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- output$property_ratio
  output$property_ratio <- DT::renderDataTable({
    DT::datatable(property_ratio, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: property_ratio
  output$property_ratio_download <- downloadHandler(
    filename = function() {
      'property_ratio.csv'},
    content = function(file) {
      write.csv(property_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}) # - END Shiny Server
  




