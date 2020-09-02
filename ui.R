### ---------------------------------------------------------------------------
### --- WD_gameReferenceHunt Project
### --- Script: ui.R, v. 0.0.1
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
library(shiny)
library(shinycssloaders)

### --- shinyUI
shinyUI(
  
  fluidPage(
  
    fluidRow(
      column(width = 7,
             br(),
             img(src = 'Wikidata-logo-en.png',
                 align = "left"),
             br(), br(), br(), br(), br(), br(), br(),
             includeHTML('html/dashboard_header.html')
      )
    ),
    fluidRow(
      column(width = 6,
        hr(),
        HTML('<h4>Item x Property x Value Dataset</h4>
             <p style="font-size:80%;"align="left">Columns:
             <b>Item: </b>Wikidata item, <b>Property: </b>Wikidata property of the Item, 
             <b>Extracted Data (JSON): </b>Extracted value for the respective property and item, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total, 
             <b>is_accepted: </b>is the proposed change acceptable given the criterion (described in the 
             dashboard\'s header).</p>'),
        DT::dataTableOutput('full_ratio', width = "100%"),
        downloadButton('full_ratio_download',
                       'Download (csv)'),
        br(), br(),
        HTML('<p style="font-size:80%;"align="left"><b>Note.</b> Please specify single quote as a string delimiter when opening this .csv file.</p>'),
        hr(),
        HTML('<h4>Per Property Statistics</h4>
             <p style="font-size:80%;"align="left">The following table presents the data aggregated across 
             the Wikidata properties. Columns: 
             <b>Property: </b>Wikidata property, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total. 
             </p>'),
        DT::dataTableOutput('property_ratio', width = "100%"),
        downloadButton('property_ratio_download',
                       'Download (csv)'),
        hr(),
        HTML('<h4>Per Datatype Statistics</h4>
             <p style="font-size:80%;"align="left">The following table presents the data aggregated across 
             the Wikidata datatypes. Columns: 
             <b>datatype: </b>Wikidata datatype, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total. 
             </p>'),
        DT::dataTableOutput('datatype_ratio', width = "100%"),
        downloadButton('datatype_ratio_download',
                       'Download (csv)'),
        hr(),
        HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
        hr(),
        includeHTML('html/technical_notes.html'),
        br(),
        hr()
      )
    )
  )
)









