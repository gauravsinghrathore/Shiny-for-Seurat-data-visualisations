library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "MiSTR vis dashboard"),
                    
                    dashboardSidebar(
                      
                      selectInput("timepoint","Select MiSTR timepoints", choices = c("day0 ES","day1 dorsal","day1 ventral","day2 dorsal","day2 ventral","day5 dorsal","day5 ventral","day9 dorsal","day9 ventral","day14 dorsal","day14 ventral","day21 dorsal","day21 ventral","day35 dorsal","day35 ventral")),
                      textInput('gene', label = "feature labels", value = "", placeholder = "add gene names ..."),
                      actionButton("GO",label = "GO"),
                      downloadButton("dl", "Download marker table")
                      
                      #radioButtons("plot_type",label = "select plot type", choices = c( "dotplot","lineplot","histogram"))
                    ),
                    dashboardBody(
                      tabsetPanel(type = "tabs",
                                  tabPanel("basic",
                                           fluidRow(
                                             box(title= "Feature Plot",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput(outputId="featureplot")),  
                                             box(title = "Cluster Plot", status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput(outputId="clusterplot")),
                                             box(title = "Violin Plot",status = "primary", solidHeader = TRUE,collapsible = TRUE, plotOutput(outputId="violinplot")), 
                                             #box(plotOutput(outputId="dotplot")),
                                             box(title = "marker table",status = "primary", solidHeader = TRUE,collapsible = TRUE, div(style = 'overflow-x: scroll',DT::dataTableOutput("marker_table")),height = "200%"),
                                             
                                             column(10,title= "top 5 marker dotplot",plotOutput(outputId="dotplot", width="1400px",height="300px"))
                                           ),
                                           
                                           #plotOutput("featureplot"),plotOutput("clusterplot"),
                                  ),
                                  tabPanel("advanced", DT::dataTableOutput("mytable"))
                      )
                    )
)
