library("highcharter")

navbarPage(
  "Paul and Patristics",
  tabPanel(
    "Database",
    uiOutput("region_filter_UI"),
    DT::dataTableOutput("patristics_table") 
  ),
  tabPanel(
    "Epistle Usage Visualisations",
    fluidPage(
      
      selectInput("epistle_usage_selected_viz",
                  "Break down usage by:",
                  choices = c("Century",
                              "Early Christian Author",
                              "Region")),
      uiOutput("epistle_usage_viz")
    )
  ),
  tabPanel("Early Christian Writer Visualisations",
           uiOutput("writer_viz_selected_author_UI"),
           # highchartOutput("writer_viz_texts")
           uiOutput("writer_viz_hc_UI")
           # uiOutput("writer_viz_ggplot2_UI")
           ),
  collapsible = TRUE
)