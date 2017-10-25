library("tidyverse")
library("shiny")
library("DT")
library("forcats")
library("rlang")
library("highcharter")
# library("oidnChaRts")

source("data-processing.R", local = TRUE)

url_generator <- function(book_name = NA,
                          begin_chapter = NA,
                          begin_v = NA,
                          end_chapter = NA,
                          end_v = NA,
                          link_text = NA) {
  paste0(
    "<a href=",
    "https://www.biblegateway.com/passage/?search=",
    gsub(" ", "+", book_name),
    "+",
    begin_chapter,
    "%3A",
    begin_v,
    "+-+",
    end_chapter,
    "%3A",
    end_v,
    "&version=NRSV",
    ">",
    link_text,
    '<span class="glyphicon glyphicon-new-window" aria-hidden="true"></span>',
    "</a>"
  )
}


shinyServer(function(input, output, session) {
  output$region_filter_UI <- renderUI({
    all_regions <- patristics_data$Region %>%
      unique()
    
    selectInput(
      "region_filter",
      "Include only these regions in the table:",
      choices = all_regions,
      selected = all_regions,
      width = "100%",
      multiple = TRUE
    )
    
  })
  
  source("tab_epistle-usage.R", local = TRUE)$value
  
  source("tab_early-christian-writer.R", local = TRUE)$value
  
  output$epistle_popularity <- renderPlot(
    main_table %>%
      group_by(epistle) %>%
      summarise(reference = n()) %>%
      arrange(desc(reference)) %>%
      ggplot(aes(x = epistle, y = reference)) + geom_col() + coord_flip()
  )
  
  output$patristics_table <- DT::renderDataTable({
    main_table %>%
      filter(region %in% input$region_filter) %>%
      mutate(epistle = url_generator(
        book_name = epistle,
        begin_chapter = epistle.start.chapter,
        begin_v = epistle.start.verse,
        end_chapter = epistle.end.chapter,
        end_v = epistle.end.chapter,
        link_text = paste(
          epistle
        )
      )) %>%
      datatable(
        rownames = FALSE,
        
        colnames = headers_main_table$column.headings,
        extensions = c('Scroller'),
        selection = 'none',
        escape = FALSE,
        options = list(
          pageLength = 10,
          columnDefs = list(list(
            width = "80px", targets = list(0, 1, 5)
          )),
          # columnDefs = list(list(width = "80px", targets = "_all")),
          server = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          # scrollY = 500,
          # scroller = TRUE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          rowCallback = htmlwidgets::JS(
            "
            function(row, data, rowi) {
            data.forEach(function(d,i) {
            if(typeof(d) === 'boolean') {
            $($('td', row)[i]).html(
            [
            '<span class=\\'',
            d ? 'glyphicon glyphicon-ok' : 'glyphicon glyphicon-remove',
            '\\'>',
            '</span'
            ].join('')
            )
            }
            })
            }
            
            "
            )
            )
            )
    
            })
  
})