output$writer_viz_selected_author_UI <- renderUI({
  
  selectInput("selected.author",
              label = "Select an author:",
              choices = sort(unique(main_table$early.christian.writer)))
  
})

letters_of_author <- eventReactive(input$selected.author,
                                   {
                                     main_table %>%
                                       filter(early.christian.writer == input$selected.author) %>%
                                       group_by(reference.text, epistle) %>%
                                       summarise(number.of.references = n()) %>%
                                       mutate(total.per.writer = sum(number.of.references)) %>%
                                       arrange(desc(reference.text)) %>%
                                       group_by(reference.text) %>% {
                                         mutate(ungroup(.), y = group_indices(.) - 1)
                                       } %>%
                                       mutate(x = plyr::mapvalues(
                                         epistle,
                                         from = epistle_order,
                                         to = 1:length(epistle_order) - 1
                                       )) %>%
                                       mutate(z = number.of.references) %>%
                                       mutate(text = reference.text)
                                     
                                   })


output$writer_viz_ggplot2 <- renderPlot({
  
  letters_of_author <- letters_of_author()
  
  letters_of_author %>%
    ggplot(aes(x = epistle,
               y = reference.text))
  
})

output$writer_viz_ggplot2_UI <- renderUI({
  
  letters_of_author <- letters_of_author()
  
  print(as.numeric(2 * nrow(letters_of_author)))
  
  if(nrow(letters_of_author) < 30){
    print("less 30")
    plotOutput("writer_viz_ggplot2", height = paste0(3 * nrow(letters_of_author) + 200, "px"))
  } else {
    print("more 30")
    plotOutput("writer_viz_ggplot2", height = "2000px")
  }
  
  
  
  
})

output$writer_viz_hc_UI <- renderUI({
  
  letters_of_author <- letters_of_author()
  
  print(as.numeric(2 * nrow(letters_of_author)))
  
  if(nrow(letters_of_author) < 30){
    print("less 30")
    highchartOutput("writer_viz_texts_hc", height = paste0(3 * nrow(letters_of_author) + 300, "px"))
  } else {
    print("more 30")
    highchartOutput("writer_viz_texts_hc", height = "1700px")
  }
  
  
  
  
})

output$writer_viz_texts_hc <- renderHighchart({
  
  if(is.null(input$selected.author)){
    return()
  }
  
  letters_of_author <- letters_of_author()
  
  
  highchart() %>%
    hc_xAxis(categories = epistle_order,
             opposite = TRUE,
             min = 0,
             max = length(epistle_order) - 1) %>%
    hc_yAxis(
      categories = as.list(rev(unique(letters_of_author$reference.text))),
      # categories = "foooo",
      min = 0,
      max = length(unique(letters_of_author$reference.text)) - 1,
      reversed = TRUE
    ) %>%
    hc_add_series(data = letters_of_author,
                  type = "bubble",
                  showInLegend = FALSE) %>%
    hc_tooltip(
      formatter = JS(
        "function (){
        return '<b>Reference Text: </b>' + this.point.text +
        '<br/>' +
        '<b>Epistle: </b>' + this.point.epistle +
        '<br/>' +
        '<b>Number of references: </b>' + this.point.z + '<br/>'
}"
    )
      ) %>%
    hc_chart(zoomType = "y")
  
})




