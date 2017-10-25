output$epistle_usage_viz <- renderUI({
  
  switch(input$epistle_usage_selected_viz,
         "Century" = highchartOutput("epistle_usage_by_century_hc"),
         "Early Christian Author" = fluidPage(
           highchartOutput("epistle_usage_by_author_over100"),
           highchartOutput("epistle_usage_by_author_less100", height = "740px")
         ),
         "Region" = highchartOutput("epistle_usage_by_region"))
  
})



output$epistle_usage_by_region <- renderHighchart({
  
  letters_by_region <- main_table %>% 
    group_by(region, epistle) %>%
    summarise(number.of.references = n()) %>%
    mutate(total.per.region = sum(number.of.references)) %>%
    arrange(desc(total.per.region))
  
  letters_by_region %>%
    stacked_bar_chart(categories.column = ~region, 
                      subcategories.column = ~epistle, 
                      value.column = ~number.of.references,
                      stacking.type = "normal", subcategories.order = epistle_order) %>%
    hc_yAxis(title = list(text = "Number of references")) %>%
    hc_title(text = "References to Epistles broken down by location of referencing author",
             align = "left") %>%
    hc_plotOptions(series = list(minPointLength = 3))
})

output$epistle_usage_by_author_over100 <- renderHighchart({
  
  letters_by_author <- main_table %>% 
    group_by(early.christian.writer, epistle) %>%
    summarise(number.of.references = n()) %>%
    mutate(total.per.writer = sum(number.of.references)) %>%
    arrange(desc(total.per.writer))
  
  letters_by_author %>%
    filter(total.per.writer > 100) %>%
    stacked_bar_chart(categories.column = ~early.christian.writer, 
                      subcategories.column = ~epistle, 
                      value.column = ~number.of.references,
                      stacking.type = "normal", subcategories.order = epistle_order) %>%
    hc_yAxis(title = list(text = "Number of references")) %>%
    hc_title(text = "References to Epistles broken down by early Christian author",
             align = "left") %>%
    hc_subtitle(text = "(with more than 100 total references to texts)", align = "left")
  
})


output$epistle_usage_by_author_less100 <- renderHighchart({
  
  letters_by_author <- main_table %>% 
    group_by(early.christian.writer, epistle) %>%
    summarise(number.of.references = n()) %>%
    mutate(total.per.writer = sum(number.of.references)) %>%
    arrange(desc(total.per.writer))
  
  letters_by_author %>%
    filter(total.per.writer < 100) %>%
    stacked_bar_chart(categories.column = ~early.christian.writer, 
                      subcategories.column = ~epistle, 
                      value.column = ~number.of.references,
                      stacking.type = "normal", subcategories.order = epistle_order) %>%
    hc_yAxis(title = list(text = "Number of references")) %>%
    hc_title(text = "References to Epistles broken down by early Christian author",
             align = "left") %>%
    hc_subtitle(text = "(with fewer than 100 total references to texts)", align = "left")
  
})


output$epistle_usage_by_century_hc <- renderHighchart({
  
  letters_in_century <-
    main_table %>%
    filter(!is.na(reference.date.start)) %>%
    filter(!is.na(reference.date.end)) %>%
    mutate(reference.start.century = cut(
      reference.date.start,
      breaks = seq(0, 400, by = 100),
      right = FALSE
    )) %>%
    mutate(reference.end.century = cut(
      reference.date.end,
      breaks = seq(0, 600, by = 100),
      right = FALSE
    )) %>%
    mutate(
      reference.start.century = fct_recode(
        reference.start.century,
        "0-99" = "[0,100)",
        "100-199" = "[100,200)",
        "200-299" = "[200,300)",
        "300-399" = "[300,400)"
      )
    ) %>%
    mutate(
      reference.end.century = fct_recode(
        reference.end.century,
        "0-99" = "[0,100)",
        "100-199" = "[100,200)",
        "200-299" = "[200,300)",
        "300-399" = "[300,400)",
        "400-499" = "[400,500)",
        "500-599" = "[500,600)"
      )
    ) %>%
    group_by(reference.start.century, epistle) %>%
    summarise(number.of.references = n())
  
  letters_in_century %>%
    hchart(
      type = "bar",
      hcaes(x = reference.start.century,
            y = number.of.references,
            group = epistle)
    ) %>%
    hc_plotOptions(series = list(stacking = TRUE)) %>%
    hc_yAxis(title = list(text = "Number of References")) %>%
    hc_xAxis(title = list(text = "Century"))
  
})