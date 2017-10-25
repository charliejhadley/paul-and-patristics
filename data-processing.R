secret_url <- read_csv("data/secret-url.csv")
patristics_data <- read_csv(secret_url[[1]])

patristics_data <- patristics_data %>%
  mutate(
    `Secondary quotation` = plyr::mapvalues(
      `Secondary quotation`,
      from = c(NA, "*"),
      to = c(FALSE, TRUE)
    ),
    `Secondary quotation` = as.logical(`Secondary quotation`)
  )

main_table <- patristics_data

headers_main_table <- tibble(column.headings = colnames(main_table),
                             column.names = tolower(make.names(colnames(main_table))))

colnames(main_table) <- tolower(make.names(colnames(main_table)))

epistle_order <- c("Romans", "1 Corinthians", "2 Corinthians", "Galatians", "Ephesians", "Philippians", "Colossians", "1 Thessalonians", "2 Thessalonians", "Philemon")

epistle_order_tib <- tibble(
  epistle.name = epistle_order,
  index = 1:length(epistle_order) - 1
)
