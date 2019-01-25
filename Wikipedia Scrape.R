#install.packages("rvest")
library("rvest")
url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
population <- url %>%
  read_html() %>%
  html_nodes("table.sortable") %>%
  html_table()
population <- population[[1]]

head(population)