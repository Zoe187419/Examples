library(rvest)

url<-"https://en.wikipedia.org/wiki/List_of_capitals_in_the_United_States"

output<-read_html(url) %>%
    html_node("#mw-content-text > div > table:nth-child(8)") %>%
    html_table(fill=TRUE)

tdist <- read_html("http://en.wikipedia.org/wiki/Student%27s_t-distribution")
output<-tdist %>%
    html_node(".wikitable") %>%
    html_table(header = FALSE)

#mw-content-text > div > table.wikitable
#mw-content-text > div > table:nth-child(8)