
require(purrr)
require(dplyr)

readLines('cluster.err') %>%
  discard(function(x) grepl('INFO : Large data should', x)) %>%
  discard(function(x) x == "" || x == " ") %>%
  writeLines('cl.err')

