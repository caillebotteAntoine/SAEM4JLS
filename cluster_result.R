
require(purrr)
require(dplyr)

min <- 116

readLines('saem.err') %>%
  discard(function(x) grepl('INFO : Large data should', x)) %>%
  discard(function(x) x == "" || x == " ") %>%
  {sapply(min:length(.),function(i) .[i])} %>%
  writeLines('cl.err')
