
require(purrr)
require(dplyr)

d <- readLines('saem.err') %>%
  discard(function(x) grepl('INFO : Large data should', x)) %>%
  discard(function(x) x == "" || x == " ") %>%
  {sapply( last(which(grepl('processing file', d)) ):length(.),function(i) .[i])} %>%
  writeLines('cl.err')
