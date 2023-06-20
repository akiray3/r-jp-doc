#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.

library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    print()

thistext <- org$Arguments[2]

arg_eng_list <- list(NULL)
length(arg_eng_list) <- nrow(org)

for (i in 1:nrow(org)) {
  if(is.na(org$Arguments[i])) {
    next
  }
  arg_eng_list[[i]] <- org$Arguments[i] %>%
    rvest::read_html() %>%
    rvest::html_nodes("p") %>%
    as.character() %>%
    stringr::str_remove_all(pattern = "<p>|</p>")
}

out1 <- unlist(arg_eng_list)
length(out1)

write.table(x = out1[1:8000], file = "R/arg_eng_list.txt", sep = ",", quote = F)





startnum <- gregexpr(pattern = "<p>", text = thistext)[[1]] + 3
stopnum <- gregexpr(pattern = "</p>", text = thistext)[[1]] - 1
transtext <- substring(text = thistext, first = startnum, last = stopnum)
