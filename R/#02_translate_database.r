#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.

library(package = "tidyverse")
library(package = "jsonlite")
library(package = "httr")
library(package = "googleLanguageR")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    print()

thistext <- org$Arguments[1]
startnum <- regexpr(pattern = "<p>", text = thistext)[1] + 3
stopnum <- regexpr(pattern = "</p>", text = thistext)[1] - 1
transtext <- substring(text = thistext, first = startnum, last = stopnum)
googleLanguageR::gl_translate(t_string = transtext, target = "jp")
googleLanguageR::gl_auth(json_file = )