#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.
library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    print()
org_sub <- org_jpn <- org
nrow_org <- nrow(org_sub)

# Description
for (i in 1:nrow_org) {
  org_sub$Description[i] <- rvest::read_html(org$Description[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()
}
write.table(x = org_sub$Description, file = "R/dsc_eng.txt",
  sep = ",", row.names = FALSE, quote = FALSE)
org_jpn$Description <- readLines("R/dsc_jpn.txt", skipNul = TRUE)[-1] %>%
  dplyr::as_tibble() %>%
  dplyr::filter(value != "") %>%
  dplyr::mutate(value = gsub(pattern = " ", replacement = "", x = value, fixed=TRUE))

# Usage
for (i in 1:nrow_org) {
  if(is.na(org$Usage[i])) {
    org_sub$Usage[i] <- NA
    next
  }
  org_sub$Usage[i] <- rvest::read_html(org$Usage[i]) %>%
    rvest::html_nodes("pre") %>%
    rvest::html_text()
}
write.table(x = org_sub$Usage, file = "R/usg_eng.txt",
  sep = ",", row.names = FALSE, quote = FALSE)
org_jpn$Usage <- readLines("R/usg_jpn.txt", skipNul = TRUE)[-1] %>%
  dplyr::as_tibble()

# Details
for (i in 1:nrow_org) {
  if(is.na(org$Details[i])) {
    org_sub$Details[i] <- NA
    next
  }
  org_sub$Details[i] <- rvest::read_html(org$Details[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    paste0(collapse = "")
}
write.table(x = org_sub$Details[1:500], file = "R/dtl_eng_1.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = org_sub$Details[501:1000], file = "R/dtl_eng_2.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = org_sub$Details[1001:1500], file = "R/dtl_eng_3.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = org_sub$Details[1501:2000], file = "R/dtl_eng_4.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = org_sub$Details[2001:2272], file = "R/dtl_eng_5.txt", sep = ",", row.names = FALSE, quote = FALSE)

org_jpn$Details <- c(
    readLines("R/dtl_jpn_1.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_2.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_3.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_4.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_5.txt", skipNul = TRUE)[-1]
  ) %>%
  dplyr::as_tibble()

jsonlite::write_json(org_jpn, "help_db_jpn_main.json")
jsonlite::fromJSON(txt = "help_db_jpn_main.json") %>%
    dplyr::as_tibble() %>%
    print()

# Arguments
for (i in 1:nrow_org) {
  if(is.na(org$Arguments[i])) {
    org_sub$Arguments[i] <- NA
    next
  }
  org_sub$Arguments[i] <- rvest::read_html(org$Arguments[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    paste0(collapse = "")
}

