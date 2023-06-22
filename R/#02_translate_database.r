#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.

library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    print()
imax <- nrow(org)
dsc_eng_list <- list(NULL)
arg_eng_list <- list(NULL)
dtl_eng_list <- list(NULL)
length(dsc_eng_list) <- length(arg_eng_list) <- length(dtl_eng_list) <- imax
names(dsc_eng_list) <- names(arg_eng_list) <- names(dtl_eng_list) <- org$func

for (i in 1:imax) {
  if (!is.na(org$Description[i])) {
    dsc_eng_list[[i]] <- org$Description[i] %>%
      rvest::read_html() %>%
      rvest::html_nodes("p") %>%
      as.character() %>%
      stringr::str_remove_all(pattern = "<p>|</p>")
  }
  if (!is.na(org$Arguments[i])) {
    arg_eng_list[[i]] <- org$Arguments[i] %>%
      rvest::read_html() %>%
      rvest::html_nodes("p") %>%
      as.character() %>%
      stringr::str_remove_all(pattern = "<p>|</p>")
  }
  if (!is.na(org$Details[i])) {
    dtl_eng_list[[i]] <- org$Details[i] %>%
      rvest::read_html() %>%
      rvest::html_nodes("p") %>%
      as.character() %>%
      stringr::str_remove_all(pattern = "<p>|</p>")
  }
}



rep(org$func, times = as.numeric(sapply(arg_eng_list, length)))
rep(org$func, times = as.numeric(sapply(dtl_eng_list, length)))


length(unlist(dsc_eng_list))
write.table(x = unlist(dsc_eng_list), file = "R/eng_dsc.txt",
  sep = ",", quote = FALSE, row.names = FALSE)
length(unlist(arg_eng_list))
write.table(x = unlist(arg_eng_list)[1:9000], file = "R/eng_arg_1.txt",
  sep = ",", quote = FALSE, row.names = FALSE)
write.table(x = unlist(arg_eng_list)[9001:17480], file = "R/eng_arg_2.txt",
  sep = ",", quote = FALSE, row.names = FALSE)
length(unlist(dsc_eng_list))
write.table(x = unlist(dsc_eng_list), file = "R/eng_dtl.txt",
  sep = ",", quote = FALSE, row.names = FALSE)

dsc_jpn <- readLines("R/jpn_dsc.txt")[-1]
dsc_eng <- readLines("R/eng_dsc.txt")[-1]
desc_je <- tibble::tibble(
    func = rep(org$func, times = as.numeric(sapply(dsc_eng_list, length))),
    jpn = dsc_jpn[dsc_jpn != ""],
    eng = dsc_eng[dsc_eng != ""]
  )

head(dsc_jpn[dsc_jpn != ""])
head(unlist(dsc_eng_list))

which(dsc_jpn == "")
which(unlist(dsc_eng_list) == "")


head(dsc_jpn)
nrow(dsc_jpn)







startnum <- gregexpr(pattern = "<p>", text = thistext)[[1]] + 3
stopnum <- gregexpr(pattern = "</p>", text = thistext)[[1]] - 1
transtext <- substring(text = thistext, first = startnum, last = stopnum)
