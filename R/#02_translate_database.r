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

dsc_jpn <- readLines("R/jpn_dsc.txt", skipNul = TRUE)[-1]
dsc_eng <- readLines("R/eng_dsc.txt", skipNul = TRUE)[-1]
func_part <- rep(org$func, times = as.numeric(sapply(dsc_eng_list, length)))
length(dsc_jpn)
length(func_part)

desc_je <- tibble::tibble(jpn = dsc_jpn, eng = dsc_eng) %>%
  dplyr::filter(eng != "") %>%
  dplyr::mutate(func = func_part) %>%
  dplyr::select(func, eng, jpn) %>%
  print()
org_nrow <- nrow(org)
for (i in 1:org_nrow) {
  desc_i <- desc_je %>%
    dplyr::filter(func == org$func[i])
  eng_i <- desc_i %>%
    dplyr::pull(eng) %>%
    paste0(collapse = "")
  jpn_i <- desc_i %>%
    dplyr::pull(jpn) %>%
    paste0(collapse = "")
  org$Description[i] <- stringr::str_replace(
      string = org$Description[i],
      pattern = eng_i,
      replacement = jpn_i
    )


}

x <- "Create a contingency table (optionally a sparse matrix) fromcross-classifying factors, usually contained in a data frame,using a formula interface."
y <- "<h3>Description</h3><p>Create a contingency table (optionally a sparse matrix) fromcross-classifying factors, usually contained in a data frame,using a formula interface.</p>"

str_detect(string = y, pattern = x)
regexpr(pattern = x, text = y)
regexpr(pattern = "apple", text = y)
regexpr(pattern = "<p>", text = y)
charmatch(x = "<p>", table = y)

startnum <- gregexpr(pattern = "<p>", text = thistext)[[1]] + 3
stopnum <- gregexpr(pattern = "</p>", text = thistext)[[1]] - 1
transtext <- substring(text = thistext, first = startnum, last = stopnum)


"These unary and binary operators perform arithmetic on numeric orcomplex vectors (or objects which can be coerced to them)."
"<h3>Description</h3><p>These unary and binary operators perform arithmetic on numeric orcomplex vectors (or objects which can be coerced to them).</p>"