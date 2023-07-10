#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.
library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    dplyr::select(-Arguments) %>%
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
  org_jpn$Usage[i] <- rvest::read_html(org$Usage[i]) %>%
    rvest::html_nodes("pre") %>%
    rvest::html_text()
}

# Details
for (i in 1:nrow_org) {
  if (is.na(org$Details[i])) {next}
  org_sub$Details[i] <- rvest::read_html(org$Details[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()
}
write.table(x = org_sub$Details, file = "R/dtl_eng.txt",
  sep = ",", row.names = FALSE, quote = FALSE)
org_jpn$Details <- readLines("R/dtl_jpn.txt", skipNul = TRUE)[-1] %>%
  dplyr::as_tibble() %>%
  dplyr::filter(value != "") %>%
  dplyr::mutate(value = gsub(pattern = " ", replacement = "", x = value, fixed=TRUE))






# Detailsの処理
for (i in 1:nrow_org) {
  if (is.na(org$Details[i])) {next}
  org_sub$Details[i] <- rvest::read_html(org$Details[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()
}
write.table(x = org_sub$Details, file = "R/dtl_eng.txt",
  sep = ",", row.names = FALSE, quote = FALSE)
org_jpn$Details <- readLines("R/dtl_jpn.txt", skipNul = TRUE)[-1] %>%
  dplyr::as_tibble() %>%
  dplyr::filter(value != "") %>%
  dplyr::mutate(value = gsub(pattern = " ", replacement = "", x = value, fixed=TRUE))




dsc_jpn$value[5]


head(test)
length(test)

sum(test != "")




org %>%
  dplyr::pull(Description) %>%
  readhtmlsub(.) %>%
  write.table(file = "R/dsc_eng.txt", sep = ",", row.names = FALSE, quote = FALSE)


head(org$Description)
head(jpn$Description)

# Detailsの処理
org %>%
  dplyr::pull(Details) %>%
  stringr::str_remove_all(pattern = "<h3>Details</h3>|<p>|</p>") %>%
  write.table(file = "R/dsc_eng.txt", sep = ",", row.names = FALSE, quote = FALSE)

jpn$Description <- readLines("R/dsc_jpn.txt", skipNul = TRUE)[-1]





org %>%
  dplyr::pull(Details) %>%
  stringr::str_remove_all(pattern = "<h3>Details</h3>|<p>|</p>") %>%
  write.table(file = "R/dtl_eng.txt", sep = ",", row.names = FALSE, quote = FALSE)




dsc_eng_list <- lapply(X = org$Description, FUN = readhtmlsub)
arg_eng_list <- lapply(X = org$Arguments, FUN = readhtmlsub)

head(org$Arguments)


org$Arguments[[2]]

readhtmlsub(org$Arguments[100])

sum(org$Arguments == "", na.rm = TRUE)

head(dsc_eng_list)
names(dsc_eng_list) <- org$func
length(dsc_eng_list)

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






dtl_eng_list[[1]]

length(unlist(dsc_eng_list))

dsc_eng_list %>%

write.table(x = unlist(), file = "R/dsc_eng.txt",
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
dsc_eng <- readLines("R/dsc_eng.txt", skipNul = TRUE)[-1]
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