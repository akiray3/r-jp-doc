#### Script for collecting R help function text.
library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    print()

write.table(x = org$Description, file = "R/dsc_eng.txt",
  sep = ",", quote = FALSE, row.names = FALSE)
seq_i <- c(seq(1, nrow(org), 500), nrow(org))
for (i in 2:length(seq_i)) {
  write.table(
    x = org$Arguments[seq_i[i - 1]:seq_i[i]],
    file = paste0("R/arg_eng_", i - 1, ".txt"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE
  )
  write.table(
    x = org$Details[seq_i[i - 1]:seq_i[i]],
    file = paste0("R/dtl_eng_", i - 1, ".txt"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE
  )
}

# desc
dsc_jpn <- readLines("R/dsc_jpn.txt", skipNul = TRUE)[-1]
dsc_eng <- readLines("R/dsc_eng.txt", skipNul = TRUE)[-1]
dsc_fnc <- rep(org$func, times = as.numeric(sapply(dsc_eng_list, length)))
desc_je <- tibble::tibble(jpn = dsc_jpn, eng = dsc_eng) %>%
  dplyr::filter(eng != "") %>%
  dplyr::mutate(func = dsc_fnc) %>%
  dplyr::select(func, eng, jpn) %>%
  print()
org_nrow <- nrow(org)
for (i in 1:org_nrow) {
  jpn_i <- desc_je %>%
    dplyr::filter(func == org$func[i]) %>%
    dplyr::pull(jpn) %>%
    paste0(collapse = "")
  org$Description[i] <- paste0("<h3>説明</h3><p>", jpn_i, "</p>")
}

# arg
arg_jpn <- c(readLines("R/arg_jpn_1.txt", skipNul = TRUE)[-1],
    readLines("R/arg_jpn_2.txt", skipNul = TRUE)[-1])
arg_eng <- c(readLines("R/arg_eng_1.txt", skipNul = TRUE)[-1],
    readLines("R/arg_eng_2.txt", skipNul = TRUE)[-1])
arg_fnc <- rep(org$func, times = as.numeric(sapply(arg_eng_list, length)))
arg_je <- tibble::tibble(jpn = arg_jpn, eng = arg_eng) %>%
  dplyr::filter(eng != "") %>%
  dplyr::mutate(func = arg_fnc) %>%
  dplyr::select(func, eng, jpn) %>%
  print()
for (i in 1:org_nrow) {
  jpn_i <- arg_je %>%
    dplyr::filter(func == org$func[i]) %>%
    dplyr::pull(jpn) %>%
    paste0(collapse = "")
  org$Description[i] <- paste0("<h3>説明</h3><p>", jpn_i, "</p>")
}
