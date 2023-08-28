#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
# Set "r-jp-doc" as the working directory.
library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")

org <- jsonlite::fromJSON(txt = "help_db_raw.json") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Value = str_remove_all(string = Value, pattern = "<h3>Values</h3>"),
      Details = str_remove_all(string = Details, pattern = "<h3>Details</h3>"),
      Examples = str_remove_all(string = Examples, pattern = "<h3>Examples</h3>"),
      References = str_remove_all(string = References, pattern = "<h3>Referencess</h3>"),
      See_Also = str_remove_all(string = See_Also, pattern = "<h3>See Also</h3>")
    ) %>%
    print()
org_sub <- org_jpn <- org %>%
  dplyr::select(-Arguments)
nrow_org <- nrow(org_sub)

# Description
for (i in 1:nrow_org) {
  org_sub$Description[i] <- rvest::read_html(org$Description[i]) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()
}
write.table(x = org_sub$Description, file = "R/dsc_eng.txt",
  sep = ",", row.names = FALSE, quote = FALSE)
org_jpn$Description <- readLines("R/dsc_jpn.txt", skipNul = TRUE)[-1]

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
org_jpn$Usage <- readLines("R/usg_jpn.txt", skipNul = TRUE)[-1]

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

# DeepLで作業

org_jpn$Details <- c(
    readLines("R/dtl_jpn_1.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_2.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_3.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_4.txt", skipNul = TRUE)[-1],
    readLines("R/dtl_jpn_5.txt", skipNul = TRUE)[-1]
  )
jsonlite::write_json(org_jpn, "help_db_jpn_main.json")
openxlsx::write.xlsx(x = org_jpn, file = "help_db_jpn_main_確認用.xlsx")


# Argumentsだけのオブジェクトを作成する

arguments_eng <- NULL
for (i in 1:nrow_org) {
  if(is.na(org$Arguments[i])) {
    next
  }
  tmp_org <- org %>%
    dplyr::slice(i) %>%
    dplyr::pull(Arguments) %>%
    rvest::read_html(.) %>%
    rvest::html_nodes("td") %>%
    rvest::html_text()
  codetmp <- tmp_org[seq(1, length(tmp_org), 2)]
  desctmp <- tmp_org[seq(2, length(tmp_org), 2)]  
  argnum <- length(codetmp)
  tmp <- tibble::tibble(
    pack = rep(org$pack[i], argnum),
    func = rep(org$func[i], argnum),
    code = codetmp,
    desc = desctmp
  )
  arguments_eng <- bind_rows(arguments_eng, tmp)
}
print(arguments_eng)
write.table(x = arguments_eng$desc[1:10000], file = "R/arg_desc_eng_1.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = arguments_eng$desc[10001:17143], file = "R/arg_desc_eng_2.txt", sep = ",", row.names = FALSE, quote = FALSE)
length(arguments_eng$desc)

arg_desc_jpn <- c(
    readLines("R/arg_desc_jpn_1.txt", skipNul = TRUE)[-1],
    readLines("R/arg_desc_jpn_2.txt", skipNul = TRUE)[-1]
  )
arguments_jpn <- arguments_eng
arguments_jpn$desc <- arg_desc_jpn
print(arguments_jpn)
jsonlite::write_json(arguments_jpn, "help_db_jpn_arguments.json")
openxlsx::write.xlsx(x = arguments_jpn, file = "help_db_jpn_arguments_確認用.xlsx")
