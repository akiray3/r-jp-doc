#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.

library(package = "tidyverse")
library(package = "jsonlite")

sessionInfo() 
pkgs <- c("base", "stats", "graphics")

out <- tibble::tibble(
        pack = NULL, func = NULL,
        var = NULL, val = NULL
    )

for (pkg in pkgs) {
    fncs <- ls(paste0("package:", pkg))
    addrows <- tibble::tibble(
            pack = pkg, func = fncs,
            var = "all_txt_tmp", val = NA
        )
    for (i in 1:length(fncs)) {
        help_txt <- try({
                utils:::.getHelpFile(help(topic = fncs[i])) %>%
                tools:::Rd2HTML(.) %>%
                capture.output(.) %>%
                paste0(., collapse = "")
        }, silent = TRUE)
        addrows$val[i] <- help_txt
    }
    out <- out %>%
        dplyr::bind_rows(.,  addrows)
}
write_json(out , "help_db_raw.json")

x <- utils:::.getHelpFile(help(topic = "mean"))
htmlx <- capture.output(tools:::Rd2HTML(x)) %>%
    paste0(collapse = "")

library(rvest)
test  <- read_html(x = htmlx)
rvest::html_children(test)
rvest::html_text(test, trim = FALSE)
rvest::html_node(test, css = "body") %>%
    rvest::html_text()
rvest::html_node(test, css = "h3")

html_element(test, css="#main h1") %>%
　　　　html_text()
