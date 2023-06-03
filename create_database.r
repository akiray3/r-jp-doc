#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.

library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")
library(package = "psych")

sessionInfo() 
pkgs <- c("base", "stats", "graphics", "psych")
out <- tibble::tibble(
            pack = pkg, 
            func = rep(fncs, each = ),
            description = NA,
            usage = NA,
            arguments = NA,
            value = NA,
            methods = NA,
            see_also = NA,
            examples = NA
        )
ls("package:base")
ls("package:psych")
ls("package:dplyr")

for (pkg in pkgs) {
    fncs <- ls(paste0("package:", pkg))
    addrows <- tibble::tibble(
            pack = pkg, 
            func = rep(fncs, each = ),
            description = NA,
            usage = NA,
            arguments = NA,
            value = NA,
            methods = NA,
            see_also = NA,
            examples = NA
        )
    for (i in 1:length(fncs)) {
        help_org <- try({
                utils:::.getHelpFile(help(topic = fncs[i]))
            }, silent = TRUE)
        help_html <- capture.output(tools:::Rd2HTML(help_org))
        sec1n <- grep(x = help_html, pattern = "<h3>Description</h3>")
        sec2n <- grep(x = help_html, pattern = "<h3>Usage</h3>")
        sec3n <- grep(x = help_html, pattern = "<h3>Arguments</h3>")
        sec4n <- grep(x = help_html, pattern = "<h3>Value</h3>")
        sec5n <- grep(x = help_html, pattern = "<h3>Methods</h3>")
        sec6n <- grep(x = help_html, pattern = "<h3>See Also</h3>")
        sec7n <- grep(x = help_html, pattern = "<h3>Examples</h3>")
        addrows$description[i] <- paste0(help_html[sec1n:(sec2n - 1)], collapse = "")
        addrows$usage[i] <- paste0(help_html[sec2n:(sec3n - 1)], collapse = "")
        addrows$arguments[i] <- paste0(help_html[sec3n:(sec4n - 1)], collapse = "")
        addrows$value[i] <- paste0(help_html[sec4n:(sec5n - 1)], collapse = "")
        addrows$methods[i] <- paste0(help_html[sec5n:(sec6n - 1)], collapse = "")
        addrows$see_also[i] <- paste0(help_html[sec6n:(sec7n - 1)], collapse = "")
        addrows$examples[i] <- paste0(help_html[sec7n:length(text_org)], collapse = "")




        addrows$val[i] <- help_txt
    }
    out <- out %>%
        dplyr::bind_rows(.,  addrows)
}
write_json(out , "help_db_raw.json")

x <- utils:::.getHelpFile(help(topic = "right_join"))



test  <- read_html(x = htmlx)
rvest::html_children(test)
rvest::html_text(test, trim = FALSE)
rvest::html_text(test, trim = TRUE)
rvest::html_node(test, css = "body") %>%
    rvest::html_text()
rvest::html_node(test, css = "h3")

html_element(test, css="body") %>%
　　　　html_text()
