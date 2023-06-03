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
            func = fncs,
            description = NA,
            usage = NA,
            arguments = NA,
            value = NA,
            methods = NA,
            see_also = NA,
            examples = NA
        )
    for (i in 1:length(fncs)) {
        trycatch_1 <- try({
            help_org <- utils:::.getHelpFile(help(topic = fncs[i]))
            help_html <- capture.output(tools:::Rd2HTML(help_org))
        }, silent = TRUE)
        if (class(trycatch_1) == "try-error") {
            cat(paste0(pkg, "---", fncs[i], "----------ERROR!"), fill = TRUE)
            next
        }
        help_str <- data.frame(
            item = help_html[grep(x = help_html, pattern = "<h3>")],
            start = grep(x = help_html, pattern = "<h3>"),
            stop = NA
        )
        help_str$stop <- c(help_str$start[2:nrow(help_str)], length(help_html))
        sec1 <- as.numeric(help_str[help_str$item == "<h3>Description</h3>", 2:3])
        sec2 <- as.numeric(help_str[help_str$item == "<h3>Usage</h3>", 2:3])
        sec3 <- as.numeric(help_str[help_str$item == "<h3>Arguments</h3>", 2:3])
        sec4 <- as.numeric(help_str[help_str$item == "<h3>Value</h3>", 2:3])
        sec5 <- as.numeric(help_str[help_str$item == "<h3>Examples</h3>", 2:3])
        try({
            addrows$description[i] <- paste0(help_html[sec1[1]:sec1[2]], collapse = "")
            addrows$usage[i] <- paste0(help_html[sec2[1]:sec2[2]], collapse = "")
            addrows$arguments[i] <- paste0(help_html[sec3[1]:sec3[2]], collapse = "")
            addrows$value[i] <- paste0(help_html[sec4[1]:sec4[2]], collapse = "")
            addrows$examples[i] <- paste0(help_html[sec5[1]:sec5[2]], collapse = "")
        }, silent = TRUE)
        cat(paste0(pkg, "---", fncs[i]), fill = TRUE)
    }
    out <- dplyr::bind_rows(out,  addrows)
}
write_json(out, "help_db_raw.json")

table(is.na(out$description[out$pack == "graphics"]))

out$description[1:100]

test  <- read_html(x = htmlx)
rvest::html_children(test)
rvest::html_text(test, trim = FALSE)
rvest::html_text(test, trim = TRUE)
rvest::html_node(test, css = "body") %>%
    rvest::html_text()
rvest::html_node(test, css = "h3")

html_element(test, css="body") %>%
　　　　html_text()
