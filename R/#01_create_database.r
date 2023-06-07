#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.
library(package = "tidyverse")
library(package = "jsonlite")
library(package = "rvest")
library(package = "psych")
sessionInfo() 
pkgs <- c("base", "stats", "graphics", "psych") # temporary
looplist <- data.frame(
            pack = rep(
                x = pkgs, 
                times = sapply(
                    X = pkgs, 
                    FUN = function(x){
                        length(ls(paste0("package:", x)))
                    }
                )
            ),
            func = unlist(
                sapply(X = pkgs, FUN = function(x){
                        ls(paste0("package:", x))
                    }
                )
            )
        )
outorg <- tibble::tibble(
        pack = NULL,
        func = NULL,
        var = NULL,
        val = NULL
    )
for (i in 1:nrow(looplist)) {
    try01 <- try({
        help_org <- utils:::.getHelpFile(help(topic = looplist$func[i]))
    }, silent = TRUE)
    if (class(try01) == "try-error") {
        try02 <- try({
            file <- help(topic = looplist$func[i])
            filenum <- which(basename(file) == looplist$func[i])
            RdDB <- file.path(dirname(file)[filenum], looplist$pack[i])
            help_org <- tools:::fetchRdDB(RdDB, looplist$func[i])
        }, silent = TRUE)
        if (class(try02) == "try-error") {
            try03 <- try({
                file <- help(topic = looplist$func[i])
                filenum <- str_detect(
                        string = dirname(file), 
                        pattern = looplist$pack[i]
                    )
                if(sum(filenum) > 1) {
                    filenum <- 1
                }
                RdDB <- file.path(dirname(file), looplist$pack[i])
                help_org <- tools:::fetchRdDB(RdDB[filenum], basename(file)[filenum])
            }, silent = TRUE)
            if (class(try03) == "try-error"){
                if (class(try03) == "try-error") {
                    cat(paste0("i = ", i), fill = TRUE)
                    cat(looplist$pack[i], fill = TRUE)
                    cat(looplist$func[i], fill = TRUE)
                    print(basename(file))
                    cat(try03, fill = TRUE)
                    next
                }
            }
        }
    }
    help_html <- capture.output(tools:::Rd2HTML(help_org))
    startstop <- data.frame(
            item = help_html[grep(x = help_html, pattern = "<h3>")],
            start = grep(x = help_html, pattern = "<h3>"),
            stop = NA
        )
    startstop$stop <- c(
            startstop$start[2:nrow(startstop)] - 1, 
            length(help_html)
        )
    addrows <- tibble::tibble(
            pack = looplist$pack[i],
            func = looplist$func[i],
            var = startstop$item %>%
                stringr::str_remove_all(pattern = "<h3>|</h3>") %>%
                stringr::str_replace_all(pattern = " ", replacement = "_"),
            val = NA
        )
    for (j in 1:nrow(startstop)) {
        addrows$val[j] <- paste0(
                help_html[startstop$start[j]:startstop$stop[j]],
                collapse = ""
            )
    }
    print(addrows)
    outorg <- dplyr::bind_rows(outorg, addrows)
}
typicalvar <- table(outorg$var)[table(outorg$var) > (nrow(looplist) / 2)]
out <- outorg %>%
    dplyr::filter(
        var %in% names(typicalvar)
    ) %>%
    dplyr::group_by(pack, func) %>%
    tidyr::spread(key = var, value = val) %>%
    dplyr::select(pack, func, Description, Usage, Arguments, Value,
        Details, Examples, References, See_Also)
jsonlite::write_json(out, "help_db_raw.json")
