library(rvest)
text <- capture.output(help(topic = "mean", help_type = "text"))
text <- (help(topic = "mean", help_type = "text"))
str(text)
names(text)
unlist(text)
