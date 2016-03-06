#!/usr/bin/Rscript

X11()
data <- read.csv("data.csv", head=TRUE)
plot(data$Number.of.processes, data$Execution.time..micro.second.)

message("Press Return To Continue")
invisible(readLines("stdin", n=1))
