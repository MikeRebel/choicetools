# Run from anywhere:  Rscript inst/shiny/cbc-designer/tests/run-tests.R
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(sub("^--file=", "", args[grep("^--file=", args)]))
app  <- normalizePath(file.path(here, ".."))
source(file.path(here, "helper.R"))
for (f in sort(list.files(file.path(app, "R"), pattern = "[.]R$", full.names = TRUE))) source(f)
for (f in sort(list.files(here, pattern = "^test-.*[.]R$", full.names = TRUE))) source(f)
finish_tests()
