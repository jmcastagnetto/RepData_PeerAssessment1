library(knitr)
# make the report
knit2html("PA1_template.Rmd")
# extract code chunks
purl("PA1_template.Rmd")