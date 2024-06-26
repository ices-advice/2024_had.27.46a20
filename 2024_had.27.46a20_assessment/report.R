## Prepare plots and tables for report

## Before:
## After:

rm(list=ls())

library(icesTAF)
library(rmarkdown)
library(tidyverse)
library(flextable)
library(icesAdvice)
library(FLCore)

mkdir("report")


rmarkdown::render(
  input="report_tables.Rmd",
  output_file = "report/report_tables.docx"
)


# for 2025 - add in change in advice tables