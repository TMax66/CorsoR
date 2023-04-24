options(prompt = "R>", digits = 2, continue = " ")


#funzione che inizializza un progetto
rproj <- function(){
  dir.create("R")
  dir.create("dati")
  dir.create("report")
  file.create(here("R", "codiciOF.R"))
  file.create(here("report", "notebook.rmd"))
  fileConn<-file(here("report","notebook.rmd"))
  writeLines(c("---",
               "title: 'R Notebook'",
               "output: html_notebook",
               "---", 
               "",
               
               "```{r}",
               "source(here('R', 'librerie.R'))",
               
               "```",

               "# CAPITOLO",
               "## Paragrafo",
  
               "### Subparagrafo", 
               
               "#### Subsubparagrafo"), fileConn)
  close(fileConn)
  
  file.create(here("R", "librerie.R"))
  fileConn2<-file(here("R","librerie.R"))
  writeLines(c("library(tidyverse)", 
               "library(openxlsx)", 
               "library(readxl)"), fileConn2)
  close(fileConn2)
  
  fileConn3 <- file(here("R", "codiciOF.R"))
  writeLines(c("source(here('R', 'librerie.R'))"), fileConn3)
  close(fileConn3)
  
}


library(here)
