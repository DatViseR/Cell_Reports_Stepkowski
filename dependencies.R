# This file allows packrat (used by rsconnect during deployment) to pick up dependencies.
library(arrow)
library(dplyr)
library(DT)
library(rhino)
library(tidyr)
library(treesitter)
library(treesitter.r)

box::use(
  shiny[tags],
)

#' @export
font_awesome <- tags$head(
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
)