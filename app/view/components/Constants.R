box::use(
  shiny[p],
)


ui_info_for_tabPanel <- function() {
  p(
    "You can to hide/show the description and filters by using the arrow on the right", 
    style = "color: #6c757d; font-style: italic; font-size: 0.6rem; margin: 0; white-space: nowrap;
             transform: translateY(-10px);"
  )
}