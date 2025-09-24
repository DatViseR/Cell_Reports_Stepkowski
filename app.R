cat("WD:", getwd(), "\n")
cat(
    "Top-level files:",
    paste(list.files(".", all.files = TRUE), collapse = ", "),
    "\n"
)
# Example: check a file you read
# cat("Exists data/foo.csv:", file.exists(here::here("data", "foo.csv")), "\n")

# Rhino / shinyApp entrypoint. Do not edit.
rhino::app()
