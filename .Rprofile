if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

# box.lsp languageserver external hook
if (nzchar(system.file(package = "box.lsp"))) {
  options(
    languageserver.parser_hooks = list(
      "box::use" = box.lsp::box_use_parser
    )
  )
}

# Force sequential install to avoid race conditions during dependency builds
options(Ncpus = 1)
Sys.setenv(MAKEFLAGS = "-j1")

# Prefer Posit Package Manager binaries for Ubuntu 22.04 (jammy)
# This helps avoid source builds of openssl and friends.
options(
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
  )
)
