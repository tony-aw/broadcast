
# NOTE: make sure the Quarto client is installed!
altdoc::setup_docs(tool = "quarto_website", overwrite = TRUE)

future::plan("multicore")
altdoc::render_docs(verbose = TRUE, parallel = TRUE, freeze = TRUE)
altdoc::preview_docs()
