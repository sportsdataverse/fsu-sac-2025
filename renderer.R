# if you want to build the slides locally
quarto::quarto_render("slides.qmd")

# duplicate `slides.html` as `index.html`
fs::file_copy("slides.html", "index.html", overwrite = TRUE)

