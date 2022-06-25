rmarkdown::render('README.md',
                  output_format = "github_document",
                  #output_dir = "_posts",
                  output_options = list(
                    html_preview = FALSE, toc = TRUE, toc_depth = 2, toc_float = TRUE)
)

