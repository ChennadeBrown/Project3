rmarkdown:: render("ReadME.Rmd",
                   output_format = "github_document",
                   output_file = "README.md",
                   output_options = list(html_preview = FALSE, keep_html = FALSE))