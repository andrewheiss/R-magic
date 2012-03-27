library(knitr)

knit("knitr/html_test_knit_.html")

# render_gfm()
# render_markdown()
opts_knit$set(out.format = "markdown")
knit("knitr/test_knit_.md")