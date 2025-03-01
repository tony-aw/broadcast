
# first, terminate R
# then, set working directory to source file locations

library(quarto)
library(stringi)

file.copy("index.qmd", "vignettes/a_readme.qmd", overwrite = TRUE)



quarto_render()

# wait until render is done!
quarto_preview()
