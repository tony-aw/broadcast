
# first, terminate R
# then, set working directory to source file location

library(quarto)

# always render first
quarto_render()

# wait preview until render is done
quarto_preview()

