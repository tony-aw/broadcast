
# first, terminate R
# then, set working directory to source file location

library(quarto)

quarto_render()

# wait until render is done!
quarto_preview()
