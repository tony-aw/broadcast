
# first, terminate R
# then, set working directory to source file location

library(quarto)

quarto_render()

# wait until render is done!
quarto_preview()


# when all's well, copy all contents of the /website/_site/ folder to the /docs/ folder