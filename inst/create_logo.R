install.packages("hexSticker")
library(hexSticker)

library(lattice)

imgurl <- system.file("figures/cat.png", package="hexSticker")

sticker(subplot = '/Users/jakepowell/OME/OMEAPI/inst/figures/cat2.png', package="OMESurvey",
        p_size=20, s_x=1, s_y=.7, s_width=.7,
        h_fill="#009BC1", h_color="#10263B",
        filename="inst/figures/imgfile.png")
usethis::use_logo("inst/figures/imgfile.png")

