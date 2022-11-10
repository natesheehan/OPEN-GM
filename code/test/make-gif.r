library(magick)

## list file names and read in
imgs = list.files("plots/GISAID/",pattern = "Landscape", full.names = TRUE)
imgs2 = list.files("plots/EMBL/",pattern = "Landscape",  full.names = TRUE)

file = list.files("plots/GISAID/",pattern = "Landscape")
file = stringr::str_remove_all(file,".png")

for (i in 1:6) {
  comb = c(imgs[i], imgs2[i])

  img_list = lapply(comb, image_read)

  ## join the images together
  img_joined = image_join(img_list)

  ## animate at 2 frames per second
  img_animated = image_animate(img_joined, fps = 100)

  ## save to disk
  image_write(image = img_animated,
              path = paste0("plots/presentation/",file[i],".gif"))

}



