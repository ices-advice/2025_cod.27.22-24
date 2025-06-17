
#setwd("C:/Users/jostou/OneDrive - Danmarks Tekniske Universitet/TAF_assesments/2025_cod.27.22-24_assessment")

library(icesTAF)

sam_assessment <- "WBCod2025"

mkdir("boot/data/sam_data")

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/data/"
  )

files <-
  paste0(
    c("cn", "cw", "dw", "lf", "lw", "mo", "nm", "pf", "pm", "survey", "sw"),
    ".dat"
  )


wd <- getwd()
setwd(paste0(getwd(), "/boot/data/sam_data"))

for (file in files) {
  download(paste0(sam_dir, file)) #stupid downloader cannot have change in directory
}

setwd(wd)


