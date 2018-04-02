library(zoo) # For apply.rolling

deal <- function (z) sd(z)

data <- read.csv("mms_20151016.csv")
datai <- data[,c("DES_Density", "DES_Temperature_para", "DES_Temperature_perp"
                 , "FGM_Magnetic_Field_w", "FGM_Magnetic_Field_x"
                 , "FGM_Magnetic_Field_y", "FGM_Magnetic_Field_z")]
windows <- as.data.frame(rollapply(datai, 15, FUN = deal))

#which.max

sorted <- order(windows$FGM_Magnetic_Field_w, decreasing=TRUE)

# Take top N, depending on data limit.
# Overlap problems?