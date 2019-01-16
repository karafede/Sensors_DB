

# setwd("C:/JRC_CA/AA AQSens")
setwd("L:/ERLAP/Diffusion/AQSens/Deliverables/2.1 review")

library(dplyr)

#############################################################################################################################################
##### Alphabetically reorder all the references insereted in the RMardown documents, as reproted at the beginning....after All..[@..]. ######
#############################################################################################################################################

# load the "reference_as_from_lib.txt" file obtained by saving the references reported in the Rmarkdown document just at the beginning of the script
# remember to add a space at the beginning of the first row.
ref_lib <- read.table("reference_as_from_lib.txt", header = FALSE, sep = ";")

# transpose the file
ref_lib <- t(ref_lib)
ref_lib <- as.data.frame(ref_lib)

# order all the reference in ALPHABETIC order
ref_lib <- ref_lib[order(ref_lib$V1),]
ref_lib <- as.data.frame(ref_lib)

# collapse all the reference along one line
list_ref_alphabet <- ref_lib %>%
    summarise(references = paste(unique(ref_lib), collapse=";"))

write.csv(ref_lib, "ref_lib_alphabetic.csv")

write.table(list_ref_alphabet, "list_ref_alphabet.txt")
# oper the .txt file and copy & paste all the reference in the Rmarkdown document (include only @ and not other symbols except
# for the line spacing with ";")

