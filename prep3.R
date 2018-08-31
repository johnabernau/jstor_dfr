##########################################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Data Prep #3
# Merge metadata with pseudo raw text
##########################################################
require(tidyverse)
##########################################################
# Load file1.RData (full text)
load(file.choose())
full_text <- asr_df
rm(asr_df)

# Load file2.RData (metadata)
load(file.choose())
meta <- fd
rm(fd)

# Make joining column
meta$id <- str_replace_all(meta$file, ".xml", "")
meta$id[1:10]
full_text$id <- str_replace_all(full_text$file, "-ngram1.txt", "")
full_text$id[1:10]

# # Option 1: Join all rows no matter what
# merged <- full_join(meta, full_text, by = "id")

# Option 2: Join only rows with matching ids
merged <- inner_join(meta, full_text, by = "id")

# Examine
names(merged)
View(merged[1:10,])


save(merged, file = "PATH_HERE/file3.RData")
