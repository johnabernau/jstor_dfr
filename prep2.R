#################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Data Prep #2
# Read ngrams and convert to "pseudo raw text"
##########################################################
require(tidyverse)
require(quanteda)
require(dplyr)
#########################################################
# Consolidate into one process
#########################################################

# jstor_expand() takes a file and returns the name and the pseudo raw text
# Use map to run this over all files in n_files
# Once you have a function, you can use safely() to get around any errors


# Set up files paths
path <- "/Users/jberna5/Desktop/ASR JSTOR/ngram1/"
n_files <- list.files(path)

#########################################################################
# Writing function
#########################################################################
# Input the file name
jstor_expand <- function(fname){
  text1 <- read.table(paste0(path, fname))
  text1 <- text1 %>% 
    rename(word = V1, n = V2)
  text1$word <- as.character(text1$word)
  text1$n <- as.numeric(text1$n)
  text1$file <- fname
  
  # Check to see if a digit is present and count characters. 
  # Then drop if a number or less than 3 characters.
  # Drop these two variables
  text1 <- text1 %>% 
    mutate(digit = str_detect(word, "[:digit:]"),
           len = str_length(word)) %>% 
    filter(digit == F & len > 2) %>% 
    select(-digit, -len)
  
  # Repeat word by its frequency, then collapse and save as a pseudo raw text
  text1$ex <- NA
  for (y in 1:nrow(text1)){
    # Repeat x word, n times...
    a <- rep(text1$word[y], text1$n[y])
    # Collapse this into a single string and save as 'ex' variable.
    text1$ex[y] <- str_c(a, collapse = " ")
  }
  
  text_collapsed <- str_c(text1$ex, collapse = " ")
  return <- cbind(fname, text_collapsed)
  
  # Print out for progress
  index <- str_which(n_files, fname)
  if (index %% 5 == 0){
    print(paste0("Expanding file # ", index, "/", length(n_files)))
    print(Sys.time())
  }
  return
}

#########################################################################

# safely() wraps around function for error-proofing
safely_jstor <- safely(jstor_expand)

# Map the safely_jstor function onto each item in n_files. Save as list "asr_expand"
asr_expand <- map(n_files, safely_jstor)

# Unlist result into a dataframe
asr_df <- data.frame(matrix(unlist(asr_expand), nrow=length(asr_expand), byrow=T), stringsAsFactors=FALSE)

# Manipulate data types
asr_df <- asr_df %>% 
  mutate(file = unlist(X1), text = as.character(X2)) %>% 
  select(-X1, -X2)
str(asr_df)

# Save when done
save(asr_df, file = "PATH_HERE/file2.RData")

