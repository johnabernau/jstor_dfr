######################################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Data Prep (contingency)
# Negative page length issue
######################################################

# Sometimes page length will return negative values
# This is likely a difference in reporting full first page (184) and abbreviated last page (87).

# ID problem cases
table(fd$length < 0)

# Create variable for string length
fd$f_stlen <- str_length(fd$fpage)
fd$l_stlen <- str_length(fd$lpage)
fd$l_diff <- fd$f_stlen - fd$l_stlen

# Get rid of length vars
fd <- select(fd, -f_stlen, -l_stlen)

# If l_diff is > 0 it's a problem
table(fd$l_diff)

############################################
# Extract missing digits from first page
############################################
# Initialize variable
fd$extract <- NA
# For each row x...
for (x in 1:nrow(fd)){
  # If the difference is positive...
  if (fd$l_diff[x] > 0)
    # Extract from f_parse starting at 1, and ending at however many digits were different (l_diff)
    fd$extract[x] <- str_sub(fd$fpage[x], 
                             start = 1, end = fd$l_diff[x]) 
}

############################################
# Now paste them together
############################################
fd$last <- NA
# For each row...
for (x in 1:nrow(fd)){
  # If extract isn't missing...
  if (!is.na(fd$extract[x])){
    # Paste the extracted + existing.  
    fd$last[x] <- paste0(fd$extract[x], fd$lpage[x])
  }
  # If extract is missing...
  if (is.na(fd$extract[x])){
    # Just use the original parsed.
    fd$last[x] <- fd$lpage[x]
  }
}
############################################
# Create clean finished variables
############################################
fd$first <- fd$fpage
fd$last <- as.numeric(fd$last)
fd$length2 <- fd$last - fd$first

# Remove unwanted variables (check your df to make sure)
fd2 <- select(fd, -fpage, -lpage, -(f_parse:extract))

fd2$length <- fd2$length2
fd2 <- select(fd2, -length2)

# save(fd2, file = "/Users/jberna5/Desktop/AJSjstor2.RData")
