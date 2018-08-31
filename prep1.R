#################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Data Prep #1
# Parsing out xml files
#################################

# Install / load packages
# install.packages("xml2")
# install.packages("tidyverse")
require(xml2)
require(tidyverse)
require(plyr)
require(dplyr)

# Identify path to metadata folder and list files
path <- "/Users/jberna5/Desktop/ASR JSTOR/metadata"
files <- list.files(path)

# Initialize empty set
final_data <- NULL

# Using the xml2 package: for each file, extract metadata and append row to final_data
for (x in files){
  path <- read_xml(paste0(path1, "/", x))
  
  # File name
  file <- x
  
  # Article type
  type <- xml_find_all(path, "/article/@article-type") %>% 
    xml_text()
  
  # Title
  title <- xml_find_all(path, xpath = "/article/front/article-meta/title-group/article-title") %>% 
    xml_text()
  
  # Author names
  authors <- xml_find_all(path, xpath = "/article/front/article-meta/contrib-group/contrib") %>%
    xml_text()
  auth1 <- authors[1]
  auth2 <- authors[2]
  auth3 <- authors[3]
  auth4 <- authors[4]
  auth5 <- authors[5]
  auth6 <- authors[6]
  auth7 <- authors[7]
  auth8 <- authors[8]
  auth9 <- authors[9]
  auth10 <- authors[10]
  
  # Affiliations
  affil <- xml_find_all(path, xpath = "/article/front/article-meta/contrib-group/aff") %>%
    xml_text()
  affil1 <- affil[1]
  affil2 <- affil[2]
  affil3 <- affil[3]
  affil4 <- affil[4]
  affil5 <- affil[5]
  affil6 <- affil[6]
  affil7 <- affil[7]
  affil8 <- affil[8]
  affil9 <- affil[9]
  affil10 <- affil[10]
  
  # Abstract
  abstract <- xml_find_all(path, xpath = "/article/front/article-meta/abstract") %>% 
    xml_text()
  
  # Month
  month <- xml_find_all(path, xpath = "/article/front/article-meta/pub-date/month") %>% 
    xml_text()
  
  # Year
  year <- xml_find_all(path, xpath = "/article/front/article-meta/pub-date/year") %>% 
    xml_text()
  
  # Volume
  vol <- xml_find_all(path, xpath = "/article/front/article-meta/volume") %>% 
    xml_text()
  
  # Issue
  iss <- xml_find_all(path, xpath = "/article/front/article-meta/issue") %>% 
    xml_text()
  
  # First page
  fpage <- xml_find_all(path, xpath = "/article/front/article-meta/fpage") %>% 
    xml_text()
  
  # Last page
  lpage <- xml_find_all(path, xpath = "/article/front/article-meta/lpage") %>% 
    xml_text()
  
  # Footnote
  notes <- xml_find_all(path, xpath = "/article/front/notes") %>% 
    xml_text()
  
  # Bind all together
  article_meta <- cbind(file, type, title, 
                        auth1, auth2, auth3, auth4, auth5, auth6, auth7, auth8, auth9, auth10, 
                        affil1, affil2, affil3, affil4, affil5, affil6, affil7, affil8, affil9, affil10,
                        abstract, month, year, vol, iss, fpage, lpage, notes)
  
  final_data <- rbind.fill(final_data, data.frame(article_meta, stringsAsFactors = FALSE))
  
  # Print progress 
  if (nrow(final_data) %% 250 == 0){
    print(paste0("Extracting document # ", nrow(final_data)))
    print(Sys.time())
  }
}

# Check output
names(final_data)
str(final_data)

# Shorter name
fd <- final_data

# Adjust data types
fd$type <- as.factor(fd$type)
fd$month <- as.numeric(fd$month)
fd$year <- as.numeric(fd$year)
fd$vol <- as.numeric(fd$vol)
fd$iss <- as.numeric(fd$iss)

# Remove variables if ALL rows are "NA"
not_all_na <- function(x) any(!is.na(x))
fd <- fd %>% select_if(not_all_na)

# Create date variable
fd$date <- paste("01", fd$month, fd$year, sep = "-")
fd$date <- as.Date(fd$date, "%d-%m-%Y")
class(fd$date)
head(fd$date)

#######################################################
# Page variables
#######################################################
# Examine for any unusual values ("cover", "ix", etc)
View(count(fd$fpage))
View(count(fd$lpage))

# Convert to numeric (roman numerals converted to NA by default)
fd$fpage <- as.numeric(fd$fpage)
fd$lpage <- as.numeric(fd$lpage)
fd$fpage[fd$fpage == ""] <- NA
fd$lpage[fd$lpage == ""] <- NA

# Create length variable
fd$length <- fd$lpage - fd$fpage
# Are you getting negative length variables? 
# See "negative page length.R"

# Examine data
View(arrange(fd, desc(length)))

ggplot(fd, aes(length)) +
  geom_histogram(bins = 50)

#######################################################
# Explore data
#######################################################
# Examine type categories
dplyr::count(fd, type)
# Careful of "research-articles" with titles like "Book Review" or "Review"
filter(fd, type == "research-articles" & title == "Book Reviews")

# Save as .RData file for future work
save(fd, file = "PATH_HERE/file1.RData")
