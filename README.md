# Text Analysis with JSTOR Archives
Accompanying materials for manuscript "Text Analysis with JSTOR Archives", published 2018 in *Socius: Sociological Research for a Dynamic World* (https://doi.org/10.1177/2378023118809264).

The following R code walks through the preparation of the data and plotting of the figure in the main text of the article. JSTOR provides instructions on creating and requesting a dataset (https://www.jstor.org/dfr/about/creating-datasets), and the following code will require access to the two delivered folders: “metadata” and “ngram1”. 

The “Prep #1” script uses a function to extract JSTOR xml metadata into an R data frame. The analysis in the article uses two separate datasets and only the ASR-specific extraction code is reproduced below. The AJS-dataset was extracted in near-identical fashion. 

The “Prep #2” script uses a function to convert JSTOR ngram data into a “pseudo raw text” format by multiplying each word by its associated frequency. What results is essentially the full text of the article, sorted by word. In other words, if “sociology” was the most frequent word at 150 uses, the first 150 words of the pseudo raw text will be “sociology”.

The “Prep #3” script joins the resultant data frames together using the file name as an identifier. 

With this data frame complete, the concluding script (analysis_plot.R) details plotting decisions, including the frequency analysis of ASR articles and the authorship and regression analysis of AJS articles.  
