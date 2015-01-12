# Author: John Letteboer
# Date: 2015-12-01
# Description: This R code read a csv data file and creates a wordcloud of a given column
# Packages Used: tm, wordcloud, RColorBrewer
#
# ===
# Functions
#
# Read file name
filePrompt <- function() {
  file <- readline("Which csv file do you want to analyze? ")
  file <- as.character(file)
  if(!grepl(".csv$", file))
    file <- paste(file, ".csv", sep = "")
  if(!file %in% list.files())
    stop("file not found")
  file
}

# Read column number
colPrompt <- function() {
  colnum <- readline("In which column is the text to be analyzed [1-n]: ")
  colnum <- as.numeric(colnum)
  if(is.na(colnum))
    stop("Invalid column number")
  colnum
}

# Read output format
formatPrompt <- function() {
  format <- readline("In which format do you want the word could: 1-pdf, 2-png: ")
  format <- as.numeric(format)
  if(is.na(colnum) || !format %in% c(1, 2)) {
    message("Invalid format. will use pdf by default.")
    format <- 1
  }
  format
}

# Making word cloud
makeWordCloud <- function(textVec, format = 1) { # 1 = pdf, 2 = png
  require(tm)
  require(wordcloud)
  require(RColorBrewer)

  ap.corpus <- Corpus(DataframeSource(data.frame(textVec)))
  ap.corpus <- tm_map(ap.corpus, tolower)
  ap.corpus <- tm_map(ap.corpus, function(x) {
    removeWords(x, append(stopwords("english"), c("via")))
  })
  ap.corpus <- tm_map(ap.corpus, removePunctuation)
  ap.tdm <- TermDocumentMatrix(ap.corpus)
  ap.m <- as.matrix(ap.tdm)
  ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v), freq=ap.v)
  table(ap.d$freq)
  pal2 <- brewer.pal(8, "Dark2")

  if (format == 2) {
    png("wordcloud.png", width = 800, height = 600)
  } else if (format == 1) {
    pdf("wordcloud.pdf")
  }

  wordcloud(ap.d$word, ap.d$freq,
            scale=c(8, .2), min.freq = 3,
            max.words = Inf, random.order = FALSE,
            rot.per = .15, colors = pal2)

  invisible(dev.off())
}

# Execute
file <- filePrompt()
df <- read.csv(file, colClasses = "character", fileEncoding = "UTF-8")

colnum <- colPrompt()
format <- formatPrompt()
makeWordCloud(df[, colnum], format)
