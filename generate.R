setwd("c:/capstone/")

### Prepare
# Load libraries and set basic options
  library(tm)
  library(slam)
  library(reshape)
  set.seed(1)
  options(mc.cores=3)

# Function to take a 0.05 sample of the dataset
  sampler <- function(dataset) 
  { 
    return(dataset[as.logical(rbinom(length(dataset),1,0.06))]) 
  } 

### Prepare Input data
# Load files
  con       <- file("files/en_US.news.txt", open="rb")
  i_news    <- readLines(con, encoding="UTF-8")
  close(con)
  rm(con)
  
  con       <- file("files/en_US.blogs.txt", open="rb")
  i_blogs   <- readLines(con, encoding="UTF-8")
  close(con)
  rm(con)
  
  con       <- file("files/en_US.twitter.txt", open="rb")
  i_twitter <- readLines(con, encoding="UTF-8")
  close(con)
  rm(con)

#Create combined data sample, remove non A-Z a-z [whitespace] characters
  input     <- c(sampler(i_blogs),sampler(i_twitter),sampler(i_news))

# Remove non A-Z a-z [whitespace] characters, remove excess whitespace and make all lower case
  input     <- gsub("[^A-Za-z ]","",input)
  rm(i_blogs,i_news,i_twitter)
  
  input     <- Corpus(VectorSource(input))
  input     <- tm_map(input, content_transformer(tolower))
  input     <- tm_map(input, stripWhitespace)

### N-gram Logic
  logic2        <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
  logic3        <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
  
  ngram1        <- list(tokenize = words)
  ngram2        <- list(tokenize = logic2)
  ngram3        <- list(tokenize = logic3)
  
  tdm1          <- TermDocumentMatrix(input,control = ngram1)
  tdm2          <- TermDocumentMatrix(input,control = ngram2)
  tdm3          <- TermDocumentMatrix(input,control = ngram3)
  
  count1        <- rowapply_simple_triplet_matrix(tdm1,sum)
  count2        <- rowapply_simple_triplet_matrix(tdm2,sum)
  count3        <- rowapply_simple_triplet_matrix(tdm3,sum)
  
  value2_1        <- sapply(strsplit(names(count2), ' '), function(a) a[1])
  value2_2        <- sapply(strsplit(names(count2), ' '), function(a) a[2])
  value3_1        <- sapply(strsplit(names(count3), ' '),function(a) a[1])
  value3_2        <- sapply(strsplit(names(count3), ' '),function(a) a[2])
  value3_3        <- sapply(strsplit(names(count3), ' '),function(a) a[3])
  
  gram1           <- data.frame(names(count1),count1,stringsAsFactors = F)
  names(gram1)    <- c('unigram','count')
  gram1_sum       <- as.numeric(nrow(gram1))
  gram1           <- head(gram1[order(gram1$count, decreasing=TRUE),],1)
  gram1           <- transform(gram1, s = count/gram1_sum)
  gram1           <- gram1[,!(names(gram1) %in% c("count"))]

  gram2           <- data.frame(names(count2),count2,value2_1,value2_2,stringsAsFactors = F)
  names(gram2)    <- c('bigram','count','input','output')
  melt2           <- melt(gram2, id=c('bigram','input','output'), measure=c('count'))
  summed2         <- cast(melt2, input ~ variable, sum)
  names(summed2)[names(summed2)=="count"] <- "sum"
  gram2           <- gram2[order(gram2$count, decreasing=TRUE),]
  gram2           <- gram2[!duplicated(gram2$input),]
  gram2           <- merge(gram2, summed2, by="input")
  rm(summed2)
  rm(melt2)
  gram2           <- transform(gram2, s = count/sum)
  gram2           <- gram2[,!(names(gram2) %in% c("count","sum"))]

  gram3           <- data.frame(names(count3),count3,paste(value3_1,value3_2),value3_3,stringsAsFactors = F)
  names(gram3)    <- c('trigram','count','input','output')
  melt3           <- melt(gram3, id=c('trigram','input','output'), measure=c('count'))
  summed3         <- cast(melt3, input ~ variable, sum)
  names(summed3)[names(summed3)=="count"] <- "sum"
  gram3           <- gram3[order(gram3$count, decreasing=TRUE),]
  gram3           <- gram3[!duplicated(gram3$input),]
  gram3           <- merge(gram3, summed3, by="input")
  rm(summed3)
  rm(melt3)
  gram3           <- transform(gram3, s = count/sum)
  gram3           <- gram3[,!(names(gram3) %in% c("count","sum"))]

  save(gram1,gram2,gram3,file = 'ngram3.RData')
