## ---- analyze_doc_density


#==============================================================================#
#                                tagChunk                                      #
#==============================================================================#
#'  tagChunk
#' 
#' This function takes as its parameter, the a list of text to be tagged, as 
#' well as the sentence, word, and POS tag annotators, and returns
#' the POS distributions of the list of text
#' 
#' @param chunk - a chunk (list) of text to be tagged
#' @param chunkNum - the chunk number
#' @param sentAnnotator - the openNLP sentence annotator
#' @param wordAnnotator - the openNLP word annotator
#' @param posAnnotator - the openNLP POS annotator
#' @param posTags - pos tags and descriptions
#' @return tags - 1 row data frame with frequencies for each feature
#' @author John James
#' @export
tagChunk <- function(chunk, chunkNum, sentAnnotator, wordAnnotator,
                     posAnnotator, posTags) {
  
  textData <- as.String(chunk)
  
  # Format sentence and word annotator
  a2 <- NLP::annotate(textData, list(sentAnnotator, wordAnnotator))
  
  # Format POS Annotator
  a3 <- NLP::annotate(textData, posAnnotator, a2)
  
  # Extract POS tag distributions
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tagsTable <- as.data.frame(table(tags))
  rownames(tagsTable) <- tagsTable[, 1]
  tagsTable$Freq <- as.numeric(tagsTable$Freq)
  tagsTable <- subset(tagsTable, tags %in% posTags$Tag)
  tagsTableLong <- cbind(chunkNum = chunkNum, tagsTable)
  tagsTableWide <- as.data.frame(t(tagsTable))
  tagsTableWide <- tagsTableWide[-1, ]
  
  # Extract word/pos tag pairs
  pairs <- sprintf("%s/%s", textData[a3w], tags)
  
  tagData <- list(tagsTableLong = tagsTableLong,
                  tagsTableWide = tagsTableWide, 
                  pairs = pairs)
  
  return(tagData)
}


#==============================================================================#
#                                  analyzeDocument                             #
#==============================================================================#
#'  analyzeDocument
#' 
#' This function takes as its parameters, the listed tokenized document 
#' to be tagged and returns a list containing:
#' - a data frame of descriptive statistics for each POS tag
#' - a list of token / pos tags pairs
#' 
#' @param document - the document to tag in unlisted token format
#' @param posTags - data frame of pos tags and descriptions
#' @return analysis - analysis of lexical density for the file
#' @author John James
#' @export
analyzeDocument <- function(document, posTags) {
  
  # Initialize Annotators
  sentAnnotator <- Maxent_Sent_Token_Annotator()
  wordAnnotator <- Maxent_Word_Token_Annotator()
  posAnnotator <- Maxent_POS_Tag_Annotator()
  
  # Prepare a list of data frames, one per chunk, with feature frequencies
  taggedChunks <- lapply(seq_along(document), function(x) {
#    cat("\r......tagging chunk", x, "out of", length(document), "chunks                 ")
    tagChunk(document[[x]], x, sentAnnotator, wordAnnotator,
             posAnnotator, posTags)
  })
  
  # Combine the list of long data frames into a single data frame for all chunks
  chunkMatrix <- rbindlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$tagsTableLong
  }))
  
  # Combine the list of wide data frames into a single data frame for all chunks
  featureMatrix <- rbindlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$tagsTableWide
  }), fill = TRUE)
  
  # Combine the list of data frames into a single data frame for all chunks
  tagPairs <- unlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$pairs
  }))
  
  # Calculate descriptive statistics and sample sizes
  features <- names(featureMatrix)
  featureStats <- rbindlist(lapply(seq_along(featureMatrix), function(x) {
    min <- min(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    max <- max(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    mean <- mean(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    range <- max - min
    total <- sum(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    sd <- sd(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    vc <- sd / mean  # normalized deviation
    te <- .05 * mean
    n <- sd^2 / (te/1.96)^2
    tag <- features[[x]]
    desc <- subset(posTags, Tag == tag, select = Description)
    df <- data.frame(tag = tag, desc = desc, min = min, max = max, 
                     mean = mean, range = range, total = total, sd = sd, 
                     vc = vc, te = te, n = n)
    df[complete.cases(df),]
  }))
  
  avgVc <- mean(featureStats$vc)
  
  analysis <- list(
    avgVc = avgVc,
    chunkMatrix = chunkMatrix,
    featureStats = featureStats,
    tagPairs = tagPairs
  )
  
  return(analysis)
}
#==============================================================================#
#                             analyzeDocDensity                                #
#==============================================================================#
#'  analyzeDocDensity
#' 
#' This function takes as its parameter, the document in word token format, the
#' document meta data, the number of chunks and the chunk size for sampling, 
#' and returns an analysis of the density of the document.  The  analysis includes 
#' descriptive statistics on the POS tags in the document as a list of tag/word 
#' pairs.
#' 
#' @param document - the document to be analyzed
#' @param metaData - the document meta data
#' @param numChunks - the number of chunks to be sampled from document
#' @param chunkSize - size of each chunk sampled from the document
#' @param regex - regex patterns
#' @param posTags - pos tags and their descriptions
#' @return analysis - analysis of lexical density for the file
#' @author John James
#' @export
analyzeDocDensity <- function(document, metaData, numChunks, chunkSize, regex,
                              posTags) {
  
  # Notify user
  message(paste('\n...conducting density analysis of ', metaData$fileDesc, 
                'at', Sys.time()))
  
  # Split data into chunks
  message(paste('......sampling', numChunks, 'chunks from document'))
  textChunks <- sampleData(document, numChunks = numChunks, 
                           chunkSize = chunkSize, format = 'lv')
  
  # Extract tokens and Words
  message('......extracting words from samples')
  sampleTokens <- quanteda::tokenize(unlist(textChunks))
  numTokens <- length(sampleTokens)
  words <- sampleTokens[nchar(sampleTokens) > 0]
  numWords <- length(words)
  
  # Prepare tag analysis
  message('......conducting document tag analysis')
  tagAnalysis <- analyzeDocument(textChunks, posTags)
  
  # Format results
  analysis <- list(
    title = metaData$category,
    numTokens = numTokens,
    numWords = numWords,
    sampleSize = numChunks,
    sampleLength = chunkSize,
    avgVc = tagAnalysis$avgVc,
    chunkMatrix = tagAnalysis$chunkMatrix,
    featureStats = tagAnalysis$featureStats,
    tagPairs = tagAnalysis$tagPairs
  )
  
  # Notify user
  message(paste('...completing density analysis of ', metaData$fileDesc, 
                'at', Sys.time()))  
  return(analysis)
}
## ---- end