#' @include create_q_matrix.R
#'
#' Classify documents or textual notes according to categories appearing in a lexicon, typically in groups (e)nvironment, (t)hought, (e)motion and (a)ction, (etea).
#' 
#' In state-space and decision modelling of textual data it is considered important to classify documents by interval group or categories so that the continuous sentiment domain can be disctretised for sub-analysis.  This function takes a document frequency matrix either from \code{\link[quanteda]} functions or from \code{\link[tm]} functions, typically from the associated function \code{\link[quanteda]{create_q_matrix}} (part of this package) and quantifies each word occurence by matching it against a lexicon of words with associated categories that the user has supplied.  The lexicon/category listing, (csv format) is expected to be found in the data directory associated with the calling function.  This lexicon can contain any number of words categorised by any number of categories to suit user interest.  The example lexicon (herein) contains a small set of words that have been successfully used to conduct proof-of-concept studies investigating crime and similar issues.  The main user changes to the lexicon are expected to extend the 'actions' group such as by changing 'crime words' to any areas of special interest.  The user can run the function \code{\link[quanteda]{etea_features}} to examine a list of words in the documents or notes being studied.  This \code{\link[quanteda]{etea_features}} is simply a wrapper to \code{\link[quanteda]} and \code{\link[tm]} functions but is a useful 'helper' function for those not wholly familiar with those packages.  The function arguments are the same as the \code{\link[quanteda]{create_q_matrix}} function from which it is derived so that users can consider the features found in the documents or notes, segment into categories and easily append them to the lexicon.  It is thus possible that a series of lexicons can be developed to examine different issues.  This function has been designed so that the output is in a form that can easily be leveraged by predictive algorithms that use segmented time-series data.  It is expected that these functions will be of interest to those studying sentiment change over time including inter-alia, neural networks, decision systems and state-space modelling.  The underlying methodology for this function has been extended and developed with permission, from an original by Tim Jurka \code{\link[sentiment]{classify_emotions}}.
#'
#' @author Chris Kirk
#'
#' @return a scored, classified matrix of document/note words as categories to provide input into other analytical systems.
#'
#' @param textColumn character vector containing the text to be analysed; mandatory
#' @param stateNum numeric vector containing identifiers for the condition or state when the document or note was recorded/written that it be correctly allocated in the event of more than one note or record being in a state; default NULL
#' @param timeNum numeric verctor containing an index that identifies when the document was recorded/noted to give a temporal record.  This normalises progress and case note recording as a progress through a system.  Typically days or minutes after the system commenced; default NULL
#' @param docvaragg specifies how the aggregation on docvars is to occur either s stateID only, t timestamp only, st state and timestamp or timestamp and state ; default NULL; Options s, t, st, ts
#' @param use_stopwords specifies whether stopwords are to be removed from the corpus (TRUE) or not removed, (FALSE).  Users are reminded that system (language-specific) stopwords may need additions or removals to tailor for a specific need; default TRUE
#' @param choice of language to determine the content of the basic stopword list; default \code{english}. See \code{\link[quanteda]} for further information.
#' @param add_stopwords a character vector of words to be added to the stopwords vector (if any); default is NULL.
#' @param remove_stopwords a character vector of words to be removed to the stopwords vector (if any); default is NULL.
#' @param verbose to see useful progress information; default is TRUE
#' @param toLower to convert all inbound text into lower case. Notably this will degrade the sentence splitting function if applied; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param stem reduce word length to root; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param removeFeatures remove particular features from inbound text as specified in a list; default is TRUE; see: \code{\link[quanteda]}
#' @param language to define local language; default is "english" see: \code{\link[quanteda]}
#' @param valuetype to define patterning; default is \code{glob}; see: \code{\link[quanteda]}
#' @param removeNumbers remove individual numbers from inbound text, (note: numbers already aggregated with characters such as 1st or 2nd are unaffected); default is TRUE; see: \code{\link[quanteda]}
#' @param removePunct remove punctuation from inbound text; default is TRUE; see: \code{\link[quanteda]}
#' @param removeSeparators remove separators from inbound text; default is TRUE; see: \code{\link[quanteda]}
#' @param removeHyphens remove hyphen characters from inbound text; default is TRUE; see: \code{\link[quanteda]}
#' @param removeTwitter remove twitter api characters from inbound text; default is TRUE; see: \code{\link[quanteda]}
#' @param ngrams integer vector specifying the number of elements to be concatenated in each ngram; default is 1L; see: \code{\link[quanteda]{ngrams}}
#' @param skip integer vector specifying the adjacency skip size for tokens forming the ngrams; \code{0}: see: \code{\link[quanteda]{ngrams}}
#' @param concatenator character for combining words, default is \code{_}; see: \code{\link[quanteda]{ngrams}}
#' @param simplify character vector of tokens rather than a length of texts; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param convert_to_tm logical specifying the requirement for the matrix to be returned in the tm TRUE or quanteda FALSE format
#' @param termNum integer specifying the minimum frequency a word is to have been found in the matrix
#' @param algorithm a character string specifying either "FALSE" or "bayes" to require a frequency score or bayesian probability output
#' @param prior integer specifying the prior bayesian weighting value
#' @param ... Extra arguments, not used
#'
#' @examples
#' # create a scored, classified matrix of CaseNotes and aggregated by state for use in further modelling
#' ## LOAD ##
#' text_df <- read.csv("data/jtr_docs.csv",header=TRUE)
#' textColumn<-as.character(text_df$Notes)
#' ## CLASSIFY ##
#' classify_eta(textColumn, stateNum=c(1,1,2,3,3), verbose=TRUE, use_stopwords=TRUE, docvaragg="s")
#'
#' # create dfm using character vector of CaseNotes, states and datetimestamps for use as a time series for a neural network or MARSS
#' ## LOAD ##
#' text_df <- read.csv("data/militant_suffragette_extract.csv",header=TRUE)
#' textColumn<-as.character(text_df$Notes) # typically textual interview or clinical notes
#' stateNum<-as.numeric(text_df$stateID) # typically identication of parts of journey/episode
#' timeNum<-as.character(text_df$datetimestamp) # typically days since start of journey/episode
#' ## CLASSIFY ##
#' etea_df_time <- classify_etea(textColumn, stateNum, timeNum, verbose=TRUE, use_stopwords=TRUE, docvaragg="t")
#' # for MARSS
#' ## CONVERT FOR MARSS ##
#' etea_matrix <- data.matrix(etea_df_time) # MARSS requires standard data matrix Note timeNum as rownames
#' dat = t(etea_matrix) # transpose to MARSS form
#' colnames(dat) <- rownames(etea_matrix) # set column names to timeNum from docvars (rownames)
#' ## dat is now available as MARSS DATA ##
#'
#' @rdname classify_etea
#'
#' @export
#'
classify_etea <- function(textColumn, stateNum=NULL, timeNum=NULL, docvaragg = "null", use_stopwords=TRUE, stopwords_language="english", add_stopwords=NULL, remove_stopwords=NULL, verbose=TRUE, toLower=FALSE, stem=FALSE, keptFeatures=NULL,  removeFeatures=TRUE, language="english", valuetype = c("glob"), thesaurus=NULL, dictionary=NULL,  removeNumbers=TRUE, removePunct=TRUE, removeSeparators=TRUE, removeHyphens=TRUE, removeTwitter=TRUE, ngrams=1L, skip=0L, concatenator="_", simplify=FALSE, convert_to_tm=TRUE,  algorithm="bayes",prior=1.0, termNum=1,...)
{

seed.val <- 2
set.seed(seed.val)
 
q_matrix <- create_q_matrix(textColumn=textColumn, stateNum=stateNum, timeNum=timeNum, docvaragg=docvaragg, use_stopwords=use_stopwords, stopwords_language=stopwords_language, add_stopwords=add_stopwords, remove_stopwords=remove_stopwords, verbose=verbose, toLower=toLower, stem=stem, keptFeatures=keptFeatures, removeFeatures=removeFeatures, language=language, valuetype=valuetype, thesaurus=thesaurus, dictionary=dictionary, removeNumbers=removeNumbers, removePunct=removePunct, removeSeparators=removeSeparators, removeHyphens=removeHyphens, removeTwitter=removeTwitter, ngrams=ngrams, skip=skip, concatenator=concatenator, simplify=simplify, convert_to_tm=convert_to_tm, ...)  
#
#documents <- data.frame(matrix(vector(), nrow=nrow(q_matrix), ncol=nrow(lexi_cats_df) )) # new df to maintain scores
 
lexicon <- read.csv("data/etea_categories.csv",header=FALSE, stringsAsFactors=FALSE)
lexi_cats_df<-as.data.frame(aggregate(lexicon[,1] ~ lexicon[,2], lexicon, function(x) length(unique(x))))
documents <- data.frame(matrix(vector(), nrow=0, ncol=(nrow(lexi_cats_df)+1) )) # df to maintain scores + 1 col for totals
colnames(lexi_cats_df)[1]<-c("cat") # name categories column
colnames(lexi_cats_df)[2]<-c("catcount") # name counts column
lexi_cats_df[,3]<-0 # create scores column and set to zero
colnames(lexi_cats_df)[3]<-c("score") # name column
lexi_cats_df # debug check
lexi_cats_df<-rbind(lexi_cats_df, total=sum(lexi_cats_df$catcount)) # total counts
lexi_cats_df[nrow(lexi_cats_df),3]<-0 # reset totals value 
lexi_cats_df[nrow(lexi_cats_df),1]<-"total" # apply total name
colnames(documents) <- lexi_cats_df[,1 ] 

  for (i in 1:nrow(q_matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
#reset scores
#   lexi_cats_df[,3]<-0 # reset scores column; set to zero

   doc <- q_matrix[i,]
    #
if (convert_to_tm == "TRUE") {
    require("tm")
    matrix_words <- findFreqTerms(doc,lowfreq=termNum)
    }
else {
    require("quanteda")
    freqTerms <- topfeatures(doc, n = nfeature(doc)) #quanteda
    matrix_words <-  freqTerms[freqTerms >= termNum] # terms greater than x
}
    #print(words)

    for (word in matrix_words) {
	   #for (key in names(scores)) {
       for (key in lexi_cats_df[,1]) {
          etea <- lexicon[which(lexicon[,2]==key),]
    #print("etea")
    #print(etea[,1])
    #print(word)
       index <- pmatch(word,etea[,1],nomatch=0)
    
#print(index)
        if (index > 0) {
      print(lexicon[index,2])
      print("wasindex num:")
      print(index)
      
      #print("  ")
      
      category <- lexicon[index,2]
      print(category)
      #count <- counts[[category]]
      count <- lexi_cats_df[(lexi_cats_df[,1]==category),2] # for bayes algo
      #print("scccnt")
      #print(count)
      #print(lexi_cats_df[(lexi_cats_df[,2]==category),1])
      #print(count)
      #print(lexi_cats_df[(lexi_cats_df[,1]==category),2]) # the count
      score <- 1.0
      #print("scc")
      #print(count)

          if (algorithm=="bayes") score <- abs(log(score*prior/count))
      #print("sc")
      #print(score)
      #print("sc")
      if (verbose) {
        print(paste("WORD:",word,"CATEGORY:",category,"SCORE:",score))
      } # end of vrbse
      
      lexi_cats_df[(lexi_cats_df[,1]==category),3] <- lexi_cats_df[(lexi_cats_df[,1]==category),3]+score
    } #end of indx
  } # end of key
} # end of word

    
    if (algorithm=="bayes") {
      #for (key in names(scores)) {
      print("bayes")
      for (key in lexi_cats_df[,1]) {
      #count <- counts[[key]]
        count <- lexi_cats_df[(lexi_cats_df[,1]==key),2]
        #total <- counts[["total"]]
        #####################
        total <- lexi_cats_df[(lexi_cats_df$cat=="total"),2]
        score <- abs(log(count/total))
        #print("score")
        #print(score)
        if (!is.null(category)) {
			lexi_cats_df[(lexi_cats_df[,1]==category),3] <- lexi_cats_df[(lexi_cats_df[,1]==category),3]+score
		}
      }
    } else {
      #for (key in names(scores)) {
	for (key in lexi_cats_df[,1]) {
         if (!is.null(category)) {
			 lexi_cats_df[(lexi_cats_df[,1]==category),3] <- lexi_cats_df[(lexi_cats_df[,1]==category),3]+0.000001
		 }
      }
    }
    print("end for")
    #best_fit <- names(scores)[which.max(unlist(scores))]
    best_score <- max(lexi_cats_df[,3], na.rm = TRUE)
    best_score_index <- which.max(lexi_cats_df[,3] )
    best_fit <- lexi_cats_df[which.max(lexi_cats_df[,3] ),1]

    #if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    #documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))



tmp_df <- as.data.frame(t(lexi_cats_df[,-1])) 
colnames(tmp_df) <- lexi_cats_df[,1 ] 
print(tmp_df)
print(documents)
documents <- rbind(documents,tmp_df[2,] ) 
# need to zero lexi col afrt ethis

# replaces this mess below

#documents <- rbind(documents,c(scores$structure, scores$money, scores$food, scores$health, scores$welfare, scores$analysis, scores$application, scores$comprehension, scores$contemplate, scores$knowledge, scores$anger, scores$disgust, scores$fear, scores$joy, scores$sadness, scores$surprise, scores$communication, scores$creative, scores$data, scores$helping, scores$management, scores$organise, scores$research, scores$teach, scores$technical, scores$unlawful ))
  } # end of i
  
#  documents <- as.data.frame(documents)
#print(dim(documents))
#print(str(documents))
  # Fri pm colnames(documents) <- c("Structure", "Money", "Food", "Health", "Welfare", "Analysis", "Application", "Comprehension", "Contemplate", "Knowledge", "Anger", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Communication", "Creative", "Data", "Helping", "Management", "Organize", "Research", "Teach", "Technical", "Unlawful")

  return(documents)
}
#etea_dfm <- classify_etea(textColumn, algorithm="bayes",verbose=TRUE,use_stopwords=TRUE)
etea_dfm <- classify_etea(textColumn, algorithm=FALSE,verbose=TRUE,use_stopwords=TRUE)
etea_dfm
