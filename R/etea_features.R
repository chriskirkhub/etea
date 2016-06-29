#' Create a list of the Document Frequency Matrix according to minimum word frequency
#'
#' This function is a wrapper for a Document Frequency Matrix provided by the function \code{\link[quanteda]{create_q_matrix}} so that it can typically be used to add terms to a lexicon.  The terms to be added to a lexicon must first be categorised in a format that matches the example in the data directory.  Parameters listed below match those of the function \code{\link[quanteda]{create_q_matrix}}.
#'
#' @author Chris Kirk
#'
#' @return a word vector.
#'
#' @param textColumn character vector containing the text to be analysed; mandatory
#' @param stateNum numeric vector containing identifiers for the condition or state when the document or note was recorded/written that it be correctly allocated in the event of more than one note or record being in a state; default NULL
#' @param timeNum numeric verctor containing an index that identifies when the document was recorded/noted to give a temporal record.  This normalises progress and case note recording as a progress through a system.  Typically days or minutes after the system commenced; default NULL
#' @param docvaragg specifies how the aggregation on docvars is to occur either s stateID only, t timestamp only, st state and timestamp or timestamp and state ; default NULL; Options s, t, st, ts
#' @param use_stopwords specifies whether stopwords are to be removed from the corpus (TRUE) or not removed, (FALSE).  Users are reminded that system (language-specific) stopwords may need additions or removals to tailor for a specific need; default TRUE
#' @param choice of language to determine the content of the basic stopword list; default \code{english}. See \code{\link{quanteda}} for further information.
#' @param add_stopwords a character vector of words to be added to the stopwords vector (if any); default is NULL.
#' @param remove_stopwords a character vector of words to be removed to the stopwords vector (if any); default is NULL.
#' @param verbose to see useful progress information; default is TRUE
#' @param toLower to convert all inbound text into lower case. Notably this will degrade the sentence splitting function if applied; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param stem reduce word length to root; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param removeFeatures remove particular features from inbound text as specified in a list; default is TRUE; see: \code{\link{quanteda}}
#' @param language to define local language; default is "english" see: \code{\link{quanteda}}
#' @param valuetype to define patterning; default is \code{glob}; see: \code{\link{quanteda}}
#' @param removeNumbers remove individual numbers from inbound text, (note: numbers already aggregated with characters such as 1st or 2nd are unaffected); default is TRUE; see: \code{\link{quanteda}}
#' @param removePunct remove punctuation from inbound text; default is TRUE; see: \code{\link{quanteda}}
#' @param removeSeparators remove separators from inbound text; default is TRUE; see: \code{\link{quanteda}}
#' @param removeHyphens remove hyphen characters from inbound text; default is TRUE; see: \code{\link{quanteda}}
#' @param removeTwitter remove twitter api characters from inbound text; default is TRUE; see: \code{\link{quanteda}}
#' @param ngrams integer vector specifying the number of elements to be concatenated in each ngram; default is 1L; see: \code{\link[quanteda]{ngrams}}
#' @param skip integer vector specifying the adjacency skip size for tokens forming the ngrams; \code{0}: see: \code{\link[quanteda]{ngrams}}
#' @param concatenator character for combining words, default is \code{_}; see: \code{\link[quanteda]{ngrams}}
#' @param simplify character vector of tokens rather than a length of texts; default is FALSE; see: \code{\link[quanteda]{tokenize}}
#' @param convert_to_tm logical specifying the requirement for the matrix to be returned in the tm TRUE or quanteda FALSE format
#' @param termNum integer specifying the minimum frequency a word is to have been found in the matrix
#' @param ... Extra arguments, not used
#'
#' @examples
#' ## LOAD ##
#' text_df <- read.csv("data/militant_suffragette_extract.csv",header=TRUE, sep=";")
#' textColumn<-as.character(text_df$Notes) # typically textual interview or clinical notes
#' ## CREATE FEATURES LIST ##
#' features_vec <- etea_features(textColumn, termNum=1,verbose=TRUE,use_stopwords=TRUE)
#' features_vec
#'
#' @rdname etea_features
#'
#' @export
#'
etea_features <- function(textColumn, stateNum=NULL, timeNum=NULL, docvaragg = "null", use_stopwords=TRUE, stopwords_language="english", add_stopwords=NULL, remove_stopwords=NULL, verbose=TRUE, toLower=FALSE, stem=FALSE, keptFeatures=NULL,  removeFeatures=TRUE, language="english", valuetype = c("glob"), thesaurus=NULL, dictionary=NULL,  removeNumbers=TRUE, removePunct=TRUE, removeSeparators=TRUE, removeHyphens=TRUE, removeTwitter=TRUE, ngrams=1L, skip=0L, concatenator="_", simplify=FALSE, convert_to_tm=TRUE, termNum=1, ...)
{
 
seed.val <- 2
set.seed(seed.val)

q_matrix <- create_q_matrix(textColumn=textColumn, stateNum=stateNum, timeNum=timeNum, docvaragg=docvaragg, use_stopwords=use_stopwords, stopwords_language=stopwords_language, add_stopwords=add_stopwords, remove_stopwords=remove_stopwords, verbose=verbose, toLower=toLower, stem=stem, keptFeatures=keptFeatures, removeFeatures=removeFeatures, language=language, valuetype=valuetype, thesaurus=thesaurus, dictionary=dictionary, removeNumbers=removeNumbers, removePunct=removePunct, removeSeparators=removeSeparators, removeHyphens=removeHyphens, removeTwitter=removeTwitter, ngrams=ngrams, skip=skip, concatenator=concatenator, simplify=simplify, convert_to_tm=convert_to_tm, ...)

if (convert_to_tm == "TRUE") {
      if (!("tm" %in% installed.packages()[, "Package"]))
        stop("You must install the tm package installed for this conversion.")
    	matrix_words <- tm::findFreqTerms(q_matrix,lowfreq=termNum)
    }
else {
      if (!("quanteda" %in% installed.packages()[, "Package"]))
        stop("You must install the quanteda package installed for this conversion.")
      freqTerms <- quanteda::topfeatures(q_matrix, n = nfeature(q_matrix)) #quanteda
      matrix_words <-  names(freqTerms[freqTerms >= termNum]) # terms greater than x
}
}

