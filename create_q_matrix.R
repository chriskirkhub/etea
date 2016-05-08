#' Elaborate a Document Frequency Matrix in either quanteda or tm format.
#' 
#' In text investigations it is considered important to consider the use of skip-grams to increase coverage without unduly increasing training size.  In multi-state and decision modelling generally, it is considered important to aggregate documents by state IDs and to return a time series.  This includes use in packages such as neural networks, decision models and in ensemble packages such as \code{\link[RTextTools]}.  This function is a wrapper for the \code{\link[quanteda]} package that elaborates a Document Frequency Matrix and returns it in a \code{\link[tm]} package format for use by other functions.  The function receives arguments that leverage functions within the quanteda package namely docvars, ngrams and skipgrams.  This function is independent but also acts as a feeder to \code{\link[etea]{classify_etea}} and is a wrapper developed with permission, of functions from originals by Ken Benoit and Paul Nulty et al \code{\link[quanteda]}.
#'
#' @author Chris Kirk
#'
#' @return a tm-type Document Term sparse Matrix object containing word frequencies with (optionally) a time-series index and a state ID identifier as a data frame.
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
#' @param ... Extra arguments, not used
#'
#' @examples
#' # create dfm using character vector of CaseNotes and aggregated by state for use in a classifier
#' ## LOAD ##
#' text_df <- read.csv("data/jtr_docs.csv",header=TRUE)
#' textColumn<-as.character(text_df$Notes)
#' ## CREATE MATRIX ##
#' create_q_matrix(textColumn, stateNum=c(1,1,2,3,3), verbose=TRUE, use_stopwords=TRUE, docvaragg="s")
#'
#' # create dfm using character vector of CaseNotes, states and datetimestamps for use as a time series for nnet or MARSS
#' ## LOAD ##
#' text_df <- read.csv("data/militant_suffragette_extract.csv",header=TRUE)
#' textColumn<-as.character(text_df$Notes) # typically textual interview or clinical notes
#' stateNum<-as.numeric(text_df$stateID) # typically identication of parts of journey/episode
#' timeNum<-as.character(text_df$datetimestamp) # typically days since start of journey/episode
#' ## CREATE MATRIX ##
#' q_tm_dfm <- create_q_matrix(textColumn, stateNum, timeNum, verbose=TRUE, use_stopwords=TRUE, docvaragg="t")
#' # for MARSS
#' q_matrix <- data.matrix(q_tm_dfm) # MARSS requires standard data matrix Note timeNum as rownames
#' ## CONVERT FOR MARSS ##
#' dat = t(q_matrix) # transpose to MARSS form
#' colnames(dat) <- rownames(q_matrix) # set column names to timeNum from docvars (rownames)
#' ## dat is now available as MARSS DATA ##
#' @references 
#' \href{http://homepages.inf.ed.ac.uk/ballison/pdf/lrec_skipgrams.pdf}{Guthrie,
#' D., B. Allison, W. Liu, and L. Guthrie. 2006. "A Closer Look at Skip-Gram 
#' Modelling."}
#'
#' @rdname create_q_matrix
#'
#' @export
#'
create_q_matrix <- function (textColumn, stateNum=NULL, timeNum=NULL, docvaragg = c("null", "s", "t", "st", "ts"), use_stopwords=TRUE, stopwords_language="english", add_stopwords=NULL, remove_stopwords=NULL, verbose=TRUE, toLower=FALSE, stem=FALSE, removeFeatures=TRUE, language="english", valuetype = c("glob"), removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, removeTwitter = FALSE, ngrams=1L, skip=0L, concatenator="_", simplify=FALSE, convert_to_tm = TRUE, ...) {

seed.val <- 2
set.seed(seed.val)
  
if (is.null(textColumn)) stop("Must supply a character vector of messages, notes or documents")
        txt_vec <- as.character(textColumn)
    
  if (use_stopwords=="TRUE") {
    
    stopwords_language=stopwords_language
    stopwords <- stopwords(stopwords_language)
    if(!is.null(add_stopwords)) {

      add_stopwords <- as.character(add_stopwords)
      
      stopwords <- c(stopwords, add_stopwords)
      
    }
    if(!is.null(remove_stopwords)) {
      remove_stopwords <- as.character(remove_stopwords)
      stopwords <- setdiff(stopwords, remove_stopwords)
    }

  }
  
  if(toLower=="TRUE") {
    txt_vec <- toLower(txt_vec, keepAcronyms = TRUE)
  }
  
  if (stem=="TRUE") {
    txt_vec <- wordstem(txt_vec, language=language)
  }
  
  eteaCorpus <- corpus(txt_vec)

  # add document level variables such a stateNum or datetimestamp tag  
  if (!is.null(stateNum)) {
  docvars(eteaCorpus, "stateNum") <- as.numeric(as.character(stateNum))
  }

  if (!is.null(timeNum)) {  
  docvars(eteaCorpus, "timeNum") <- as.numeric(as.character(timeNum))
  }

  docvaragg <- match.arg(docvaragg) 

  if ((!is.null(stateNum)) || (!is.null(timeNum))) {
      
      if (docvaragg=="s") {

	if (is.null(stateNum)) stop("Must supply a numeric vector of state IDs")
	  if (use_stopwords=="TRUE") {
	    q_matrix<-removeFeatures(dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("stateNum"),...), stopwords)   
	} else {
    q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("stateNum"),...)
	}
      }
      else if (docvaragg=="t") {
	if (is.null(timeNum)) stop("Must supply a numeric vector of timestamps (ie days lapsed counters)")
	  if (use_stopwords=="TRUE") {
	    q_matrix<-removeFeatures(dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("timeNum"),...), stopwords)   
	} else {
    q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("timeNum"),...)
	}
	}
      else if (docvaragg=="st") {
	if ((is.null(timeNum)) || (is.null(stateNum)) ) stop("Numeric vector of state IDs or timestamps (ie days lapsed counters) missing")
	  if (use_stopwords=="TRUE") {
	    q_matrix<-removeFeatures(dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("stateNum", "timeNum"),...), stopwords)   
	} else {
    q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("stateNum", "timeNum"),...)
	}	
	}
      else if (docvaragg=="ts") {
	if ((is.null(timeNum)) || (is.null(stateNum)) ) stop("Numeric vector of state IDs or timestamps (ie days lapsed counters) missing")
	  if (use_stopwords=="TRUE") {
	    q_matrix<-removeFeatures(dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("timeNum", "stateNum"),...), stopwords)   
	} else {
    q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator, groups=c("timeNum", "stateNum"),...)
	}	
	}
      else {
            q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator,...)
	}
}

  if ((is.null(stateNum)) & (is.null(timeNum))) {
        q_matrix<-dfm(eteaCorpus, removePunct=removePunct, removeSeparators=removeSeparators, removeNumbers=removeNumbers, removeHyphens=removeHyphens, removeTwitter=removeTwitter, simplify=simplify, ngrams=ngrams, skip=skip, concatenator=concatenator,...)
	}

if (convert_to_tm=="TRUE") {
  tmdfm <- convert(q_matrix, to="tm")
}
  gc()

  return(tmdfm)  
}

