#' @include create_q_matrix.R
#'
#' Classify documents or textual notes according to categories appearing in a lexicon, typically in groups of categories, namely, (e)nvironment, (t)hought, (e)motion and (a)ction, (etea).
#'
#' In state-space and decision modelling of textual data it is considered important to classify documents by interval group or categories so that the continuous sentiment domain can be disctretised for sub-analysis.  This function takes a document frequency matrix either from \code{\link{quanteda}} functions or from \code{\link{tm}} functions, typically from the associated function \code{\link[quanteda]{create_q_matrix}} (part of this package) and quantifies each word occurence by matching it against a lexicon of words with associated categories that the user has supplied.  The lexicon/category listing, (csv format with sep = ";") is expected to be found in the data directory associated with the calling function.  This lexicon can contain any number of words categorised by any number of categories to suit user interest.  The example lexicon (herein) contains a small set of words that have been successfully used to conduct proof-of-concept studies investigating crime and similar issues.  The main user changes to the lexicon are expected to extend the 'actions' group such as by changing 'special interest' to any areas of special interest.  The user can run the function \code{\link[etea]{etea_features}} to examine a list of words in the documents or notes being studied.  This \code{\link[etea]{etea_features}} is simply a wrapper to \code{\link{quanteda}} and \code{\link{tm}} functions but is a useful 'helper' function for those not wholly familiar with those packages.  The function arguments are the same as the \code{\link[etea]{create_q_matrix}} function from which it is derived so that users can consider the features found in the documents or notes, segment into categories and easily append them to the lexicon.  It is thus possible that a series of lexicons can be developed to examine different issues.  This function has been designed so that the output is in a form that can easily be leveraged by predictive algorithms that use segmented time-series data.  It is expected that these functions will be of interest to those studying sentiment change over time including inter-alia, neural networks, decision systems and state-space modelling.  The underlying methodology for this function has been extended and developed with permission, from an original by Tim Jurka \code{\link{sentiment}{classify_emotions}}.
#'
#' @author Chris Kirk
#'
#' @return a scored, classified matrix of document/note words as categories to provide input into other analytical systems.
#'
#' @param textColumn character vector containing the text to be analysed; mandatory
#' @param stateNum numeric vector containing identifiers for the condition or state when the document or note was recorded/written that it be correctly allocated in the event of more than one note or record being in a state; default NULL
#' @param timeNum numeric vector containing an index that identifies when the document was recorded/noted to give a temporal record.  This normalises progress and case note recording as a progress through a system.  Typically days or minutes after the system commenced; default NULL
#' @param docvaragg specifies how the aggregation on docvars is to occur either 'state' stateID only, 'time' timestamp only, 'statetime' state and timestamp or timestamp and state ; default NULL; Options state, time, statetime, timestate
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
#' @param prior integer specifying the prior bayesian weighting value
#' @param ... Extra arguments, not used
#'
#' @examples
#' # create a scored, classified matrix of CaseNotes and aggregated by state for use in further modelling
#' ## LOAD ##
#' text_df <- read.csv("data/jtr_docs.csv",header=TRUE, sep=";")
#' textColumn<-as.character(text_df$Notes)
#' ## CLASSIFY ##
#' classify_eta(textColumn, stateNum=c(1,1,2,3,3), verbose=TRUE, use_stopwords=TRUE, docvaragg="state")
#'
#' # create dfm using character vector of CaseNotes, states and datetimestamps for use as a time series for a neural network or MARSS
#' ## LOAD ##
#' text_df <- read.csv("data/militant_suffragette_extract.csv",header=TRUE, sep=";")
#' textColumn<-as.character(text_df$Notes) # typically textual interview or clinical notes
#' statecol<-as.numeric(text_df$stateNum) # typically identication of parts of journey/episode
#' timecol<-as.character(text_df$timeNum) # typically days since start of journey/episode
#' ## CLASSIFY ##
#' etea_df_time <- classify_etea(textColumn, statecol, timecol, verbose=TRUE, use_stopwords=TRUE, docvaragg="time")
#' # for MARSS
#' ## CONVERT FOR MARSS ##
#' etea_matrix <- data.matrix(etea_df_time) # MARSS requires standard data matrix Note timeNum as rownames
#' dat = t(etea_matrix) # transpose to MARSS form
#' colnames(dat) <- rownames(etea_matrix) # set column names to timeNum from docvars (rownames)
#' ## dat is now available as MARSS DATA ##
#'
#' @rdname classify_etea
#' @title classifier-leverage function to classify text into groups of categories
#'
#' @export
#'
classify_etea <- function(textColumn, stateNum=NULL, timeNum=NULL, docvaragg = "null", use_stopwords=TRUE, stopwords_language="english", add_stopwords=NULL, remove_stopwords=NULL, verbose=TRUE, toLower=FALSE, stem=FALSE, keptFeatures=NULL,  removeFeatures=TRUE, language="english", valuetype = c("glob"), thesaurus=NULL, dictionary=NULL,  removeNumbers=TRUE, removePunct=TRUE, removeSeparators=TRUE, removeHyphens=TRUE, removeTwitter=TRUE, ngrams=1L, skip=0L, concatenator="_", simplify=FALSE, useSentences=TRUE, convert_to_tm=TRUE,  pstrong=1.0, pweak=0.5, termNum=1,...)
{
  
  seed.val <- 2
  set.seed(seed.val)
  
  q_matrix <- create_q_matrix(textColumn=textColumn, stateNum=stateNum, timeNum=timeNum, docvaragg=docvaragg, use_stopwords=use_stopwords, stopwords_language=stopwords_language, add_stopwords=add_stopwords, remove_stopwords=remove_stopwords, verbose=verbose, toLower=toLower, stem=stem, keptFeatures=keptFeatures, removeFeatures=removeFeatures, language=language, valuetype=valuetype, thesaurus=thesaurus, dictionary=dictionary, removeNumbers=removeNumbers, removePunct=removePunct, removeSeparators=removeSeparators, removeHyphens=removeHyphens, removeTwitter=removeTwitter, ngrams=ngrams, skip=skip, concatenator=concatenator, simplify=simplify, useSentences=useSentences, convert_to_tm=convert_to_tm, ...)
  
  lexicon <- read.csv("data/etea_categories.csv",header=TRUE, sep=";", stringsAsFactors=FALSE)
  subj_lexicon <- read.csv("data/subjectivity.csv",header=TRUE, sep=";", stringsAsFactors=FALSE)

  
  lexi_cats_df<-as.data.frame(aggregate(lexicon[,1] ~ lexicon[,2], lexicon, function(x) length(unique(x))))
  subj_lexi_cats_df<-as.data.frame(aggregate(subj_lexicon[,1] ~ subj_lexicon[,3], subj_lexicon, function(x) length(unique(x))))
  documents <- data.frame(matrix(vector(), nrow=0, ncol=(3*(nrow(lexi_cats_df))))) # df to maintain scores + 1 col for totals
  
  colnames(lexi_cats_df)[1]<-c("cat") # name categories column
  colnames(lexi_cats_df)[2]<-c("catcount") # name counts column
  colnames(subj_lexi_cats_df)[1]<-c("subj_cat") # name categories column
  colnames(subj_lexi_cats_df)[2]<-c("subj_catcount") # name counts column
  lexi_cats_df[,3]<-0 # create scores column and set to zero
  subj_lexi_cats_df[,3]<-0 # create scores column and set to zero
  colnames(lexi_cats_df)[3]<-c("score") # name column
  colnames(subj_lexi_cats_df)[3]<-c("subj_score") # name column
  
  lexi_cats_df<-rbind(lexi_cats_df, total=sum(lexi_cats_df$catcount)) # total counts
  subj_lexi_cats_df<-rbind(subj_lexi_cats_df, total=sum(subj_lexi_cats_df$subj_catcount)) # total counts
  lexi_cats_df[nrow(lexi_cats_df),3]<-0 # reset totals value
  subj_lexi_cats_df[nrow(subj_lexi_cats_df),3]<-0 # reset totals value
  lexi_cats_df[nrow(lexi_cats_df),1]<-"total" # apply total name
  subj_lexi_cats_df[nrow(subj_lexi_cats_df),1]<-"subj_total" # apply total name
  
  pos_doc_vec <- paste("pos", lexi_cats_df[1:(nrow(lexi_cats_df)-1),1 ], sep = "_")
  neg_doc_vec <- paste("neg", lexi_cats_df[1:(nrow(lexi_cats_df)-1),1 ], sep = "_")
  diff_doc_vec <- lexi_cats_df[1:(nrow(lexi_cats_df)-1),1 ]
  colnames(documents) <- c(pos_doc_vec, neg_doc_vec, diff_doc_vec)
  lexi_cats_df[,4]<-0 #
  lexi_cats_df[,5]<-0 #
  colnames(lexi_cats_df)[4]<-c("pos_score") # name column
  colnames(lexi_cats_df)[5]<-c("neg_score") # name column
  
  lexi_cats_df[,6]<-0 #
  lexi_cats_df[,7]<-0 #
  colnames(lexi_cats_df)[6]<-c("pos_valence") # name column
  colnames(lexi_cats_df)[7]<-c("neg_valence") # name column
  
  subj_missing <- ("Missing words")
  etea_missing <- ("Missing words")
  
  for (i in 1:nrow(q_matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    lexi_cats_df[,3]<-0 # reset scores column; set to zero
    lexi_cats_df[,4]<-0
    lexi_cats_df[,5]<-0
    
    doc <- q_matrix[i,]
    
    if (convert_to_tm == "TRUE") {
      if (!("tm" %in% installed.packages()[, "Package"]))
        stop("You must install the tm package installed for this conversion.")
      
      matrix_words <- findFreqTerms(doc,lowfreq=termNum,highfreq = Inf)
      
    }
    else {
      if (!("quanteda" %in% installed.packages()[, "Package"]))
        stop("You must install the quanteda package installed for this conversion.")
      freqTerms <- quanteda::topfeatures(doc, n = nfeature(doc)) #quanteda
      matrix_words <-  names(freqTerms[freqTerms >= termNum]) # terms greater than x
    }

    for (word in matrix_words) {

      for (key in lexi_cats_df[,1]) {
        etea <- lexicon[which(lexicon[,2]==key),]

        index <- charmatch(word,etea[,1],nomatch=0)

        if (index > 0) {

          category <- etea[index,2]

          score <- 1.0
          
          if (verbose) {
          } # end of verbose
          
          lexi_cats_df[(lexi_cats_df[,1]==category),3] <- lexi_cats_df[(lexi_cats_df[,1]==category),3]+score

          ####### subjectivity starts #######################
          for (subj_key in subj_lexi_cats_df[,1]) {

            if ((subj_key == "both")||(subj_key == "subj_total")) next
            
            subj_etea <- subj_lexicon[which(subj_lexicon[,3]==subj_key),]
            #subj_index <- pmatch(word,subj_etea[,1],nomatch=0)
            subj_index <- charmatch(word,subj_etea[,1],nomatch=0)
            
            if (subj_index > 0) {

              subj_strength <- subj_etea[subj_index,2] # is strongsubj or weaksubj
              subj_category <- subj_etea[subj_index,3] # categories here are only either positive or negative

              subj_score <- pweak # a base score
              if (subj_strength == "strongsubj") subj_score <- pstrong # enhance the score
              # note it will still be a positive score value just associated with negative for a negative score
              
              if (verbose) { #
              } # end of verbose
        
              if (subj_category=="positive") {

                lexi_cats_df[(lexi_cats_df[,1]==category),4] <- lexi_cats_df[(lexi_cats_df[,1]==category),4]+subj_score
              }
              else { # it's negative

                lexi_cats_df[(lexi_cats_df[,1]==category),5] <- lexi_cats_df[(lexi_cats_df[,1]==category),5]+subj_score
              }
              subj_score=0
            } #end of subj_index
          } # end of sub_key
          ####### subjectivity ends #######################
          
          ###### resets ####################
          
          score=0 # reset score
        } #end of index
      } # end of key
      

    } # end of word
    
      for (key in lexi_cats_df[,1]) {

        lexi_cats_df[(lexi_cats_df[,1]==key),3] <- lexi_cats_df[(lexi_cats_df[,1]==key),3]+0.000001
        for (subj_key in subj_lexi_cats_df[,1]) {
          if (subj_key=="positive") {
            lexi_cats_df[(lexi_cats_df[,1]==key),4] <- lexi_cats_df[(lexi_cats_df[,1]==key),4]+0.000001
          }
          else { # it's negative
            lexi_cats_df[(lexi_cats_df[,1]==key),5] <- lexi_cats_df[(lexi_cats_df[,1]==key),5]+0.000001
          }
        }
      }
      
    best_score <- max(lexi_cats_df[,3], na.rm = TRUE)
    best_score_index <- which.max(lexi_cats_df[,3] )
    best_fit <- lexi_cats_df[which.max(lexi_cats_df[,3] ),1]

    # assign polarity as a proportion of score
    for (key in lexi_cats_df[,1]) { #get the category from col 1

      positive_allocation <- lexi_cats_df[(lexi_cats_df[,1]==key),4]
      negative_allocation <- lexi_cats_df[(lexi_cats_df[,1]==key),5]
      total_allocation <- positive_allocation + negative_allocation
      
      lexi_cats_df[(lexi_cats_df[,1]==key),6] <- (positive_allocation / total_allocation) * lexi_cats_df[(lexi_cats_df[,1]==key),3]
      lexi_cats_df[(lexi_cats_df[,1]==key),7] <- (negative_allocation / total_allocation) * -1 * lexi_cats_df[(lexi_cats_df[,1]==key),3]
      
    }
    
    tmp_df <- as.data.frame(t(lexi_cats_df[1:(nrow(lexi_cats_df)-1),-1])) #transpose exclude first column and totals
    
    tmp_df <- as.data.frame(tmp_df[2,]) # retain only the score row
    
    colnames(tmp_df) <- lexi_cats_df[1:(nrow(lexi_cats_df)-1),1 ]
    
    pos_valence_vec <- lexi_cats_df[1:(nrow(lexi_cats_df)-1),6 ]
    neg_valence_vec <- lexi_cats_df[1:(nrow(lexi_cats_df)-1),7 ]
    diff_valence_vec <- lexi_cats_df[1:(nrow(lexi_cats_df)-1),6 ] + lexi_cats_df[1:(nrow(lexi_cats_df)-1),7 ]
    all_valence_vec <- c(pos_valence_vec, neg_valence_vec, diff_valence_vec)
    
    documents[nrow(documents)+1, ] <- all_valence_vec

  } # end of i
  
  # return the scored documents
  return(documents)
}

