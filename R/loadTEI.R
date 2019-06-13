#' @title Load drama
#' @description This function parses and loads one or more dramas in raw TEI format.
#' @param filename The filename of the drama to load (or a list thereof).
#' @param dataDirectory The directory that holds the file(s).
#' @import xml2
#' @importFrom data.table data.table
#' @exportClass QDDrama
#' @export
loadDramaTEI <- function(filename, dataDirectory=paste0(getOption("qd.datadir"), "/tei/")) {
  if (is.list(filename)) {
    drama_list <- lapply(filename, loadDramaTEI)
    drama <- drama_list[[1]]
    for (i in 2:length(drama_list)) {
      drama <- combine(drama, drama_list[[i]])
    }
  } else {
    raw_tei <- xml2::read_xml(paste0(dataDirectory, filename))
    nsp <- xml2::xml_ns_rename(xml2::xml_ns(raw_tei), d1="tei")
    # dracorid:
    # id <- gsub("ger", "", xml2::xml_text(xml2::xml_find_first(raw_tei, "//tei:publicationStmt/tei:idno[@type='dracor']", ns=nsp)))
    id <- gsub("([^.]+).xml", "\\1", filename)
    corpus <- "ger"
    
    drama <- list()
    text_segments <- parseTEI(raw_tei, nsp, id, corpus)
    drama$text <- text_segments$text
    drama$segments <- text_segments$segments
    drama$characters <- loadCharactersTEI(raw_tei, nsp, drama$text)
    drama$meta <- loadMetaTEI(raw_tei, nsp, corpus, id)
    drama$mentions <- loadMentionsTEI()
    
    class(drama$text) <- append("QDHasUtteranceBE", class(drama$text))
    class(drama$segments) <- append("QDHasSegments", class(drama$segments))
    class(drama$mentions) <- append("QDHasUtteranceBE", class(drama$mentions))
    
    class(drama) <- append("QDDrama", class(drama))
  }
  drama
}

#' @importFrom tokenizers tokenize_words
parseTEI <- function(raw_tei, nsp, id, corpus) {
  text_l <- list()
  segments_l <- list()
  
  d_length <- 0
  act_counter <- 0
  scene_counter <- 0
  position <- 1
  relevant_tei <- xml2::xml_find_all(raw_tei, 
                                     "//tei:div[@type='act']|
                                      //tei:div[@type='scene']|
                                      //tei:sp|
                                      //tei:div/tei:stage", 
                                     ns=nsp)
  
  for (elem in relevant_tei) {
    if (identical(xml2::xml_attr(elem, "type", ns = nsp), "act")) { # new act
      if (act_counter > 0 && scene_counter == 0) { # if there are no scenes
        segments_l <- writeSegmentRow(corpus, id, act_begin, 
                                      act_counter, 0, 0, scene_counter, segments_l)
      }
      act_begin <- position
      act_counter <- act_counter + 1
      scene_counter <- 0
    } else if (identical(xml2::xml_attr(elem, "type", ns = nsp), "scene")) { # new scene
      scene_end <- position - 1
      if (scene_counter > 0) {
        segments_l <- writeSegmentRow(corpus, id, act_begin, act_counter, 
                                      scene_begin, scene_end, scene_counter, segments_l)
      }
      scene_counter <- scene_counter + 1
      scene_begin <- position
    } else if (identical(xml2::xml_name(elem, ns = nsp), "tei:stage")) { # stage-direction
      tokenized_st <- tokenizers::tokenize_words(xml2::xml_text(elem), lowercase = FALSE, strip_punct = FALSE)[[1]]
      #d_length <- d_length + length(tokenized_st)
      # TODO: STAGE
      position <- position + nchar(xml2::xml_text(elem))
    } else if (identical(xml2::xml_name(elem, ns = nsp), "tei:sp")) { # speaker & speach
      raw_ids <- strsplit(xml2::xml_attr(elem, "who", ns = nsp), "#")[[1]]
      speaker_ids <- sapply(raw_ids, trimws)[2:length(raw_ids)]
      for (sp_elem in xml2::xml_children(elem)) {
        if (identical(xml2::xml_name(sp_elem, ns = nsp), "tei:speaker")) {
          speaker_surface <- gsub("\\.", "", xml2::xml_text(sp_elem))
          d_length <- d_length + length(tokenizers::tokenize_words(speaker_surface, lowercase = FALSE, strip_punct = FALSE)[[1]])
        } else if (identical(xml2::xml_name(sp_elem, ns = nsp), "tei:p")) {
          for (p_elem in xml2::xml_contents(sp_elem)) {
            if (identical(xml2::xml_name(p_elem, ns = nsp), "tei:stage")) {
              tokenized_st <- tokenizers::tokenize_words(xml2::xml_text(elem), lowercase = FALSE, strip_punct = FALSE)[[1]]
              #d_length <- d_length + length(tokenized_st)
              # TODO: STAGE
            } else {
              tokenized_p <- tokenizers::tokenize_words(xml2::xml_text(p_elem), lowercase = FALSE, strip_punct = FALSE)[[1]]
              text_l <- writeTextRow(corpus, id, tokenized_p, speaker_ids, text_l, position, sp_elem, speaker_surface)
              d_length <- d_length + length(tokenized_p)
            }
          }
          position <- position + nchar(xml2::xml_text(sp_elem))
        } else if (identical(xml2::xml_name(sp_elem, ns = nsp), "tei:lg")) {
          temp_text <- (paste(lapply(xml2::xml_children(sp_elem), xml2::xml_text)))
          tokenized_lg <- tokenizers::tokenize_words(temp_text, lowercase = FALSE, strip_punct = FALSE)[[1]]
          text_l <- writeTextRow(corpus, id, tokenized_lg, speaker_ids, text_l, position, sp_elem, speaker_surface)
          d_length <- d_length + length(tokenized_lg)
          position <- position + nchar(xml2::xml_text(sp_elem))
        } else if (identical(xml2::xml_name(sp_elem, ns = nsp), "tei:stage")) {
          tokenized_st <- tokenizers::tokenize_words(xml2::xml_text(elem), lowercase = FALSE, strip_punct = FALSE)[[1]]
          #d_length <- d_length + length(tokenized_st)
          # TODO: STAGE
          position <- position + nchar(xml2::xml_text(sp_elem))
        }
      }
    }
  }
  
  # text table
  dt_text <- data.table::data.table(as.data.frame(do.call(rbind, text_l)))
  names(dt_text) <- c("corpus", "drama", "utteranceBegin", "utteranceEnd", 
                      "Speaker.figure_surface", "Speaker.figure_id", "Token.surface")
  dt_text$Token.pos <- NA
  dt_text$Token.lemma <- NA
  dt_text$length <- d_length
  
  dt_text <- fixColumnType(dt_text)
  
  dt_text$Speaker.figure_surface <- as.factor(dt_text$Speaker.figure_surface)
  dt_text$Speaker.figure_id <- as.factor(dt_text$Speaker.figure_id)
  
  # segments table
  dt_segments <- data.table::data.table(as.data.frame(do.call(rbind, segments_l)))
  names(dt_segments) <- c("corpus", "drama", "begin.Act", "end.Act", "Number.Act",
                          "begin.Scene", "end.Scene", "Number.Scene")
  act_begins <- unique(dt_segments$begin.Act)[-1]
  for (i in 1:length(act_begins)) {
    dt_segments$end.Act[dt_segments$Number.Act == i] <- act_begins[[i]] - 1
  }
  dt_segments$end.Act[is.na(dt_segments$end.Act)] <- position - 1
  
  dt_segments <- fixColumnType(dt_segments)
  
  list("text" = dt_text, "segments" = dt_segments)
}

# internal
writeTextRow <- function(corpus, id, tokenized, speakers, text_l, position, sp_elem, speaker_surface) {
  for (token in tokenized) {
    for (speaker in speakers) {
      text_l <- append(text_l, list(list(corpus,
                                         id,
                                         position,
                                         position + nchar(xml2::xml_text(sp_elem)) - 1,
                                         speaker_surface,
                                         speaker,
                                         token)))
    }
  }
  text_l
}

# internal
writeSegmentRow <- function(corpus, id, act_begin, act_counter, 
                            scene_begin, scene_end, scene_counter, segments_l) {
  segments_l <- append(segments_l, list(list(corpus,
                                             id,
                                             act_begin,
                                             NA, # act end
                                             act_counter,
                                             scene_begin,
                                             scene_end,
                                             scene_counter)))
}

# internal
fixColumnType <- function(dt) {
  dt[] <- lapply(dt, function(column) {
    if (is.character(column[[1]])) {
      as.character(column)
    } else if (is.numeric(column[[1]])){
      as.numeric(column)
    } else {
      column
    }
  })
  dt
}

# internal
loadCharactersTEI <- function(raw_tei, nsp, dt_text) {
  `:=` <- NULL
  Speaker.figure_id <- NULL
  Gender <- NULL
  
  dt_characters <- data.table::copy(dt_text)
  dt_characters[, c("utteranceBegin","utteranceEnd", "Token.surface", "Token.pos",
                    "Token.lemma", "length"):=NULL] 
  dt_characters <- unique(dt_characters)
  dt_characters$Gender <- character()
  listPerson <- xml2::xml_find_all(raw_tei, "//tei:listPerson/tei:person", ns = nsp)
  for (elem in listPerson) {
    dt_characters[Speaker.figure_id == xml2::xml_attr(elem, "id"), Gender := xml2::xml_attr(elem, "sex")]
  }
  dt_characters$Age <- numeric()
  dt_characters
}

# internal
loadMetaTEI <- function (raw_tei, nsp, corpus, id) {
  title <- xml2::xml_text(xml2::xml_find_first(raw_tei, "//tei:titleStmt/tei:title[@type='main']", ns = nsp))
  language <- xml2::xml_attr(raw_tei, "lang")
  author <- xml2::xml_text(xml2::xml_find_first(raw_tei, "//tei:titleStmt/tei:author", ns = nsp))
  pnd <- gsub("pnd:", "", xml2::xml_attr(xml2::xml_find_first(raw_tei, "//tei:titleStmt/tei:author", ns = nsp), "key"))
  date_written <- fixDate(xml2::xml_attr(xml2::xml_find_first(raw_tei, "//tei:bibl[@type='originalSource']/tei:date[@type='written']", ns = nsp), "when"))
  date_printed <- fixDate(xml2::xml_attr(xml2::xml_find_first(raw_tei, "//tei:bibl[@type='originalSource']/tei:date[@type='print']", ns = nsp), "when"))
  date_premiere <- fixDate(xml2::xml_attr(xml2::xml_find_first(raw_tei, "//tei:bibl[@type='originalSource']/tei:date[@type='premiere']", ns = nsp), "when"))
  date_translated <- NA
  dt_meta <- data.table::data.table(corpus = corpus, drama = id, documentTitle = title, 
                        language = language, Name = author, Pnd = pnd, Translator.Name = NA, 
                        Translator.Pnd = NA, Date.Written = date_written, Date.Printed = date_printed, 
                        Date.Premiere = date_premiere, Date.Translation = NA)
  dt_meta
}

# internal 
fixDate <- function(date) {
  if (is.na(date)) {
    NA
  } else {
    as.numeric(date)
  }
}

# internal
loadMentionsTEI <- function() {
  dt_mentions <- data.table::data.table(corpus=character(), drama=character(), utteranceBegin=numeric(),
                            utteranceEnd=numeric(), utteranceSpeakerId=character(), 
                            mentionBegin=numeric(), mentionEnd=numeric(), 
                            mentionSurface=character(), entityId=character())
}

