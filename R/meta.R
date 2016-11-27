#' @export
load.meta <- function(ids = c("rksp.0"), url="http://localhost:8080/drama.web") {
  meta <- read.csv(paste(url, "all", sep="/"))
  rownames(meta) <- meta$id;
  figures <- load.annotations(ids, 
                              type="de.unistuttgart.ims.drama.api.DramatisPersonae", 
                              coveredType="de.unistuttgart.ims.drama.api.Figure",
                              url=url);
  drama2figures <- data.frame(tapply(figures$Figure.surface, figures$drama, length));
  colnames(drama2figures) <- c("Figures (in Dramatis Personae)");
  tokens <- load.text(ids, tokens=TRUE,url=url);
  drama2tokens <- data.frame(tapply(tokens$Token.surface, tokens$drama, FUN=length));
  colnames(drama2tokens) <- c("Tokens (in speech)");
  m <- merge(meta, drama2figures, by=0)[,-1]
  rownames(m) <- m$id
  merge(m, drama2tokens, by=0)[,-1]
  
}