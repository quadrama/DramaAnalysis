require(DramaAnalysis)

DramaAnalysis::setup()

rksp.0 <- list(mtext=loadSegmentedText("tg:rksp.0"))
levels(rksp.0$mtext$Speaker.figure_surface) <- c("Angelo", "Appiani", "Battista", "Camillo", "Claudia G.", "Claudia G.", "Conti", "Kammerdiener", "Prinz", "Emilia G.", "Marinelli", "Odoardo G.", "Odoardo G.", "Orsina", "Pirro")
devtools::use_data(rksp.0, overwrite=TRUE)

vndf.0 <- list(mtext=loadSegmentedText("tg:vndf.0"))
levels(vndf.0$mtext$Speaker.figure_surface) <- c("Abraham", "Balthasar", 
                                                 "Benvolio", "Graf Capulet", 
                                                 "Bedienter", "Drei Musikanten", 
                                                 "Dritter Musikant", "Dritter Wächter", 
                                                 "Ein Apotheker", "Bürger", "Page", 
                                                 "Erste Bediente", 
                                                 "Bedienter", "Erster Musikant", 
                                                 "Erster Wächter", "Erster Wärter", 
                                                 "Escalus", "Gräfin Capulet", "Gräfin Montague", 
                                                 "Gregorio", "Julia", "Lorenzo", 
                                                 "Marcus", "Mercutio", 
                                                 "Graf Montague", "Peter", "Romeo", 
                                                 "Simson", "Tybalt", 
                                                 "Wächter", "Wärterin", "Wärterin", 
                                                 "Zweiter Bediente", "Zweiter Capulet", 
                                                 "Zweiter Musikant", "Zweiter Wächter")

devtools::use_data(vndf.0, overwrite=TRUE)
