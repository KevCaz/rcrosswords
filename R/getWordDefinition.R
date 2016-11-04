getWordDefinition <- function(word){

    require(RCurl)



    ### Scrap web page

    url <- getURL(paste('http://dictionnaire.sensagent.leparisien.fr/', tolower(word), '/fr-fr/', sep = ''))
    con <- textConnection(url)
    wbp <- readLines(con, encoding = 'utf8')
    close(con)

    wbp <- wbp[grep('class=\"wording\"', wbp)]
    wbp <- wbp[grep('definitions|divSynonyms', wbp)]
    wbp <- strsplit(wbp, 'wording\">')

    if (length(wbp) > 0){

        def <- NULL
        for (i in 1 : length(wbp)){
            pos <- grep('[0-9]\\.|title', wbp[[i]])
            sop <- which(pos == length(wbp[[i]]))
            if (length(sop) > 0) pos <- pos[-sop]
            if (length(pos) > 0){
                pos <- pos + 1
                for (j in 1 : length(pos)){
                    def <- c(def, strsplit(wbp[[i]][pos[j]], '</span>')[[1]][1])
                }
            }
        }
        for (i in 1 : length(def)){
            pos <- which(letters == substr(def[i], 1, 1))
            if (length(pos) > 0)
                def[i] <- gsub('\\.$', '', paste(LETTERS[pos], substr(def[i], 2, nchar(def[i])), sep = ''))
        }
        def <- paste(def, collapse = ' ; ')
    } else {
        def <- ''
    }

    return(data.frame(word = toupper(word), definition = def))
}
