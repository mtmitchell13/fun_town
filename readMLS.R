## function to extract data from MLS emails
readMLS <- function(id) {
        
        link <- paste("http://emailrpt.gsmls.com/public/show_public_report_rpt.do?report=clientfull&Id=", 
                      id, sep = "")
        
        doc <- readLines(link, warn = FALSE, skipNul = TRUE)
        
        stage <- regmatches(doc, 
                         regexpr("<div class=\"report-row-label.{0,12}\">.*</div>|<div class=\"report-row-label normalWrap.{0,10}\">.*</div>|<div class=\".{0,6}l report-row-label.{0,21}\">.*</div>", 
                      doc))

        stage <- gsub("<div class=\"report-row-label.{0,21}\">|<div class=\".{0,6}l report-row-label.{0,21}\">|</div>|<label>|&nbsp;", 
                   "", stage)
        
        labs <- regmatches(stage, regexpr(".+</label>", stage))
        labs <- gsub("</label>", "", labs)
        
        for (i in 2:(length(stage))) {
                if (stage[grepl("</label>", stage[i-1]) & !grepl("</label>", stage[i])]) {
                        print(stage[i])
                }
        }
        
}       