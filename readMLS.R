## function to extract data from MLS emails
readMLS <- function(id) {
  
  link <- paste("http://emailrpt.gsmls.com/public/show_public_report_rpt.do?report=clientfull&Id=", 
                id, sep = "")
  
  doc <- readLines(link, warn = FALSE, skipNul = TRUE)
  
  main <- regmatches(doc, regexpr("<div class=\"l town\">.+</div>|<div class=\"l address\">.+</div>", doc))
  main <- gsub("<div class=\"l .{4,7}\">|</div>|&nbsp;", "", main)
  main <- unlist(strsplit(main, "\\*"))

  
  stage <- regmatches(doc, 
                      regexpr("<div class=\"report-row-label.{0,12}\">.*</div>|<div class=\"report-row-label normalWrap.{0,10}\">.*</div>|<div class=\".{0,6}l report-row-label.{0,21}\">.*</div>", 
                              doc))
  
  stage <- stage[!grepl("<label>:</label>", stage)]
  
  stage[grepl("Kitch:", stage) & grepl("normalWrap", stage)] <- gsub("Kitch:", "KitchDesc:", 
                                                                     stage[grepl("Kitch:", stage) & grepl("normalWrap", stage)])
  
  stage[grepl("FHA55+:", stage, fixed = TRUE)] <- gsub("+", "", stage[grepl("FHA55+:", stage, fixed = TRUE)], fixed = TRUE)
  
  labs <- regmatches(stage, regexpr("<label>.+</label>", stage))
  labs <- gsub("<label>|</label>|&nbsp;", "", labs)

  stage <- gsub("<div class=\"[clear ]*[l ]*report-row-label *[ normalWrap| fieldHide]*\">|</*label>|</div>|&nbsp;", 
                "", stage)
  
  vals <- paste(stage, collapse = "")
  vals <- gsub("\\s+", " ", vals)
  vals <- strsplit(vals, paste(unique(labs), collapse = "|"))
  vals <- unlist(vals)
  vals <- vals[-1]
  
  df <- data.frame(labs, vals)
  df$labs <- gsub(":", "", df$labs)
  df$vals <- as.character(df$vals)
  
  df$reportId <- id
  df$rnum <- 1:nrow(df)
  df$propertyId <- 0
  
  for (i in 1:(sum(df$labs == "MLS#")-1)) {
          df$propertyId[df$propertyId == 0 & df$rnum < which(df$labs == "MLS#")[i+1]] <- i
  }
  
  df$propertyId[df$propertyId == 0] <- sum(df$labs == "MLS#")
  
  df$propertyId <- paste(df$reportId, df$propertyId, sep = "_")
  
  df <- subset(df, select = -rnum)
  
  df <- df[!duplicated(df[ ,c("labs", "reportId", "propertyId")]), ]

  df <- dcast(df, reportId + propertyId ~ labs, value.var = "vals")

  return(df)
      
}