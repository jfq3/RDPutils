make_tax_table <- function(in_file="fixrank_classified.txt", confidence=0.5) {
  #Begin with output from command line version of classifier.
  #    In this example, the example output is named "dist_03_class.txt."
  #    It is fixed rank.
  class.table <- read.table(in_file, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
  head(class.table)
  
  #Remove unnecessary columns
  class.table <- class.table[ , -c(2,4,7,10,13, 16, 19)]
  head(class.table)
  
  #Add column names
  col.labels <- c("seq.id", "domain", "domain.pc", "phylum", "phylum.pc", "class", "class.pc", "order", "order.pc", "family", "family.pc", "genus", "genus.pc")
  colnames(class.table) <- col.labels
  head(class.table)
  rm(col.labels)
  
  #    Assign class.table$seq.id as row names of class.table.
  #    Delete class.table$seq.id.
  #    Sort class.table by row names.
  row.names(class.table) <- class.table$seq.id
  class.table <- class.table[ , -1]
  class.table <- class.table[order(rownames(class.table)), ]
  head(class.table)
  dim(class.table)
  
  #Fix class.table's with 12 columns by consolidating groups based on confidences.
  #For eample, genera classified with less than specified confidence become unclassified_family, etc.

  #If columns are factors, convert them to character.
  #   Was not necessary.
  i <- sapply(class.table, is.factor) 
  class.table[i] <- lapply(class.table[i], as.character) 
  rm(i)
  
  #Create a vector designating confidence columns
  col.no <- seq(from=2, to=12, by=2)
  
  #There may be NA's in some columns, so replace them first 
  # with confidence < specified confidence:
  for (i in 1:nrow(class.table)) {
    for (j in col.no) {
      if (is.na(class.table[i, j])) {
        class.table[i, j] <- 0.2
      }
    }
  }
  
  #Replace IDs where domain is unidentfied.
  for (i in  1:nrow(class.table)) {
    if (class.table[i, 2] < confidence) {
      class.table[i, 2] <- 1
      class.table[i, 1] <- "unclassified_Domain"
    }
  }
  
  #Replace IDs where confidence is less than 0specified confidence:
  col.no <- seq(from=4, to=12, by=2)
  for (i in 1:nrow(class.table)) {
    for (j in col.no) {
      if (class.table[i, j] < confidence) {
        class.table[i, j] <- 1
        if(substr(class.table[i, (j-3)], 1, 7)=="unclass") {class.table[i, (j-1)] <- class.table[i, (j-3)]}
        else {class.table[i, (j-1)] <- paste("unclassified_", class.table[i, (j-3)], sep="")}
      }
    }
  }
  
  #Convert character columns to factors.
  i <- sapply(class.table, is.character) 
  class.table[i] <- lapply(class.table[i], as.factor) 
  
  #Remove confidence columns
  class.table <- class.table[ , -c(2, 4, 6, 8, 10, 12)]
  head(class.table)
  class.table <- tax_table(as.matrix(class.table), errorIfNULL=TRUE)
  return(class.table)
}