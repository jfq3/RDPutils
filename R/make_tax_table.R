make_tax_table <- function(in_file="fixrank_classified.txt", ragged = FALSE, confidence=0.5) {
  # Begin with output from command line version of classifier.
  # In this example, the example output is named "dist_03_class.txt."
  # It is fixed rank.
  class.table <- read.table(in_file, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
  
  rank.pos <- seq(from = 4, to = ncol(class.table), by = 3)
  ranks <- as.vector(class.table[1 , rank.pos])
  
  # Remove unnecessary columns
  class.table <- class.table[ , -c(2, rank.pos)]

  # Assign first column of class.table as row names.
  # Delete first column of class.table.
  # Sort class.table by row names.
  row.names(class.table) <- class.table[ , 1]
  class.table <- class.table[ , -1]
  class.table <- class.table[order(rownames(class.table)), ]
  
  # If ragged = FALSE, fix class.table's by consolidating groups based on confidences.
  # For example, genera classified with less than specified confidence become unclassified_family, etc.

    # Create a vector designating confidence columns
    col.no <- seq(from = 2, to = ncol(class.table), by = 2)
    
    # There may be NA's in some columns, so replace them first 
    # with confidence < specified confidence:
    for (i in 1:nrow(class.table)) {
      for (j in col.no) {
        if (is.na(class.table[i, j])) {
          class.table[i, j] <- confidence/2
        }
      }
    }
    
    #Replace IDs where first rank is unidentfied.
    first.rank <- ranks[1]
    for (i in  1:nrow(class.table)) {
      if (class.table[i, 2] < confidence) {
        class.table[i, 2] <- 1
        if (ragged) {
          class.table[i, 1] <- ""
        } else {
          class.table[i, 1] <- paste("uncl", first.rank, sep = "_")
        }
      }
    }
    
    #Replace IDs where confidence is less than specified confidence:
    col.no <- seq(from=4, to=ncol(class.table), by=2)
    for (i in 1:nrow(class.table)) {
      for (j in col.no) {
        if (class.table[i, j] < confidence) {
          class.table[i, j] <- 1
          if (ragged) {
            if (substr(class.table[i, (j-3)], 1, 4)=="uncl") {
                class.table[i, (j-1)] <- ""
            } else {
                class.table[i, (j-1)] <- ""
            }
          } else {
            if(substr(class.table[i, (j-3)], 1, 4)=="uncl") {
              class.table[i, (j-1)] <- class.table[i, (j-3)]
            } else {
              class.table[i, (j-1)] <- paste("uncl", class.table[i, (j-3)], sep="_")
            }
          }
        }
      }
    }    
  #Convert character columns to factors.
  i <- sapply(class.table, is.character) 
  class.table[i] <- lapply(class.table[i], as.factor) 
  
  #Remove confidence columns
  class.table <- class.table[ , -c(seq(from = 2, to = ncol(class.table), by = 2))]
  ranks <- sapply(ranks, simple_cap)
  colnames(class.table) <- ranks
  class.table <- tax_table(as.matrix(class.table), errorIfNULL=TRUE)
  return(class.table)
  }
    
