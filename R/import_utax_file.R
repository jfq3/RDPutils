import_utax_file <- function (in_file = "utax_file.txt", confidence = 0.8) {
  # Read in utax file.
  temp <- read.table(file = in_file, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
  # Extract otu names.
  otus <- temp[ , 1]
  # Extract taxonomy with confidences.
  taxa <- temp[ , 2]
  # Determine number of ranks.
  n.ranks <- vector("integer")
  for (i in 1:nrow(temp)) {
    n.ranks[i] <- count_char_occurrences(":", temp[i, 2])
  }
  n.ranks <- max(n.ranks)
  # Modify taxonomy field.
  # Delete quotation marks
  taxa <- gsub('"', '', taxa)
  # Delete closing parenthesis
  taxa <- gsub(')', '', taxa)
  # Substitute commma for opening parenthesis.
  taxa <- gsub('\\(', ',', taxa)
  # Subsitute underscore for colon
  taxa <- gsub(':', '_', taxa)
  # Create data frame with ranks and confidences in separate columns.
  class.table <- matrix(data = NA, nrow = length(otus), ncol = 2*n.ranks)
  # utax output is not consistent. Sometimes there is no confidence value
  # for the highest rank. This for loop with if/else is how I handled it.
  for (i in 1:nrow(class.table)) {
    taxa.line <- strsplit(taxa[i], ',')
    if (!(substr(taxa.line[[1]], 1, 2)[2] %in% c("0.", "1.", NA))) {
      class.table[i, 1] <- taxa.line[[1]][1]
      class.table[i, 2] <- "1.0"
      for (j in 2:length(taxa.line[[1]])) {
        class.table[i, j+1] <- taxa.line[[1]][j]
      }
    } else {
      for (j in 1:length(taxa.line[[1]])) {
        class.table[i, j] <- taxa.line[[1]][j]
      }
    }
  }
  
  if (all(is.na(class.table[ , (2*n.ranks)]))) {
    class.table <- class.table[ , -c(2*n.ranks-1, 2*n.ranks)]
  }
  
  #Create a vector designating confidence columns.
  conf.col.no <- seq(from=2, to=ncol(class.table), by=2)
  
  # Convert these columns to numeric
  class.table <- as.data.frame(class.table)
  class.table[conf.col.no] <- lapply(class.table[conf.col.no], as.character)
  class.table[conf.col.no] <- lapply(class.table[conf.col.no], as.numeric)
  
  # Create a vector designating taxa columns.
  taxa.col.no <- c(seq(from=1, to=ncol(class.table), by=2))
  
  # Convert these columns to character.
  class.table[taxa.col.no] <- lapply(class.table[taxa.col.no], as.character)
  
  #There may be NA's in some columns, so replace them first.
  # with confidence < specified confidence:
  for (i in 1:nrow(class.table)) {
    for (j in conf.col.no) {
      if (is.na(class.table[i, j])) {
        class.table[i, j] <- confidence/2
      }
    }
  }
  
  # Replace IDs where highest rank is unidentfied.
  for (i in  1:nrow(class.table)) {
    if (class.table[i, 2] < confidence) {
      class.table[i, 2] <- 1
      class.table[i, 1] <- paste("uncl_", class.table[i, 1], sep="")
    }
  }
  
  # Replace IDs where confidence is less than specified confidence:
  # col.no <- seq(from=3, to=ncol(class.table)-1, by=2)
  for (i in 1:nrow(class.table)) {
    for (j in conf.col.no[-1]) {
      if (class.table[i, j] < confidence) {
        class.table[i, j] <- 1
        if(substr(class.table[i, (j-3)], 1, 5)=="uncl_") {
          class.table[i, (j-1)] <- class.table[i, (j-3)]
        }
        else {
          class.table[i, (j-1)] <- paste("uncl_", class.table[i, (j-3)], sep="")
        }
      }
    }
  }
  
  # Remove confidence columns
  class.table <- class.table[ , -conf.col.no]
  # Add row names and column names.
  row.names(class.table) <- otus
  taxa <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")[1:ncol(class.table)]
  colnames(class.table) <- taxa
  # Convert to phyloseq tax_table
  class.table <- as.matrix(class.table)
  class.table <- tax_table(class.table, errorIfNULL = TRUE)
  return(class.table)
}
