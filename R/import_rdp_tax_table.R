#' Import RDP Classifier Taxonomy Table
#' @aliases import_rdp_tax_table
#' @param in_file A fix-rank tab-delimited text file output by the RDP Classifier
#' @param confidence The confidence level for filtering the taxonomy
#' @usage import_rdp_tax_table(in_file, confidence)
#' @details A confidence value of 0.5 is recommended for shorter amplicons and a value of 0.8 for full-length 16S rRNA gene sequences.
#' @return A phyloseq tax_table object
#' @export
#' @importFrom utils read.table
#' @examples
#' \dontrun{
#' import_rdp_tax_table(in_file = "rdp_classifier_result.tsv", confidence = 0.8)
#' }
#'
import_rdp_tax_table <- function(in_file, confidence=0.5) {
  #Begin with fixed rank output from command line version of classifier.
  class_table <- utils::read.table(in_file, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
  class_table <- class_table[order(class_table[, 1]), ]
  
  rank.pos <- seq(from = 4, to = ncol(class_table), by = 3)
  
  ranks <- class_table[1 , rank.pos]
  ranks <- sapply(ranks, simple_cap)
  
  # Remove unnecessary columns
  class_table <- class_table[ , -c(2, rank.pos)]
  
  # Assign first column of class_table as row names.
  # Delete first column of class_table.
  # Sort class_table by row names. Necessary?
  row.names(class_table) <- class_table[ , 1]
  class_table <- class_table[ , -1]
  # class_table <- class_table[order(rownames(class_table)), ]
  
  # Fix class_table's by consolidating groups based on confidences.
  # For example, genera classified with less than specified confidence become uncl_family, etc.
  
  # Create a vector designating confidence columns
  col.no <- seq(from = 2, to = ncol(class_table), by = 2)
  
  # Subset to confidence columns; covert to a matrix
  confidence_matrix <- class_table[, col.no]
  confidence_matrix <- as.matrix(confidence_matrix)
  
  # Create a vector designating taxa
  tax_no <- seq(from = 1, to = ncol(class_table), by = 2)
  # Extract the taxa to a matrix
  taxa_matrix <- class_table[, tax_no]
  taxa_matrix <- as.matrix(taxa_matrix)
  
  # Take care of confidences less than cutoff in first column
  for (i in 1:nrow(taxa_matrix)) {
    if (confidence_matrix[i, 1] < confidence) {
      # taxa_matrix[i, 1] <- paste0("uncl_", taxa_matrix[i, 1])
      taxa_matrix[i, 1] <- "uncl_domain"
    }
  }
  
  # Take care of confidences less than cutoff in other columns
  for (i in 1:nrow(taxa_matrix)) {
    for (j in 2: 6) {
      if(substring(taxa_matrix[i, j-1], 1, 4) == "uncl") {
        taxa_matrix[i, j] <- taxa_matrix[i, j-1]
      } else {
        if(confidence_matrix[i, j] < confidence) {
          taxa_matrix[i, j] <-  paste0("uncl_", taxa_matrix[i, j-1])
        }
      }
    }
  }
  
  colnames(taxa_matrix) <- ranks
  
  class_table <- phyloseq::tax_table(as.matrix(taxa_matrix), errorIfNULL=TRUE)
  return(class_table)
}

