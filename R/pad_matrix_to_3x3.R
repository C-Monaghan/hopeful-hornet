pad_matrix_to_3x3 <- function(mat) {
  full_states <- paste("To", 1:3)
  full_rows <- paste("From", 1:3)
  
  mat <- as.matrix(mat)
  mat_padded <- matrix(0, nrow = 3, ncol = 3,
                       dimnames = list(full_rows, full_states))
  
  existing_rows <- intersect(rownames(mat), full_rows)
  existing_cols <- intersect(colnames(mat), full_states)
  
  mat_padded[existing_rows, existing_cols] <- mat[existing_rows, existing_cols]
  
  return(mat_padded)
}
