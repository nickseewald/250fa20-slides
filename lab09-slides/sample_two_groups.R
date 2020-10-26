sample_two_groups <- function(d) {
  dnames <- names(d)
  x <- data.frame(d[, 1],
                  d[sample(1:nrow(d), nrow(d)), 2])
  names(x) <- dnames
  x
}

rowTable <- function(d) {
  addmargins(table(d) / rowSums(table(d)), 2)
}



