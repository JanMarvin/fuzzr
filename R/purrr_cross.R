vec_size_safe <- function(x) {
  if (is.null(x)) return(0)
  if (is.data.frame(x)) return(nrow(x))
  if (length(x) == 0) return(0)
  1L * length(x) # atomic or list with elements
}

# custom purrr::cross() implementation. Not sure if actually needed.
# returns additional arguments from the nested list
purrr_cross <- function(.l) {
  if (purrr::is_empty(.l)) return(.l)

  n <- length(.l)
  names <- names(.l)
  lengths <- sapply(.l, vec_size_safe)

  total_rows <- prod(lengths)
  out <- vector("list", total_rows)

  # precompute indices for each column
  indices <- lapply(seq_len(n), function(j) {
    rep(seq_len(lengths[j]),
        times = if (j == n) 1 else prod(lengths[(j + 1):n]),
        each = if (j == 1) 1 else prod(lengths[1:(j - 1)])
    )
  })

  for (i in seq_len(total_rows)) {
    row <- vector("list", n)
    for (j in seq_len(n)) {
      row[[j]] <- .l[[j]][[indices[[j]][i]]]
    }
    names(row) <- names
    out[[i]] <- row
  }

  purrr::compact(out)
}
