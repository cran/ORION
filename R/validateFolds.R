# Internal: validate cross-validation fold structure
.validateFoldList <- function(foldList, n) {

  if (!is.list(foldList)) {
    stop("`foldList` must be a list.")
  }

  for (run in foldList) {
    if (!is.list(run)) {
      stop("Each CV run must be a list of folds.")
    }

    for (fold in run) {
      if (!is.integer(fold)) {
        stop("Each fold must be an integer vector of indices.")
      }

      if (any(fold < 1 | fold > n)) {
        stop("Fold indices out of bounds.")
      }
    }
  }
  TRUE
}
