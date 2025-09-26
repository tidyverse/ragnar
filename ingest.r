paths <- letters
pending <- as.list(letters)
active <- list()
n_workers <- 4L

launch_one <- function() {
  active[[length(active) + 1L]] <<- submit(pending[[lp <- length(pending)]])
  length(pending) <<- lp - 1L
}

while (length(pending)) {
  while (length(active) < n_workers) {
    launch_one()
  }
  is_done <- vapply(pending, resolved, TRUE)
  results <- active[done]
  for (result in results) {
    ragnar_store_update(store, result)
  }
  active <- active[!done]
}
