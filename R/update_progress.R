update_progress <- function() {
  assign("val", get("val", envir = progress_lock) + 1, envir = progress_lock)
  setTxtProgressBar(pb, get("val", envir = progress_lock))
}