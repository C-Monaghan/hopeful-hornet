library(callr)
library(this.path)

job <- callr::r_bg(function() {
  source(here(this.dir(), "01__simulation.R"))
})

job$is_alive()

job$wait()
d