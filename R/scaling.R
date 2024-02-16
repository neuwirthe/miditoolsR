# scaling commands

scale01 <- function(val) {
  approxfun(x = c(0, 1), y = c(0, 127), rule = 2)(val) |> round()
}

scale11 <- function(val) {
  approxfun(x = c(-1, 1), y = c(0, 127), rule = 2)(val) |> round()
}


scale_long <- function(val) {
  approxfun(x = c(-1, 1), y = c(0, 16383))(val) |>
    round()
}


channel_mod <- function(channel) channel - 1
