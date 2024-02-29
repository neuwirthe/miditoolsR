#' tuning
#'
#' @description
#' Tuning
#'
#' * `mastertune_coarse()`: changes tuning of instrument in semitones, i.e.
#' transposing
#' * `mastertune_fine()`: changes tuning of instruments in cents
#' (subdivisions of a semitone)
#'
#' @param time time of changing tuning in milliseconds
#' @param channel channel to use on midi output, in range 1-16
#' @param semitones for changing tuning in semitone steps, range -24 to 24
#' @param cents for changing tuning in cent steps, range -100 to 100
#'
#' @name tuning
#'




#' @export
#' @rdname tuning
mastertune_coarse <- function(time, channel, semitones){
  rpn(time, channel, 2, 0, semitones + 64, 0)
}

#' @export
#' @rdname tuning
mastertune_fine <- function(time, channel, cents){
  val <- scale_long(cents/100) |> round()
  hi_val <- (val/128) |> floor()  |>  as.integer()
  lo_val <- (val %% 128) |> floor()  |> as.integer()
  rpn(time, channel, 1, 0, hi_val, lo_val)
}

