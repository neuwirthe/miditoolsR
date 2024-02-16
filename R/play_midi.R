#' play midi objects
#' @description
#' Play all kinds of midi related objects
#'
#' @param object midi object
#'
#' @name play
#'
#' @export
#' @rdname play
play <- function(object) UseMethod("play")

#' @export
play.character <- function(object) {
  if (!is.character(object)) stop("Argument is not a file path")
  if (!file.exists(object)) stop(paste("file", object, "does not exist"))
  if (!midiplayR::is_midi_file(object)) stop("is not a midi file")
  midiplayR::play_midi_file(object)
}

#' @export
play.midi_df <- function(object) {
  object |>
    make_midi_csv() |>
    csv2midi() |>
    play()
}


#' @export
play.midi_track <- function(object) {
  object |>
    make_midi_df() |>
    make_midi_csv() |>
    csv2midi() |>
    play()
}


#' @export
play.midi_events <- function(...) {
  objects <- list(...)
  bind_rows(objects) |>
    make_midi_track() |>
    make_midi_df() |>
    make_midi_csv() |>
    csv2midi() |>
    play()
}
