#' Notes
#'
#' @description
#' Basic commands to create notes and select instruments
#'
#' * `note()`: Create notes by combining `note_on()` and `note_off()`.
#' * `note_on()`: Create a command which will start playing a note.
#' * `note_off()`: Create a commandx which will stop playing a note.
#' * `chord()`: Create command to play notes simultaneously.
#'
#' @importFrom stats approxfun
#' @param time starting time of note or time of changing instrument in milliseconds
#' @param channel channel to use on midi output, in range 1-16,
#'     channel 10 is the drum channel.
#' @param pitch range from 0 to 127, pitch (frequency related)
#' @param pitch_vec vector of pitches forming a chord
#' @param velocity of the note, corresponds to volume of tone
#' @param duration duration of sound produced by this command
#'
#' @name notes
#'
#' @return tibble containing row(s) defining sound and instrument events
#'
#' @import dplyr
#' @import purrr
NULL


#' @export
#' @rdname notes
note <- function(time, channel, pitch, velocity, duration) {
  bind_rows(
    note_on(
      time,
      channel,
      pitch,
      velocity
    ),
    note_off(time + duration, channel, pitch)
  ) |>
    add_class("midi_events")
}


#' @export
#' @rdname notes
chord <- function(time, channel, pitch_vec, velocity, duration) {
  pitch_vec |>
    map(\(x)note(time, channel, x, velocity, duration)) |>
    bind_rows() |> add_class("midi_events")
}

#' @export
#' @rdname notes
note_on <- function(time, channel, pitch, velocity) {
  tibble(
    time = time,
    command = "Note_on_c",
    p1 = channel |> channel_mod(),
    p2 = pitch,
    p3 = velocity |> scale01()
  ) |> add_class("midi_events")
}


#' @export
#' @rdname notes
note_off <- function(time, channel, pitch) {
  tibble(
    time = time,
    command = "Note_off_c",
    p1 = channel |> channel_mod(),
    p2 = pitch,
    p3 = 0
  ) |> add_class("midi_events")
}
