#' Notes and instruments
#'
#' @description
#' Basic commands to create notes and select instruments
#'
#' * `note()`: Create notes by combining `note_on()` and `note_off()`.
#' * `note_on()`: Create a command which will start playing a note.
#' * `note_off()`: Create a command which will stop playing a note.
#' * `chord()`: Create command to play notes simultaneously.
#' * `instrument()`: Selects the instruments for the notes following.
#'
#' @importFrom stats approxfun
#' @param time starting time of note or time of changing instrument in milliseconds
#' @param channel channel to use on midi output, in range 1-16,
#'     channel 10 is the drum channel.
#' @param pitch range from 0 to 127, pitch (frequency related)
#' @param pitch_vec vector of pitches forming a chord
#' @param velocity of the note, corresponds to volume of tone
#' @param duration duration of sound produced by this command
#' @param instrument selects instrument, number in range 1-128
#' @name sound_commands
#'
#' @return tibble containing row(s) defining sound and instrument events
#'
#' @import dplyr
#' @import purrr
NULL


#' @export
#' @rdname sound_commands
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
#' @rdname sound_commands
chord <- function(time, channel, pitch_vec, velocity, duration) {
    pitch_vec |>
      map(\(x)note(time, channel, x, velocity, duration)) |>
    bind_rows()
}

#' @export
#' @rdname sound_commands
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
#' @rdname sound_commands
note_off <- function(time, channel, pitch) {
  tibble(
    time = time,
    command = "Note_off_c",
    p1 = channel |> channel_mod(),
    p2 = pitch,
    p3 = 0
  )|> add_class("midi_events")
}

#' @export
#' @rdname sound_commands
instrument <- function(time, channel = 1, instrument = 1) {
  tibble(
    time = time,
    command = "Program_c",
    p1 = channel |> channel_mod(),
    p2 = instrument - 1
  ) |> add_class("midi_events")
}


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


