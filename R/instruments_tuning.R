#' instruments and tuning
#'
#' @description
#' Instruments and tuning
#'
#' * `instrument()`: selects instrument to be played, differemt channels
#' can play different instruments simultaneously
#' * `mastertune_coarse()`: changes tuning of instrument in semitones, i.e.
#' transposing
#' * `mastertune_fine()`: changes tuning of instruments in cents
#' (subdivisions of a semitone)
#' * `bank_select()`: changes set of instruments available
#' (offers variations of standard instruments)
#' * `bank_select_gm2()`: changes set of instruments available
#' (offers variations of standard instruments)
#' if midiplayer supports MIDI standard GM2
#'
#' @param time starting time of note or time of changing instrument in milliseconds
#' @param channel channel to use on midi output, in range 1-16,
#'     channel 10 is the drum channel.
#' @param instrument instrument number, range 1 to 128
#' @param semitones for changing tuning in semitone steps, range -24 to 24
#' @param cents for changing tuning in cent steps, range -100 to 100
#' @param bank instrument or drum bank to select
#'
#' @name instruments_tuning
#'
#' @export
#' @rdname instruments_tuning
instrument <- function(time, channel = 1, instrument = 1) {
  tibble(
    time = time,
    command = "Program_c",
    p1 = channel |> channel_mod(),
    p2 = instrument - 1
  ) |> add_class("midi_events")
}


#' @export
#' @rdname instruments_tuning
mastertune_coarse <- function(time, channel, semitones){
  rpn(time, channel, 2, 0, semitones + 64, 0)
}

#' @export
#' @rdname instruments_tuning
mastertune_fine <- function(time, channel, cents){
  val <- scale_long(cents/100) |> round()
  hi_val <- (val/128) |> floor()  |>  as.integer()
  lo_val <- (val %% 128) |> floor()  |> as.integer()
  rpn(time, channel, 1, 0, hi_val, lo_val)
}

#' @export
#' @rdname instruments_tuning
bank_select <- function(time, channel, bank) {
  bind_rows(
    control(time, channel, 0, bank),
  )
}


#' @export
#' @rdname instruments_tuning
drum_bank_select <-
  function(time, channel, bank) {
    bind_rows(
      control(time, channel, 0, 120),
      control(time, channel, 32, 0),
      instrument(time,channel,bank+1)
    )
  }


#' @export
#' @rdname instruments_tuning
bank_select_gm2 <- function(time, channel, bank) {
  bind_rows(
    control(time, channel, 0, 121),
    control(time, channel, 32, bank)
  )
}


