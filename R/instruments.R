#' instruments and drums
#'
#' @description
#' Instruments and drums
#'
#' * `instrument()`: selects instrument to be played, differemt channels
#' can play different instruments simultaneously
#' * `bank_select()`: changes set of instruments available
#' (offers variations of standard instruments)
#' * `bank_select_gm2()`: changes set of instruments available
#' (offers variations of standard instruments)
#' if midiplayer supports MIDI standard GM2
#' * `drum_bank_select()`: changes drum set
#'
#' @param time starting time of note or time of changing instrument in milliseconds
#' @param channel channel to use on midi output, in range 1-16,
#'     channel 10 is the drum channel.
#' @param instrument instrument number, range 1 to 128
#' @param bank instrument or drum bank to select
#'
#' @name instruments
#'
#' @export
#' @rdname instruments
instrument <- function(time, channel = 1, instrument = 1) {
  tibble(
    time = time,
    command = "Program_c",
    p1 = channel |> channel_mod(),
    p2 = instrument - 1
  ) |> add_class("midi_events")
}

#' @export
#' @rdname instruments
bank_select <- function(time, channel, bank) {
  bind_rows(
    control(time, channel, 0, bank),
  )
}


#' @export
#' @rdname instruments
drum_bank_select <-
  function(time, channel, bank) {
    bind_rows(
      control(time, channel, 0, 120),
      control(time, channel, 32, 0),
      instrument(time,channel,bank+1)
    )
  }


#' @export
#' @rdname instruments
bank_select_gm2 <- function(time, channel, bank) {
  bind_rows(
    control(time, channel, 0, 121),
    control(time, channel, 32, bank)
  )
}


