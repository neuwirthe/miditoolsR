#' Midi controls
#'
#' @description
#' Commands to modify instrument sounds
#'
#' @param time time for applying effect
#' @param channel channel number, range 1-16, channel 10 is drum channel
#' @param volume volume of sound, range 0-1
#' @param on_off switch, 0 is off, 1 is on
#' @param value strength of effect, range from 0 to 1
#'
#'
#'
#' @return midi_event dataframe
#'
#' @name sound_modifier_commands


#' @export
#' @rdname sound_modifier_commands
pitch_bend <- function(time, channel, bend) {
#' @param bend amout of pitch bending, range -1 to 1
  tibble(
    time = time,
    command = "Pitch_bend_c",
    p1 = channel |> channel_mod(),
    p2 = scale_long(bend)
  )
}




#' @export
#' @rdname sound_modifier_commands
pitch_bend_range <- function(time, channel, semitones){
#' @param semitones maximum pitch bend for pitch bend values -1 and 1
  hi_val <- semitones |> floor() |> as.integer()
  cents <- (semitones * 100) %% 100
  rpn(time, channel, 0, 0, hi_val, cents)
}

#' @export
#' @rdname sound_modifier_commands
pan <- function(time, channel, position) {
#' @param position position of instrument in stereo space, range from -1 (left) to 1 (right)
  pos <- scale11(position)
  control(
    time, channel, 10,
    position |> scale11()
  )
}


#' @export
#' @rdname sound_modifier_commands
volume <- function(time, channel, volume) {
  control(
    time, channel , 7,
    volume |> scale01()
  )
}


#' @export
#' @rdname sound_modifier_commands
expression <- function(time, channel, volume) {
  control(
    time, channel, 11,
    volume |> scale01()
  )
}


#' @export
#' @rdname sound_modifier_commands
modulation <- function(time, channel, on_off) {
  control(
    time, channel, 1,
    on_off |> scale01()
  )
}




#' @export
#' @rdname sound_modifier_commands
sustain <- function(time, channel, on_off) {
  control(
    time, channel, 64,
    on_off |> scale01()
  )
}


#' @export
#' @rdname sound_modifier_commands
reverb <- function(time, channel, value) {
  control(
    time, channel, 91,
    value |> scale01()
  )
}



#' @export
#' @rdname sound_modifier_commands
tremolo <- function(time, channel, value) {
  control(
    time, channel, 92,
    value |> scale01()
  )
}

#' @export
#' @rdname sound_modifier_commands
chorus <- function(time, channel, value) {
  control(
    time, channel, 93,
    value |> scale01()
  )
}





