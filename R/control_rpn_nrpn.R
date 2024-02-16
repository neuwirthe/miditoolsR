# midi control rpn nrpn

control <- function(time, channel, control_num, control_val) {
  tibble(
    time = time,
    command = "Control_c",
    p1 = channel |> channel_mod(),
    p2 = control_num,
    p3 = control_val
  ) |> add_class("midi_events")
}

rpn <- function(track, time, channel, hi_rpn, lo_rpn, hi_data, lo_data) {
  bind_rows(
    control(time, channel, 100, hi_rpn),
    control(time, channel, 101, lo_rpn),
    control(time, channel, 6, hi_data),
    control(time, channel, 38, lo_data),
    control(time, channel, 101, 127),
    control(time, channel, 100, 127)
  ) |> add_class("midi_events")
}


nrpn <- function(track, time, channel, hi_nrpn, lo_nrpn, hi_data, lo_data) {
  bind_rows(
    control(time, channel, 98, hi_nrpn),
    control(time, channel, 99, lo_nrpn),
    control(time, channel, 6,  hi_data),
    control(time, channel, 38, lo_data),
    control(time, channel, 99, 127),
    control(time, channel, 98, 127)
  ) |> add_class("midi_events")
}
