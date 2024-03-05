#' note manipulation
#'
#' @description
#' Combine `Note_on_c` commands and `Note_off_c commands to a single
#' `Note_c` command or expand `Note_c` command to  `Note_on_c` and `Note_off_c`
#' commands.
#'
#' * `clean_notes_off()`: Replace `Note_on_c` events with volume 0 by `Note_off_c` events
#' * `combine_notes_on_off()` Replace `Note_on_c` and corresponding `Note_off_c` events
#'     by `Note_c` events.
#' * `combine_notes_on_off()` Expand `Note_c` event to `Note_on_c` and corresponding `Note_off_c` events.

#' @name note_manipulation
#'
#' @param midi_df dataframe of class `midi_df`
#' @param midi_df_ext dataframe of class `midi_df_ext`
#'
#' @return dataframe containing midi structures

#' @export
#' @rdname note_manipulation
clean_notes_on_off <- function(midi_df) {
  midi_df |>
    filter_notes() |>
    bind_rows(
      midi_df |>
        filter_drum_beats()
    ) ->
  notes_and_drums
  midi_df |>
    filter(!(.data$command %in% c("Note_on_c", "Note_off_c"))) ->
  other_all
  notes_and_drums |>
    mutate(command = ifelse((.data$command == "Note_on_c") & (.data$p3 == 0),
      "Note_off_c",
      .data$command
    )) |>
    mutate(p3 = ifelse(.data$command == "Note_off_c", 0, .data$p3)) ->
  notes_on_off_cleaned
  bind_rows(
    other_all,
    notes_on_off_cleaned
  ) |>
    arrange(.data$track, .data$time) |>
    combine_notes_other()
}

simplify_notes <- function(clean_df) {
  clean_df |>
    filter(.data$command == "Note_off_c") ->
  notes_off
  clean_df |>
    filter(.data$command == "Note_on_c") ->
  notes_on
  notes_on |>
    rowwise() |>
    group_split() |>
    map(\(x){
      notes_off |>
        filter(.data$time >= x$time & .data$p2 == x$p2) ->
      temp_df
      if (nrow(temp_df) > 0) {
        return(
          bind_rows(
            x,
            temp_df |> utils::head(1)
          )
        )
      } else {
        return(x)
      }
    }) |>
    map(\(x){
      if (nrow(x) == 2) {
        slice(x, 1) |>
          mutate(p4 = slice(x, 2)$time - .data$time) |>
          mutate(command = "Note_c")
      } else {
        slice(x, 1)
      }
    }) |>
    bind_rows()
}

#' @export
#' @rdname note_manipulation
combine_notes_on_off <- function(midi_df) {
  midi_df |>
    filter_others() ->
  other_commands
  midi_df |>
    filter_notes() |>
    clean_notes_on_off() |>
    simplify_notes() ->
  notes
  midi_df |>
    filter_drum_beats() |>
    clean_notes_on_off() |>
    simplify_notes() ->
  drum_beats

  bind_rows(
    other_commands,
    notes,
    drum_beats
  ) |>
    arrange(.data$track, .data$time) |>
    combine_notes_other()
  add_class("midi_df_ext") |>
    remove_class("midi_df")
}

#' @export
#' @rdname note_manipulation
expand_notes_on_off <- function(midi_df_ext) {
  midi_df_ext |>
    filter(!(.data$command == "Note_c")) ->
  other
  midi_df_ext |>
    filter(.data$command == "Note_c") |>
    rowwise() |>
    group_split() |>
    map(\(x){
      end_time <- as.integer(x$time) + as.integer(x$p4)
      bind_rows(
        x |>
          mutate(command = "Note_on_c") |>
          mutate(p4 = NA),
        x |>
          mutate(time = end_time) |>
          mutate(command = "Note_off_c")
      )
    }) |>
    bind_rows() ->
  notes_expanded
  bind_rows(
    other,
    notes_expanded
  ) |>
    arrange(.data$track, .data$time) |>
    combine_notes_other() |>
    add_class("midi_df") |>
    remove_class("midi_df_ext")
}

combine_notes_drums_other <- function(notes = tibble(), drums = tibble(), other = tibble()) {
  notes <-
    notes |>
    note_params2gen_params()
  drums <-
    drums |>
    note_params2gen_params()
  bind_rows(
    notes,
    drums,
    other
  ) |>
    clean_midi_df()
}

clean_midi_df <- function(full_df) {
  full_df |>
    filter(.data$command == "Header") ->
  header
  full_df |>
    filter(.data$command == "End_of_file") ->
  eof
  full_df |>
    filter(!(.data$command %in% c("Header", "End_of_file"))) |>
    group_by(.data$track) |>
    group_split() |>
    map(\(x){
      x |>
        filter(.data$command == "Start_track") -> start_track
      x |>
        filter(.data$command == "End_track") -> end_track
      bind_rows(
        start_track,
        x |>
          filter(!(.data$command %in% c("Start_track", "End_track"))) |>
          note_params2gen_params(),
        end_track
      )
    }) |>
    bind_rows() ->
  tracks
  bind_rows(
    header,
    tracks,
    eof
  )
}

filter_notes <- function(full_df) {
  full_df |>
    filter(.data$command %in% c("Note_c", "Note_on_c", "Note_off_c")) |>
    filter(.data$p1 != 10) ->
  filtered
  filtered |>
    select(track:p3) |>
    mutate(across(p1:p3, as.integer)) ->
  tmp_df
  if ("p4" %in% names(filtered)) {
    if (any(!is.na(filtered$p4))) {
      tmp_df |>
        bind_cols(
          filtered |>
            select(p4) |>
            mutate(p4 = as.integer(p4))
        )
    }
  }
  tmp_df |>
    gen_params2note_params() |>
    remove_class("midi_df") |>
    add_class("midi_events")
}


filter_drum_beats <- function(full_df) {
  full_df |>
    filter(.data$command %in% c("Note_c", "Note_on_c", "Note_off_c")) |>
    filter(.data$p1 == 10) |>
    gen_params2note_params()
}

filter_others <- function(full_df) {
  full_df |>
    filter(!(.data$command %in% c("Note_c", "Note_on_c", "Note_off_c")))
}

gen_params2note_params <- function(midi_df) {
  if ("p1" %in% names(midi_df)) {
    rename(midi_df, channel = p1) -> midi_df
  }
  if ("p2" %in% names(midi_df) & all(midi_df$channel != 10)) {
    rename(midi_df, pitch = p2) -> midi_df
  }
  if ("p2" %in% names(midi_df) & all(midi_df$channel == 10)) {
    rename(midi_df, drum = p2) -> midi_df
  }
  if ("p3" %in% names(midi_df)) {
    rename(midi_df, velocity = p3) -> midi_df
  }
  if ("p4" %in% names(midi_df)) {
    rename(midi_df, duration = p4) -> midi_df
  }
  midi_df
}


note_params2gen_params <- function(midi_df) {
  if ("channel" %in% names(midi_df)) {
    rename(midi_df, p1 = .data$channel) -> midi_df
  }
  if ("pitch" %in% names(midi_df)) {
    rename(midi_df, p2 = pitch) -> midi_df
  }
  if ("drum" %in% names(midi_df)) {
    rename(midi_df, p2 = drum) -> midi_df
  }
  if ("velocity" %in% names(midi_df)) {
    rename(midi_df, p3 = velocity) -> midi_df
  }
  if ("duration" %in% names(midi_df)) {
    rename(midi_df, p4 = duration) -> midi_df
  }
  midi_df
}
