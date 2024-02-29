#' read midi file
#'
#' @description
#' Read MIDI file into dataframe (tibble)
#'
#' @param midi_file path of a midi file. Needs to be given for input in `read_midi_file`.
#'
#' @name read_midi_file
#'
#' @returns midi_df (tibble with MIDI events in tracks)

read_midi_file <- function(midi_file){
  csv_file <- tempfile(fileext = ".csv")
  midi2csv(midi_file,csv_file)
  utils::read.csv(csv_file,header=FALSE) |>
    as_tibble() -> result
  result |>
  set_names(c("track", "time", "command", paste0("p",1:(ncol(result)-3)))) |>
    mutate_at(vars("command","p1","p2","p3"),stringr::str_trim) |>
    add_class("midi_df")
}


