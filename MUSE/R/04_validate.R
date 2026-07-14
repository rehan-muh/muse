#' Validate a MUSE score
#'
#' Returns structured problems or stops in strict mode. This is intended for
#' package users, test suites, and code-review pipelines.
#'
#' @param score A `muse_score`-like list.
#' @param strict Stop when an error-level problem is found.
#' @return A data.frame with `severity`, `table`, `field`, and `message`.
#' @export
validate_muse_score <- function(score, strict = FALSE) {
  problems <- list()
  add <- function(severity, table, field, message) {
    problems[[length(problems) + 1L]] <<- data.frame(
      severity = severity,
      table = table,
      field = field,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  if (!is.list(score)) {
    add("error", "score", NA_character_, "Score must be a list-like object.")
    out <- .rbind_fill(problems, .empty_problems())
    if (strict) stop(out$message[[1L]], call. = FALSE)
    return(out)
  }

  table_names <- c("events", "lyrics", "syllables", "measures", "parts")
  required <- c(table_names, "meta")
  for (nm in required) {
    if (is.null(score[[nm]])) add("error", nm, NA_character_, "Missing component.")
  }
  for (nm in table_names) {
    if (!is.null(score[[nm]]) && !is.data.frame(score[[nm]])) {
      add("error", nm, NA_character_, "Component must be a data.frame.")
    }
  }
  if (!is.null(score$meta) && !is.list(score$meta)) {
    add("error", "meta", NA_character_, "Component must be a list.")
  }

  events <- if (is.data.frame(score$events)) score$events else NULL
  lyrics <- if (is.data.frame(score$lyrics)) score$lyrics else NULL
  syllables <- if (is.data.frame(score$syllables)) score$syllables else NULL
  measures <- if (is.data.frame(score$measures)) score$measures else NULL
  parts <- if (is.data.frame(score$parts)) score$parts else NULL

  if (!is.null(events)) {
    req <- c(
      "event_id", "part_id", "voice", "staff", "measure_index", "seq",
      "onset_q", "score_onset_q", "duration_q", "score_offset_q",
      "is_rest", "is_grace"
    )
    for (nm in setdiff(req, names(events))) {
      add("error", "events", nm, "Required column is missing.")
    }
    if ("event_id" %in% names(events)) {
      if (anyNA(events$event_id)) add("error", "events", "event_id", "Missing event identifier found.")
      if (anyDuplicated(events$event_id)) add("error", "events", "event_id", "Event identifiers are not unique.")
    }
    if ("duration_q" %in% names(events) && any(events$duration_q < 0, na.rm = TRUE)) {
      add("error", "events", "duration_q", "Negative duration found.")
    }
    if ("duration_missing" %in% names(events) && any(events$duration_missing %in% TRUE)) {
      add("error", "events", "duration_q", "A nongrace note has no MusicXML duration.")
    }
    if (all(c("score_onset_q", "score_offset_q") %in% names(events)) &&
        any(events$score_offset_q + 1e-9 < events$score_onset_q, na.rm = TRUE)) {
      add("error", "events", "score_offset_q", "Offset precedes onset.")
    }
    if (all(c("measure_onset_q", "score_onset_q") %in% names(events)) &&
        any(events$score_onset_q + 1e-9 < events$measure_onset_q, na.rm = TRUE)) {
      add("error", "events", "score_onset_q", "Event begins before its measure.")
    }
  }

  if (!is.null(parts) && "part_id" %in% names(parts)) {
    if (anyNA(parts$part_id) || any(!nzchar(parts$part_id))) {
      add("error", "parts", "part_id", "Part identifiers must be nonempty.")
    }
    if (anyDuplicated(parts$part_id)) add("error", "parts", "part_id", "Part identifiers are not unique.")
    if (!is.null(events) && "part_id" %in% names(events) &&
        !all(events$part_id %in% parts$part_id)) {
      add("error", "events", "part_id", "Event refers to an unknown part.")
    }
  }

  if (!is.null(measures)) {
    req <- c("part_id", "measure_index", "measure_label", "measure_onset_q")
    for (nm in setdiff(req, names(measures))) add("error", "measures", nm, "Required column is missing.")
    if ("divisions" %in% names(measures) && any(!is.finite(measures$divisions) | measures$divisions <= 0, na.rm = TRUE)) {
      add("error", "measures", "divisions", "Divisions must be finite and positive.")
    }
    if (all(c("part_id", "measure_index") %in% names(measures))) {
      key <- paste(measures$part_id, measures$measure_index, sep = "\r")
      if (anyDuplicated(key)) add("error", "measures", "measure_index", "Part/measure index keys are not unique.")
      for (pid in unique(measures$part_id)) {
        x <- measures$measure_onset_q[measures$part_id == pid]
        if (length(x) > 1L && any(diff(x) < -1e-9, na.rm = TRUE)) {
          add("error", "measures", "measure_onset_q", paste0("Measure onsets decrease in part ", pid, "."))
        }
      }
    }
  }

  if (!is.null(lyrics) && nrow(lyrics) && "event_id" %in% names(lyrics) &&
      !is.null(events) && "event_id" %in% names(events) &&
      !all(lyrics$event_id %in% events$event_id)) {
    add("error", "lyrics", "event_id", "Lyric refers to an unknown event.")
  }

  if (!is.null(syllables)) {
    if ("syllable_id" %in% names(syllables)) {
      if (anyNA(syllables$syllable_id)) add("error", "syllables", "syllable_id", "Missing syllable identifier found.")
      if (anyDuplicated(syllables$syllable_id)) add("error", "syllables", "syllable_id", "Syllable identifiers are not unique.")
    }
    if (nrow(syllables) && "onset_event_id" %in% names(syllables) &&
        !is.null(events) && "event_id" %in% names(events) &&
        !all(syllables$onset_event_id %in% events$event_id)) {
      add("error", "syllables", "onset_event_id", "Syllable refers to an unknown onset event.")
    }
    if (nrow(syllables) && "word_uid" %in% names(syllables) &&
        any(is.na(syllables$word_uid) | !nzchar(syllables$word_uid))) {
      add("error", "syllables", "word_uid", "Word keys must be nonempty.")
    }
  }

  alignment <- if (is.list(score$meta) && is.data.frame(score$meta$alignment)) score$meta$alignment else NULL
  if (is.list(score$meta) && !is.null(score$meta$alignment) && !is.data.frame(score$meta$alignment)) {
    add("error", "alignment", NA_character_, "`meta$alignment` must be a data.frame.")
  }
  if (!is.null(alignment) && nrow(alignment)) {
    if (!is.null(events) && "event_id" %in% names(alignment) &&
        !all(alignment$event_id %in% events$event_id)) {
      add("error", "alignment", "event_id", "Alignment refers to an unknown event.")
    }
    if (!is.null(syllables) && "syllable_id" %in% names(alignment) &&
        !all(alignment$syllable_id %in% syllables$syllable_id)) {
      add("error", "alignment", "syllable_id", "Alignment refers to an unknown syllable.")
    }
    if (all(c("event_id", "verse") %in% names(alignment))) {
      key <- paste(alignment$event_id, alignment$verse, sep = "\r")
      if (anyDuplicated(key)) add("error", "alignment", "event_id", "An event has multiple alignments in the same verse.")
    }
  }

  out <- .rbind_fill(problems, .empty_problems())
  if (strict && any(out$severity == "error")) {
    details <- paste0(out$table[out$severity == "error"], ": ", out$message[out$severity == "error"])
    stop(paste(details, collapse = " "), call. = FALSE)
  }
  out
}

.empty_problems <- function() {
  data.frame(
    severity = character(),
    table = character(),
    field = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

#' Report validation problems
#' @param score A `muse_score`.
#' @return Same as `validate_muse_score(score, strict = FALSE)`.
#' @export
muse_problems <- function(score) validate_muse_score(score, strict = FALSE)
