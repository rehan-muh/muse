#' Flatten a MUSE score
#' @param score A `muse_score`.
#' @param level `event`, `note`, `syllable`, or `word`.
#' @param verse Optional verse labels.
#' @return A data.frame.
#' @export
muse_table <- function(
    score,
    level = c("event", "note", "syllable", "word"),
    verse = NULL) {
  if (!is_muse_score(score)) {
    stop("`score` must be a muse_score.", call. = FALSE)
  }
  level <- match.arg(level)
  out <- switch(
    level,
    event = score$events,
    note = .table_note(score),
    syllable = .table_syllable(score),
    word = .table_word(score)
  )
  if (!is.null(verse) && level != "event" && "verse" %in% names(out)) {
    out <- out[out$verse %in% as.character(verse), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

.table_note <- function(score) {
  sung <- score$events[
    !score$events$is_rest & !score$events$is_grace,
    ,
    drop = FALSE
  ]
  if (!nrow(sung)) return(sung)

  alignment <- score$meta$alignment %||% .empty_alignment()
  if (!nrow(alignment)) {
    sung$syllable_id <- NA_integer_
    sung$verse <- NA_character_
    sung$melisma_pos <- NA_integer_
    sung$explicit <- NA
    sung$word_id <- NA_integer_
    sung$word_uid <- NA_character_
    sung$word_index <- NA_integer_
    sung$syl_in_word <- NA_integer_
    sung$syllabic <- NA_character_
    sung$syllable_text <- NA_character_
    sung$syllable_n_notes <- NA_integer_
    sung$n_attacks <- NA_integer_
    sung$is_melisma <- NA
    sung$alignment_policy <- NA_character_
    return(sung)
  }

  alignment_columns <- alignment[, c(
    "event_id", "syllable_id", "verse", "melisma_pos", "explicit"
  ), drop = FALSE]
  out <- merge(
    sung,
    alignment_columns,
    by = "event_id",
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(score$syllables)) {
    syllable_columns <- score$syllables[, c(
      "syllable_id", "word_id", "word_uid", "word_index", "syl_in_word",
      "syllabic", "text", "n_notes", "n_attacks", "is_melisma",
      "alignment_policy"
    ), drop = FALSE]
    names(syllable_columns)[names(syllable_columns) == "text"] <-
      "syllable_text"
    names(syllable_columns)[names(syllable_columns) == "n_notes"] <-
      "syllable_n_notes"
    out <- merge(
      out,
      syllable_columns,
      by = "syllable_id",
      all.x = TRUE,
      sort = FALSE
    )
  }

  out <- out[
    order(
      out$part_id,
      out$voice,
      out$score_onset_q,
      out$seq,
      out$verse,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

.table_syllable <- function(score) {
  syllables <- score$syllables
  if (!nrow(syllables)) return(syllables)

  event_columns <- intersect(
    c(
      "event_id", "measure", "measure_index", "measure_label", "onset_q",
      "measure_onset_q", "score_onset_q", "onset_beat", "beat_strength",
      "midi"
    ),
    names(score$events)
  )
  onset_events <- score$events[, event_columns, drop = FALSE]
  names(onset_events)[names(onset_events) == "event_id"] <- "onset_event_id"
  out <- merge(
    syllables,
    onset_events,
    by = "onset_event_id",
    all.x = TRUE,
    sort = FALSE
  )
  out <- out[
    order(out$part_id, out$voice, out$verse, out$syllable_id),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

.table_word <- function(score) {
  syllables <- .table_syllable(score)
  if (!nrow(syllables)) return(data.frame())

  rows <- lapply(split(syllables, syllables$word_uid), function(word) {
    word <- word[order(word$syl_in_word), , drop = FALSE]
    data.frame(
      part_id = word$part_id[[1L]],
      voice = word$voice[[1L]],
      staff = word$staff[[1L]],
      verse = word$verse[[1L]],
      word_id = word$word_id[[1L]],
      word_uid = word$word_uid[[1L]],
      word_index = word$word_index[[1L]],
      word_text = paste(word$text, collapse = ""),
      n_syllables = nrow(word),
      n_notes = sum(word$n_notes, na.rm = TRUE),
      n_attacks = sum(word$n_attacks, na.rm = TRUE),
      any_melisma = any(word$is_melisma, na.rm = TRUE),
      onset_event_id = word$onset_event_id[[1L]],
      onset_measure = word$measure_label[[1L]],
      onset_q = word$onset_q[[1L]],
      score_onset_q = word$score_onset_q[[1L]],
      stringsAsFactors = FALSE
    )
  })
  out <- .rbind_fill(rows)
  out <- out[
    order(out$part_id, out$voice, out$verse, out$word_index),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}
