#' Melismatic density
#' @param score A `muse_score`.
#' @param ... Unused.
#' @return Syllable-level data.frame with a summary attribute.
#' @export
feature_melisma_density <- function(score, ...) {
  syllables <- .table_syllable(score)
  if (!nrow(syllables)) return(syllables)
  columns <- intersect(c("syllable_id", "part_id", "voice", "verse", "word_id", "word_uid",
                         "text", "n_notes", "n_attacks", "dur_q", "is_melisma",
                         "score_onset_q", "measure_label"), names(syllables))
  out <- syllables[, columns, drop = FALSE]
  attr(out, "summary") <- data.frame(
    n_syllables = nrow(out),
    melisma_rate = mean(out$is_melisma, na.rm = TRUE),
    mean_notes = mean(out$n_notes, na.rm = TRUE),
    max_notes = max(out$n_notes, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  out
}

#' Melodic contour at musical-event grain
#' @param score A `muse_score`.
#' @param ... Unused.
#' @return One row per pitched nongrace event.
#' @export
feature_contour <- function(score, ...) {
  events <- score$events[!score$events$is_rest & !score$events$is_grace & !is.na(score$events$midi), , drop = FALSE]
  if (!nrow(events)) return(events)
  events$abs_interval <- abs(events$interval)
  events[, intersect(c("event_id", "part_id", "voice", "measure_index", "measure_label",
                        "score_onset_q", "midi", "interval", "abs_interval", "contour"), names(events)), drop = FALSE]
}

feature_interval_profiles <- function(score, ...) feature_contour(score, ...)
muse_validation_problems <- function(score) validate_muse_score(score, strict = FALSE)
as_muse_score <- function(x) { if (!is_muse_score(x)) stop("Object is not a muse_score.", call. = FALSE); x }
muse_pianoroll <- function(...) stop("Plotting helper not included in the minimal CI runner.", call. = FALSE)

#' Generic permutation test
#' @param data A data.frame.
#' @param statistic Scalar numeric statistic function.
#' @param permute Null permutation function returning a data.frame.
#' @param n Positive number of permutations.
#' @param alternative `two.sided`, `greater`, or `less`.
#' @param seed Optional local random seed; the caller's RNG state is restored.
#' @return A `muse_permutation` object.
#' @export
muse_permutation <- function(data, statistic, permute, n = 1000,
                             alternative = c("two.sided", "greater", "less"), seed = NULL) {
  alternative <- match.arg(alternative)
  if (!is.data.frame(data) || !is.function(statistic) || !is.function(permute))
    stop("Invalid permutation-test inputs.", call. = FALSE)
  if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 1 || abs(n - round(n)) > 1e-9)
    stop("`n` must be a positive integer.", call. = FALSE)
  n <- as.integer(round(n))
  run <- .with_seed(seed, {
    observed <- statistic(data)
    if (length(observed) != 1L || !is.finite(observed))
      stop("`statistic` must return one finite number.", call. = FALSE)
    null <- vapply(seq_len(n), function(iteration) {
      permuted_data <- permute(data)
      if (!is.data.frame(permuted_data)) stop("`permute` must return a data.frame.", call. = FALSE)
      value <- statistic(permuted_data)
      if (length(value) != 1L || !is.finite(value))
        stop("Nonfinite permuted statistic at iteration ", iteration, ".", call. = FALSE)
      value
    }, numeric(1))
    center <- mean(null)
    p_value <- switch(alternative,
      greater = (1 + sum(null >= observed)) / (n + 1),
      less = (1 + sum(null <= observed)) / (n + 1),
      two.sided = (1 + sum(abs(null - center) >= abs(observed - center))) / (n + 1))
    list(observed = observed, null = null, p_value = p_value)
  })
  structure(list(observed = run$observed, null = run$null, p_value = run$p_value,
                 n = n, alternative = alternative, seed = seed), class = "muse_permutation")
}

#' @export
print.muse_permutation <- function(x, ...) {
  cat("MUSE permutation test\n",
      sprintf("  observed statistic : %.4f\n", x$observed),
      sprintf("  null mean (sd)     : %.4f (%.4f)\n", mean(x$null), stats::sd(x$null)),
      sprintf("  permutations       : %d\n", x$n),
      sprintf("  alternative        : %s\n", x$alternative),
      sprintf("  p-value            : %.4g\n", x$p_value), sep = "")
  invisible(x)
}
