options(stringsAsFactors = FALSE, width = 120)
suppressPackageStartupMessages(library(MUSE))

out_dir <- "analysis-output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source_root <- "symbtr/MusicXML"
if (!dir.exists(source_root)) stop("Missing SymbTr MusicXML directory: ", source_root)

bind_fill <- function(xs) {
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  xs <- xs[vapply(xs, nrow, integer(1)) > 0]
  if (!length(xs)) return(data.frame())
  nm <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (z in setdiff(nm, names(x))) x[[z]] <- NA
    x[nm]
  })
  out <- do.call(rbind, xs); rownames(out) <- NULL; out
}

safe_mean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
safe_median <- function(x) if (any(is.finite(x))) median(x[is.finite(x)]) else NA_real_
score_id <- function(path) {
  rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", normalizePath(source_root, winslash = "/")), "/?"), "", normalizePath(path, winslash = "/"))
  gsub("[^A-Za-z0-9._-]+", "_", tools::file_path_sans_ext(rel))
}
has_lyrics <- function(path) {
  z <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  any(grepl("<lyric", z, fixed = TRUE))
}

all_files <- sort(list.files(source_root, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE))
if (!length(all_files)) stop("No XML files found.")
probe <- unique(as.integer(round(seq(1, length(all_files), length.out = min(700L, length(all_files))))))
candidates <- all_files[probe]
candidates <- candidates[vapply(candidates, has_lyrics, logical(1))]
if (length(candidates) < 120L) {
  extra <- setdiff(all_files, candidates)
  extra <- extra[vapply(extra, has_lyrics, logical(1))]
  candidates <- unique(c(candidates, extra))
}
candidates <- head(candidates, 220L)

target_n <- 30L
scores <- list(); used_paths <- character(); import_log <- list(); score_summary <- list()
events_all <- list(); syllables_all <- list(); notes_all <- list(); words_all <- list(); contours_all <- list()

for (path in candidates) {
  if (length(scores) >= target_n) break
  id <- score_id(path)
  warnings <- character()
  parsed <- withCallingHandlers(
    tryCatch(read_musicxml(path, melisma_policy = "hybrid", validate = TRUE), error = identity),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  if (inherits(parsed, "error")) {
    import_log[[length(import_log) + 1L]] <- data.frame(source_id=id, file=path, status="error",
      warning_count=length(unique(warnings)), message=conditionMessage(parsed))
    next
  }
  sy <- muse_table(parsed, "syllable")
  if (nrow(sy) < 20L) {
    import_log[[length(import_log) + 1L]] <- data.frame(source_id=id, file=path, status="skipped_too_small",
      warning_count=length(unique(warnings)), message=paste("Only", nrow(sy), "syllables"))
    next
  }
  problems <- validate_muse_score(parsed, strict = FALSE)
  if (nrow(problems)) stop("Validated parse returned problems for ", id)

  ev <- muse_table(parsed, "event"); nt <- muse_table(parsed, "note"); wd <- muse_table(parsed, "word")
  ct <- feature_contour(parsed); md <- feature_melisma_density(parsed); md_sum <- attr(md, "summary")
  for (xname in c("ev","sy","nt","wd","ct")) {
    x <- get(xname); x$source_id <- id; assign(xname, x)
  }
  fractional <- is.finite(ev$midi) & abs(ev$midi - round(ev$midi)) > 1e-9
  if (any(fractional)) {
    stopifnot(all(is.na(ev$pitch_class[fractional])))
    stopifnot(all(abs(ev$pitch_class_12[fractional] - (ev$midi[fractional] %% 12)) < 1e-9))
  }
  stopifnot(all(ev$score_offset_q + 1e-9 >= ev$score_onset_q, na.rm = TRUE))

  scores[[id]] <- parsed; used_paths[id] <- path
  events_all[[id]] <- ev; syllables_all[[id]] <- sy; notes_all[[id]] <- nt
  words_all[[id]] <- wd; contours_all[[id]] <- ct
  import_log[[length(import_log) + 1L]] <- data.frame(source_id=id, file=path, status="success",
    warning_count=length(unique(warnings)), message=paste(unique(warnings), collapse=" | "))
  score_summary[[id]] <- data.frame(
    source_id=id, title=parsed$meta$title, parts=nrow(parsed$parts), measures=nrow(parsed$measures),
    events=nrow(ev), sung_notes=sum(!ev$is_rest & !ev$is_grace), syllables=nrow(sy), words=nrow(wd),
    melisma_rate=md_sum$melisma_rate, mean_notes_per_syllable=md_sum$mean_notes,
    median_syllable_duration_q=safe_median(sy$dur_q),
    mean_abs_interval=safe_mean(ct$abs_interval), fractional_pitch_events=sum(fractional),
    fractional_pitch_rate=mean(fractional[is.finite(ev$midi)]), stringsAsFactors=FALSE)
}
if (length(scores) < 10L) stop("Too few scores parsed successfully: ", length(scores))

imports <- bind_fill(import_log); summaries <- bind_fill(score_summary)
events <- bind_fill(events_all); syllables <- bind_fill(syllables_all); notes <- bind_fill(notes_all)
words <- bind_fill(words_all); contours <- bind_fill(contours_all)

policy_rows <- list()
for (id in head(names(scores), 10L)) {
  for (policy in c("explicit", "hybrid", "legacy")) {
    sc <- read_musicxml(used_paths[[id]], melisma_policy = policy, validate = TRUE)
    f <- feature_melisma_density(sc); s <- attr(f, "summary")
    policy_rows[[length(policy_rows)+1L]] <- data.frame(source_id=id, policy=policy,
      syllables=s$n_syllables, melisma_rate=s$melisma_rate, mean_notes=s$mean_notes, max_notes=s$max_notes)
  }
}
policy <- bind_fill(policy_rows)

syllables$global_word <- paste(syllables$source_id, syllables$word_uid, sep="::")
word_sizes <- table(syllables$global_word)
ms <- syllables[word_sizes[syllables$global_word] >= 2L & is.finite(syllables$dur_q), , drop=FALSE]
contrasts <- lapply(split(ms, ms$global_word), function(x) {
  x <- x[order(x$syl_in_word), , drop=FALSE]
  if (nrow(x) < 2L) return(NULL)
  data.frame(source_id=x$source_id[1], global_word=x$global_word[1], word_text=paste(x$text, collapse=""),
    n_syllables=nrow(x), final_duration_q=x$dur_q[nrow(x)], nonfinal_mean_q=mean(x$dur_q[-nrow(x)]),
    contrast_q=x$dur_q[nrow(x)]-mean(x$dur_q[-nrow(x)]))
})
word_contrasts <- bind_fill(contrasts)
final_test <- muse_permutation(word_contrasts,
  statistic=function(x) mean(x$contrast_q),
  permute=function(x) { x$contrast_q <- x$contrast_q * sample(c(-1,1), nrow(x), replace=TRUE); x },
  n=4999, alternative="greater", seed=20260713)

beat_data <- notes[is.finite(notes$beat_strength) & !is.na(notes$melisma_pos), , drop=FALSE]
beat_data$is_syllable_onset <- beat_data$melisma_pos == 1L
beat_data <- beat_data[ave(as.integer(beat_data$is_syllable_onset), beat_data$source_id, FUN=function(z) length(unique(z))) > 1L, , drop=FALSE]
beat_stat <- function(x) mean(x$beat_strength[x$is_syllable_onset]) - mean(x$beat_strength[!x$is_syllable_onset])
beat_perm <- function(x) {
  key <- paste(x$source_id, x$part_id, x$voice, x$measure_index, sep="::")
  for (ii in split(seq_len(nrow(x)), key)) if (length(ii) > 1L) x$is_syllable_onset[ii] <- sample(x$is_syllable_onset[ii])
  x
}
beat_test <- muse_permutation(beat_data, beat_stat, beat_perm, n=4999, alternative="greater", seed=20260714)

fractional_events <- events[is.finite(events$midi) & abs(events$midi-round(events$midi)) > 1e-9, , drop=FALSE]
contour_summary <- as.data.frame(table(contours$contour, useNA="no"), stringsAsFactors=FALSE)
names(contour_summary) <- c("contour", "n")
contour_summary$proportion <- contour_summary$n / sum(contour_summary$n)

analysis_summary <- data.frame(
  analysis=c("word_final_duration", "syllable_onset_beat_strength", "microtonal_pitch_integrity"),
  n=c(nrow(word_contrasts), nrow(beat_data), nrow(fractional_events)),
  estimate=c(final_test$observed, beat_test$observed, if (nrow(fractional_events)) 1 else NA_real_),
  p_value=c(final_test$p_value, beat_test$p_value, NA_real_),
  unit=c("quarter notes", "metric-strength units", "proportion passing assertions"), stringsAsFactors=FALSE)

png(file.path(out_dir, "fig_score_melisma_rates.png"), 1200, 800, res=140)
hist(summaries$melisma_rate, breaks="FD", main="Melisma Rates Across Parsed SymbTr Scores",
     xlab="Proportion of syllables assigned to more than one note")
dev.off()
png(file.path(out_dir, "fig_word_final_contrasts.png"), 1200, 800, res=140)
hist(word_contrasts$contrast_q, breaks="FD", main="Within-word Final-Syllable Duration Contrasts",
     xlab="Final minus mean nonfinal duration (quarter notes)"); abline(v=0, lty=2); abline(v=mean(word_contrasts$contrast_q), lwd=2)
dev.off()
png(file.path(out_dir, "fig_policy_sensitivity.png"), 1200, 800, res=140)
boxplot(melisma_rate ~ policy, data=policy, main="Melisma Estimates by Alignment Policy", ylab="Melisma rate", xlab="Policy")
dev.off()
png(file.path(out_dir, "fig_fractional_alterations.png"), 1200, 800, res=140)
if (nrow(fractional_events)) hist(fractional_events$alter, breaks=20, main="Fractional Pitch Alterations Preserved by MUSE", xlab="MusicXML alter (semitones)") else plot.new()
dev.off()

write.csv(imports, file.path(out_dir, "import_log.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(summaries, file.path(out_dir, "score_summary.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(policy, file.path(out_dir, "melisma_policy_sensitivity.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(word_contrasts, file.path(out_dir, "word_final_duration_contrasts.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(beat_data, file.path(out_dir, "syllable_onset_beat_data.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(fractional_events, file.path(out_dir, "fractional_pitch_events.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(contour_summary, file.path(out_dir, "contour_summary.csv"), row.names=FALSE)
write.csv(analysis_summary, file.path(out_dir, "analysis_summary.csv"), row.names=FALSE)
saveRDS(scores, file.path(out_dir, "parsed_muse_scores.rds"), compress="xz")
saveRDS(list(final_test=final_test, beat_test=beat_test), file.path(out_dir, "permutation_results.rds"))

symbtr_commit <- Sys.getenv("SYMBTR_COMMIT", unset="unknown")
report <- c(
  "# MUSE 0.2.0: real-data validation on SymbTr MusicXML", "",
  paste0("- SymbTr commit: `", symbtr_commit, "`"),
  paste0("- Candidate files attempted: ", nrow(imports)),
  paste0("- Scores parsed and strictly validated: ", sum(imports$status == "success"), " / ", nrow(imports)),
  paste0("- Parsed events: ", nrow(events), "; syllables: ", nrow(syllables), "; words: ", nrow(words)),
  paste0("- Scores containing fractional MIDI pitches: ", sum(summaries$fractional_pitch_events > 0), " / ", nrow(summaries)),
  paste0("- Fractional pitch events preserved: ", nrow(fractional_events), " (all passed pitch-class integrity assertions)"), "",
  "## Analysis 1: word-final duration",
  paste0("Across ", nrow(word_contrasts), " multisyllabic words, the mean final-minus-nonfinal contrast was ",
         format(round(final_test$observed, 4), nsmall=4), " quarter notes; one-sided sign-flip permutation p = ", format(final_test$p_value, digits=4), "."), "",
  "## Analysis 2: metrical placement of syllable onsets",
  paste0("Across ", nrow(beat_data), " aligned notes, syllable onsets exceeded continuation notes by ",
         format(round(beat_test$observed, 4), nsmall=4), " metric-strength units; measure-stratified permutation p = ", format(beat_test$p_value, digits=4), "."), "",
  "## Alignment-policy sensitivity",
  paste0("Mean melisma rate (10-score subset): explicit = ", round(mean(policy$melisma_rate[policy$policy=="explicit"]),4),
         ", hybrid = ", round(mean(policy$melisma_rate[policy$policy=="hybrid"]),4),
         ", legacy = ", round(mean(policy$melisma_rate[policy$policy=="legacy"]),4), "."), "",
  "## Reproducibility",
  "The package was installed in a clean GitHub Actions R environment. Every retained score was parsed with `read_musicxml()`, checked with `validate_muse_score()`, flattened with `muse_table()`, summarized with package feature functions, and tested with `muse_permutation()`.", "",
  "## Session information", "```", capture.output(sessionInfo()), "```")
writeLines(report, file.path(out_dir, "REAL_DATA_REPORT.md"), useBytes=TRUE)
writeLines(c(paste0("MUSE_VERSION=", as.character(packageVersion("MUSE"))), paste0("SYMBTR_COMMIT=", symbtr_commit),
             paste0("RUN_UTC=", format(Sys.time(), tz="UTC", usetz=TRUE))), file.path(out_dir, "provenance.txt"))
cat(paste(report, collapse="\n"), "\n")
