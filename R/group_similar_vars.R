#' Auto‑group similar variable names by text similarity
#'
#' Uses string distances + hierarchical clustering and auto-selects a cut height
#' by maximizing mean silhouette. Returns a tibble with group ids, sizes, and
#' readable group labels. Optionally absorbs singleton items into a same‑stem
#' multi‑member cluster under conservative thresholds.
#'
#' @param x Character vector of variable names.
#' @param method Distance method for \code{stringdist::stringdistmatrix()} (default "jw").
#' @param normalize If TRUE, lowercase and collapse separators for robustness.
#' @param normalize_numbers If TRUE, replace digit runs with "#".
#' @param min_multisize Minimum size for at least one multi‑member cluster (default 2).
#' @param h_grid Optional numeric vector of candidate cut heights for the dendrogram.
#' @param block_prefix_len Compare only within blocks defined by the first N letters (default 3).
#' @param absorb_singletons_by_stem Try absorbing singletons into same‑stem clusters (default TRUE).
#' @param require_triple_underscores_for_absorb If TRUE, only absorb singletons whose original
#'   name contains "___" (default TRUE).
#' @param relax_factor Allow singleton’s mean distance up to this multiple of the candidate
#'   cluster’s internal cohesion (default 1.2).
#' @param abs_thresh Absolute mean‑distance threshold for absorption (default 0.33).
#'
#' @return A tibble with columns: var, group_id, group_label, group_label_first, group_size.
#'
#' @examples
#' vars <- c("gender","age","education","ethnicity","income","marital_status",
#'           "race___1","race___2","race___3","race___4","race___5","race___6","race___98")
#' group_similar_vars(vars)
#'
#' @importFrom stats as.dist hclust cutree quantile
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom dplyr group_by group_modify ungroup arrange first bind_rows
#' @importFrom tibble tibble
#' @importFrom cluster silhouette
#' @export
group_similar_vars <- function(
  x,
  method = "jw",
  normalize = TRUE,
  normalize_numbers = TRUE,
  min_multisize = 2,
  h_grid = NULL,
  block_prefix_len = 3,
  absorb_singletons_by_stem = TRUE,
  require_triple_underscores_for_absorb = TRUE,
  relax_factor = 1.2,
  abs_thresh = 0.33
) {
  stopifnot(is.character(x))
  n <- length(x)
  if (n <= 2L) {
    return(dplyr::tibble(
      var = x,
      group_id = seq_along(x),
      group_label = x,
      group_label_first = x,
      group_size = 1L
    ))
  }

  # --- helpers ---
  .norm <- function(s) {
    if (!normalize) return(s)
    s <- stringr::str_to_lower(s)
    if (normalize_numbers) s <- stringr::str_replace_all(s, "\\d+", "#")
    s <- stringr::str_replace_all(s, "[^a-z0-9]+", "_")
    s <- stringr::str_replace_all(s, "^_+|_+$", "")
    s
  }
  .letters_only <- function(s) gsub("[^a-z]", "", s)

  .lcp <- function(ss) {
    if (length(ss) == 1) return(ss)
    split <- strsplit(ss, "", fixed = TRUE)
    maxlen <- min(lengths(split))
    pref <- character(maxlen)
    for (i in seq_len(maxlen)) {
      ch <- vapply(split, `[[`, "", i)
      if (length(unique(ch)) > 1)
        return(if (i == 1) "" else paste0(pref[seq_len(i-1)], collapse = ""))
      pref[i] <- ch[1]
    }
    paste0(pref, collapse = "")
  }
  .medoid_idx <- function(idx, D) {
    if (length(idx) == 1) return(idx)
    subD <- as.matrix(D)[idx, idx, drop = FALSE]
    idx[which.min(rowMeans(subD))]
  }

  # --- normalize for distance (keep originals for output) ---
  x_norm <- .norm(x)

  # --- block by leading letters to avoid cross-talk ---
  block_key <- substr(.letters_only(x_norm), 1, block_prefix_len)
  block_key[block_key == ""] <- substr(x_norm[block_key == ""], 1, block_prefix_len)

  # --- cluster within each block with auto-tuned cut ---
  cluster_block <- function(ix) {
    xn <- x_norm[ix]
    if (length(ix) == 1L) {
      return(dplyr::tibble(var = x[ix], norm = xn, group_id = 1L))
    }
    D <- stats::as.dist(stringdist::stringdistmatrix(xn, xn, method = method))
    hc <- stats::hclust(D, method = "average")

    h_grid_loc <- if (is.null(h_grid)) {
      merges <- hc$height
      lo <- max(min(merges, na.rm = TRUE) - 1e-6, 0)
      hi <- min(max(merges, na.rm = TRUE) + 1e-6, 1)
      seq(lo, hi, length.out = 40L)
    } else h_grid

    score_cut <- function(h) {
      cl <- cutree(hc, h = h)
      k <- length(unique(cl))
      if (k <= 1L || k >= length(ix)) return(c(score = -Inf, k = k, h = h))
      sizes <- table(cl)
      if (!any(sizes >= min_multisize)) return(c(score = -Inf, k = k, h = h))
      s <- try({
        sil <- cluster::silhouette(cl, D)
        mean(sil[, "sil_width"])
      }, silent = TRUE)
      if (inherits(s, "try-error") || !is.finite(s)) s <- -Inf
      prop_single <- mean(sizes == 1L)
      c(score = s - 0.05 * prop_single, k = k, h = h)
    }

    cand <- vapply(h_grid_loc, score_cut, numeric(3L))
    best_idx <- which.max(cand["score", ])
    cl <- if (!is.finite(cand["score", best_idx])) {
      cutree(hc, k = min(3L, length(ix)))
    } else {
      cutree(hc, h = cand["h", best_idx])
    }

    dplyr::tibble(var = x[ix], norm = xn, group_id = as.integer(cl))
  }

  pieces <- split(seq_along(x), block_key, drop = TRUE)
  res_list <- lapply(pieces, cluster_block)

  # make group ids globally unique
  offset <- 0L
  for (i in seq_along(res_list)) {
    g_local <- res_list[[i]]$group_id
    res_list[[i]]$group_id <- g_local + offset
    offset <- offset + max(g_local)
  }
  df <- dplyr::bind_rows(res_list)

  # --- optional: conservative singleton absorb by stem + triple-underscore guard ---
if (absorb_singletons_by_stem) {
  Dfull <- stats::as.dist(stringdist::stringdistmatrix(df$norm, df$norm, method = method))
  Dm <- as.matrix(Dfull)

  df$stem <- sub("_.+$", "", df$norm)
  df$has_triple <- grepl("___", df$var, fixed = TRUE)

  grp_members <- split(seq_len(nrow(df)), df$group_id)
  grp_sizes   <- vapply(grp_members, length, integer(1))
  multi_ids   <- names(grp_members)[grp_sizes >= 2]

  # Cohesion per multi-member group: use a *looser* yardstick (q90 of off-diagonals)
  grp_intra_q90 <- setNames(rep(Inf, length(grp_members)), names(grp_members))
  for (g in multi_ids) {
    idx <- grp_members[[g]]
    off <- Dm[idx, idx, drop = FALSE]
    off <- off[off != 0]
    grp_intra_q90[g] <- if (length(off)) stats::quantile(off, 0.90, names = FALSE) else 0
  }

  # Majority stem per group + whether group contains any triple-underscore member
  grp_major_stem <- vapply(grp_members, function(ii) {
    names(sort(table(df$stem[ii]), decreasing = TRUE))[1]
  }, character(1))
  grp_has_triple <- vapply(grp_members, function(ii) any(df$has_triple[ii]), logical(1))

  # Singletons to consider
  is_singleton_row <- grp_sizes[match(df$group_id, names(grp_sizes))] == 1L

  for (i in which(is_singleton_row)) {
    # optional gate: only absorb if this singleton has triple underscores
    if (require_triple_underscores_for_absorb && !df$has_triple[i]) next

    st <- df$stem[i]
    # candidate groups: multi, same majority stem, and (optionally) have a triple-underscore member
    cand_groups <- names(grp_members)[grp_sizes >= 2 &
                                      grp_major_stem == st &
                                      (!require_triple_underscores_for_absorb | grp_has_triple)]
    if (length(cand_groups) == 0L) next

    # Evaluate fit to each candidate group
    cand_df <- do.call(rbind, lapply(cand_groups, function(g) {
      idx <- grp_members[[g]]
      data.frame(
        g           = as.integer(g),
        mean_to_grp = mean(Dm[i, idx]),
        intra_q90   = grp_intra_q90[[g]]
      )
    }))

    # Best candidate = smallest mean_to_grp
    cand_df <- cand_df[order(cand_df$mean_to_grp, cand_df$g), , drop = FALSE]
    best <- cand_df[1, ]

    # Accept if the singleton is not worse than (relax_factor * q90) OR clears abs_thresh
    # This avoids punishing very tight clusters.
    if (best$mean_to_grp <= max(relax_factor * best$intra_q90, abs_thresh)) {
      old_gid <- df$group_id[i]
      df$group_id[i] <- best$g
      grp_members[[as.character(best$g)]]  <- c(grp_members[[as.character(best$g)]], i)
      grp_members[[as.character(old_gid)]] <- setdiff(grp_members[[as.character(old_gid)]], i)
    }
  }
}

  # --- labels per final group ---
  add_labels <- function(sub) {
    if (nrow(sub) == 1L) {
      return(dplyr::tibble(
        var = sub$var,
        group_label = sub$var,
        group_label_first = dplyr::first(sub$var),
        group_size = 1L
      ))
    }
    D <- stats::as.dist(stringdist::stringdistmatrix(sub$norm, sub$norm, method = method))
    lcp <- .lcp(sub$norm)
    lab <- gsub("^_+|_+$", "", lcp)
    if (nchar(lab) < 3) {
      med <- sub$var[.medoid_idx(seq_len(nrow(sub)), D)]
      group_label <- med
    } else {
      group_label <- lab
    }
    dplyr::tibble(
      var = sub$var,
      group_label = group_label,
      group_label_first = dplyr::first(sub$var),
      group_size = nrow(sub)
    )
  }

  df |>
    dplyr::group_by(group_id) |>
    dplyr::group_modify(~ add_labels(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(group_id, var)
}


