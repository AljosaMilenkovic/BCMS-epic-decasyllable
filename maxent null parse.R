library(dplyr)
library(maxent.ot)
library(stringr)

options(scipen = 999)

# function that enumerates position
enumerate_positions <- function(text) {
  chars <- unlist(strsplit(text, ""))
  letter_count <- 0
  result <- character(0)
  for (char in chars) {
    if (char == " ") {
      result <- c(result, char)
    } else {
      letter_count <- letter_count + 1
      result <- c(result, paste0(char, letter_count))
    }
  }
  paste(result, collapse = "")
}

lines_prose <- read.csv("lines prose.csv", header = TRUE) %>%
  filter(corpus != "scramble") %>%
  mutate(
    line = as.character(line),
    corpus = as.character(corpus),
    enumerated_line = sapply(line, enumerate_positions)
  )

lines_prose <- lines_prose %>%
  mutate(In = row_number()) %>%
  {
    bind_rows(
      .,
      mutate(., 
             line = "null_parse",
             enumerated_line = "null_parse")
    )
  } %>%
  arrange(In, enumerated_line == "null_parse")

# N based on enumerated_line
lines_prose <- lines_prose %>%
  mutate(
    N = case_when(
      corpus == "prose" & enumerated_line == "null_parse" ~ 1L,
      corpus == "prose" & enumerated_line != "null_parse" ~ 0L,
      corpus == "real"  & enumerated_line == "null_parse" ~ 0L,
      corpus == "real"  & enumerated_line != "null_parse" ~ 1L,
      TRUE ~ NA_integer_
    )
  )

lines_prose <- lines_prose %>%
  mutate(Out = enumerated_line) %>%
  dplyr::select(In,Out,N)

# violation coding
# Stress->Strong: penalizes stressed syllables in W
lines_prose <- lines_prose %>%
  mutate(stress_strong = sapply(Out, function(x) {
    pattern <- "[rRfF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Heavy->Strong: penalizes heavies in W
lines_prose <- lines_prose %>%
  mutate(heavy_strong = sapply(Out, function(x) {
    pattern <- "[URF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# High->Strong: penalizes Highs in W
lines_prose <- lines_prose %>%
  mutate(high_strong = sapply(Out, function(x) {
    # Pattern 1: [fF] in even positions
    fF_matches <- str_extract_all(x, "[fF](\\d+)")[[1]]
    fF_violations <- 0
    if(length(fF_matches) > 0) {
      fF_numbers <- as.numeric(str_extract(fF_matches, "\\d+"))
      fF_violations <- sum(fF_numbers %% 2 == 0, na.rm = TRUE)
    }
    
    # Pattern 2: [rR] in odd positions  
    rR_matches <- str_extract_all(x, "[rR](\\d+)")[[1]]
    rR_violations <- 0
    if(length(rR_matches) > 0) {
      rR_numbers <- as.numeric(str_extract(rR_matches, "\\d+"))
      rR_violations <- sum(rR_numbers %% 2 == 1, na.rm = TRUE)
    }
    
    # Total violations = both conditions
    return(fF_violations + rR_violations)
  }))

# Strong->Stress: penalizes unstressed syllables in S
lines_prose <- lines_prose %>%
  mutate(strong_stressed = sapply(Out, function(x) {
    pattern <- "[uU](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy: penalizes lights in S
lines_prose <- lines_prose %>%
  mutate(strong_heavy = sapply(Out, function(x) {
    pattern <- "[ufr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->High: penalizes toneless syllables in S
lines_prose <- lines_prose %>%
  mutate(strong_high = sapply(Out, function(x) {
    # Extract all position numbers from the string
    all_positions <- as.numeric(str_extract_all(x, "\\d+")[[1]])
    
    if(length(all_positions) == 0) return(0)
    
    # Find odd positions
    odd_positions <- all_positions[all_positions %% 2 == 1]
    
    # Find high-toned positions
    high_positions <- c()
    
    # Pattern 1: positions with [fF] are high-toned
    fF_matches <- str_extract_all(x, "[fF](\\d+)")[[1]]
    if(length(fF_matches) > 0) {
      fF_positions <- as.numeric(str_extract(fF_matches, "\\d+"))
      high_positions <- c(high_positions, fF_positions)
    }
    
    # Pattern 2: positions immediately following [rR] are high-toned
    rR_matches <- str_extract_all(x, "[rR](\\d+)")[[1]]
    if(length(rR_matches) > 0) {
      rR_positions <- as.numeric(str_extract(rR_matches, "\\d+"))
      # Positions immediately following rR (rR_position + 1)
      following_positions <- rR_positions + 1
      # Only include positions that actually exist in the line
      following_positions <- following_positions[following_positions %in% all_positions]
      high_positions <- c(high_positions, following_positions)
    }
    
    # Remove duplicates from high_positions
    high_positions <- unique(high_positions)
    
    # Count odd positions that are NOT high-toned
    violations <- sum(!odd_positions %in% high_positions)
    
    return(violations)
  }))


# Stress->(Heavy->Strong): penalizes stressed heavies in W
lines_prose <- lines_prose %>%
  mutate(stress_heavy_strong = sapply(Out, function(x) {
    pattern <- "[RF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Heavy->(Strong->Stress): penalizes unstressed heavies in S: IMPOSSIBLE IN RYAN 2017!!!
#lines_prose <- lines_prose %>%
#  mutate(heavy_strong_stressed = sapply(Out, function(x) {
#    pattern <- "[U](\\d+)"
#    matches <- str_extract_all(x, pattern)[[1]]
#    if(length(matches) == 0) return(0)
#    numbers <- as.numeric(str_extract(matches, "\\d+"))
#    sum(numbers %% 2 == 1, na.rm = TRUE)
#  }))

# Stress->(Strong->Heavy): penalizes stressed lights in S
lines_prose <- lines_prose %>%
  mutate(stress_strongheavy = sapply(Out, function(x) {
    pattern <- "[fr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Stress->(High->Strong): penalizes stressed High-toned syllables in W
lines_prose <- lines_prose %>%
  mutate(stress_high_strong = sapply(Out, function(x) {
    pattern <- "[fF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(Strong->High): penalizes stressed toneless syllables in S
lines_prose <- lines_prose %>%
  mutate(stressed_stronghigh = sapply(Out, function(x) {
    pattern <- "[rR](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy_cadence: penalizes light syllables in the cadence
lines_prose <- lines_prose %>%
  mutate(strong_heavy_cadence = if_else((grepl("[urf]9", Out)), 1, 0))

# Heavy->Strong_cadence: penalizes heavies in W in cadence
lines_prose <- lines_prose %>%
  mutate(heavy_strong_cadence = if_else((grepl("[URF]10", Out)),1,0))

# Stressed->(Strong<->Heavy)_cadence: penalizes stressed lights in the cadence
lines_prose <- lines_prose %>%
  mutate(stressed_strongheavy_cadence = if_else((grepl("[rf]9", Out)), 1, 0))

# MParse: penalizes null parse
lines_prose <- lines_prose %>%
  mutate(MParse = if_else((Out == "null_parse"), 1, 0))

# fits maximal model
model_mparse_max <- optimize_weights(lines_prose, mu = 0, sigma = 100)
model_mparse_max$weights
model_mparse_max$loglik
AIC_model_max <- 2*model_mparse_max$k-2*model_mparse_max$loglik
BIC_model_max <- model_mparse_max$k*log(model_mparse_max$n)-2*model_mparse_max$loglik

# ---- Forward selection for maxent.ot ----
# df: your constraint tableau (like lines_maxent)
# base_idx: integer indices of the always-included columns (e.g., c(1:3, 14))
# pool_idx: integer indices of candidate constraints to add (e.g., 4:13)
# criterion: "lrt" (default), or "aic", or "bic"
# alpha: significance threshold for LRT-based forward selection
# upper_bound, mu, sigma: passed to optimize_weights()
# max_steps: cap how many constraints to add
# verbose: print progress

# ---- Forward selection for maxent.ot (robust, with retry & IC threshold) ----
# df: constraint tableau (e.g., lines_maxent)
# base_idx: integer indices of columns always included (e.g., c(1:3, 14))
# pool_idx: integer indices of candidate constraints to add (e.g., 4:13)
# criterion: "lrt" (likelihood ratio test), "aic", or "bic"
# alpha: significance level for LRT (only used if criterion = "lrt")
# upper_bound, mu, sigma: passed to optimize_weights() for normal fits
# max_steps: cap on number of constraints to add
# retry_on_lrt_error: if TRUE, on LRT error refit both models with retry_* params
# retry_upper_bound, retry_sigma: enforced on retry refits
# min_ic_gain: for AIC/BIC selection, require at least this improvement per step
# verbose: print progress messages
forward_select_maxent <- function(
    df,
    base_idx,
    pool_idx,
    criterion = c("lrt","aic","bic"),
    alpha = 0.05,
    upper_bound = 10,
    mu = 0,
    sigma = 100,
    max_steps = Inf,
    verbose = TRUE,
    retry_on_lrt_error = TRUE,
    retry_upper_bound  = 10,
    retry_sigma        = sigma,
    min_ic_gain        = 2
) {
  criterion <- match.arg(criterion)
  
  # --- helpers ---
  fit_model <- function(col_idx, ub = upper_bound, sg = sigma) {
    m <- optimize_weights(df[, col_idx, drop = FALSE],
                          upper_bound = ub, mu = mu, sigma = sg)
    attr(m, "cols") <- col_idx
    m
  }
  
  model_info <- function(model) {
    ll  <- model$loglik
    k   <- model$k
    n   <- nrow(df)
    aic <- -2 * ll + 2 * k
    bic <- -2 * ll + log(n) * k
    list(ll = ll, k = k, n = n, AIC = aic, BIC = bic)
  }
  
  # Compare with LRT, retrying with forced bounds if needed
  safe_compare_lrt <- function(m_small, m_large) {
    out <- try(compare_models(m_small, m_large, method = "lrt"), silent = TRUE)
    if (!inherits(out, "try-error")) {
      return(list(obj = out, retried = FALSE, m_small = m_small, m_large = m_large))
    }
    
    if (!retry_on_lrt_error) {
      return(list(error = attr(out, "condition"), retried = FALSE))
    }
    
    # Refit both models with retry constraints
    cols_small <- attr(m_small, "cols")
    cols_large <- attr(m_large, "cols")
    
    m_small2 <- try(fit_model(cols_small, ub = retry_upper_bound, sg = retry_sigma), silent = TRUE)
    m_large2 <- try(fit_model(cols_large, ub = retry_upper_bound, sg = retry_sigma), silent = TRUE)
    if (inherits(m_small2, "try-error") || inherits(m_large2, "try-error")) {
      return(list(error = "refit-failed", retried = TRUE))
    }
    
    out2 <- try(compare_models(m_small2, m_large2, method = "lrt"), silent = TRUE)
    if (!inherits(out2, "try-error")) {
      return(list(obj = out2, retried = TRUE, m_small = m_small2, m_large = m_large2))
    }
    
    list(error = attr(out2, "condition"), retried = TRUE)
  }
  
  # --- initialize base model ---
  current_cols   <- base_idx
  remaining_cols <- setdiff(pool_idx, base_idx)
  step <- 0L
  
  current_model <- fit_model(current_cols)
  cur_info <- model_info(current_model)
  
  if (verbose) {
    message(sprintf(
      "Start: ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
      cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC
    ))
  }
  
  # history log
  log_tbl <- data.frame(
    step = integer(),
    added = character(),
    lrt_stat = numeric(),
    p_value = numeric(),
    delta_AIC = numeric(),
    delta_BIC = numeric(),
    ll = numeric(),
    k = integer(),
    AIC = numeric(),
    BIC = numeric(),
    retried = logical(),
    accepted = logical(),
    stringsAsFactors = FALSE
  )
  
  repeat {
    if (length(remaining_cols) == 0L) break
    if (step >= max_steps) break
    
    # Evaluate each remaining candidate
    candidates <- lapply(remaining_cols, function(j) {
      cols_j  <- c(current_cols, j)
      model_j <- fit_model(cols_j)
      info_j  <- model_info(model_j)
      
      retried_flag <- FALSE
      stat <- NA_real_
      pval <- NA_real_
      # score: larger is better in our selection
      if (criterion == "lrt") {
        cmp <- safe_compare_lrt(current_model, model_j)
        if (!is.null(cmp$obj)) {
          obj <- cmp$obj
          stat <- if (!is.null(obj$statistic)) obj$statistic else
            if (!is.null(obj$LRT))       obj$LRT       else NA_real_
          pval <- if (!is.null(obj$p.value))  obj$p.value  else
            if (!is.null(obj$p))         obj$p        else NA_real_
          retried_flag <- isTRUE(cmp$retried)
          
          # If retry created new models, carry forward those fits/info
          if (retried_flag) {
            current_model <- cmp$m_small
            cur_info      <- model_info(current_model)
            model_j       <- cmp$m_large
            info_j        <- model_info(model_j)
          }
          score <- ifelse(is.na(pval), -Inf, -pval)  # smaller p -> bigger score
        } else {
          # LRT failed even after retry: skip
          score <- -Inf
          retried_flag <- isTRUE(cmp$retried)
          if (verbose) {
            message(sprintf("  Skipping %s: LRT failed (%s).",
                            colnames(df)[j],
                            if (is.character(cmp$error)) cmp$error else "compare_models error"))
          }
        }
      } else if (criterion == "aic") {
        score <- cur_info$AIC - info_j$AIC   # positive = improvement
      } else { # "bic"
        score <- cur_info$BIC - info_j$BIC
      }
      
      list(
        idx = j,
        nm = colnames(df)[j],
        model = model_j,
        info = info_j,
        stat = stat,
        p = pval,
        score = score,
        delta_AIC = cur_info$AIC - info_j$AIC,
        delta_BIC = cur_info$BIC - info_j$BIC,
        retried = retried_flag
      )
    })
    
    # pick best candidate by score
    scores <- vapply(candidates, function(x) x$score, numeric(1))
    best_i <- which.max(scores)
    best   <- candidates[[best_i]]
    
    # stopping rules
    accept <- FALSE
    if (criterion == "lrt") {
      accept <- !is.na(best$p) && (best$p < alpha)
      if (!accept) {
        if (verbose) message("Stop: no candidate passes LRT threshold.")
        break
      }
    } else if (criterion == "aic") {
      accept <- !is.na(best$delta_AIC) && (best$delta_AIC >= min_ic_gain)
      if (!accept) {
        if (verbose) message(sprintf("Stop: no AIC improvement \u2265 %.2f.", min_ic_gain))
        break
      }
    } else { # "bic"
      accept <- !is.na(best$delta_BIC) && (best$delta_BIC >= min_ic_gain)
      if (!accept) {
        if (verbose) message(sprintf("Stop: no BIC improvement \u2265 %.2f.", min_ic_gain))
        break
      }
    }
    
    # accept best and update state
    step <- step + 1L
    current_cols   <- c(current_cols, best$idx)
    current_model  <- best$model
    cur_info       <- best$info
    remaining_cols <- setdiff(remaining_cols, best$idx)
    
    log_tbl <- rbind(
      log_tbl,
      data.frame(
        step = step,
        added = best$nm,
        lrt_stat = ifelse(is.na(best$stat), NA_real_, best$stat),
        p_value  = ifelse(is.na(best$p),   NA_real_, best$p),
        delta_AIC = best$delta_AIC,
        delta_BIC = best$delta_BIC,
        ll = cur_info$ll, k = cur_info$k,
        AIC = cur_info$AIC, BIC = cur_info$BIC,
        retried = best$retried,
        accepted = accept,
        stringsAsFactors = FALSE
      )
    )
    
    if (verbose) {
      if (criterion == "lrt") {
        message(sprintf(
          "Step %d: +%s%s | LRT=%s, p=%s | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
          step, best$nm, if (best$retried) " (refit with bound)" else "",
          if (is.na(best$stat)) "NA" else sprintf("%.3f", best$stat),
          if (is.na(best$p)) "NA" else format(best$p, digits = 3),
          cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC
        ))
      } else if (criterion == "aic") {
        message(sprintf(
          "Step %d: +%s%s | \u0394AIC=%.3f | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
          step, best$nm, if (best$retried) " (refit with bound)" else "",
          best$delta_AIC, cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC
        ))
      } else {
        message(sprintf(
          "Step %d: +%s%s | \u0394BIC=%.3f | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
          step, best$nm, if (best$retried) " (refit with bound)" else "",
          best$delta_BIC, cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC
        ))
      }
    }
  }
  
  list(
    final_model      = current_model,
    selected_indices = setdiff(current_cols, base_idx),
    selected_names   = setdiff(colnames(df)[current_cols], colnames(df)[base_idx]),
    history          = log_tbl
  )
}


# Your setup
lines_prose$intercept <- 0

# base (null) model columns: 1:3 plus the intercept you just added
base_idx <- c(1:3, 18)

# pool of constraints to consider
pool_idx <- 4:17

# run AIC-based forward selection
fs_aic <- forward_select_maxent(
  df = lines_prose,
  base_idx = base_idx,
  pool_idx = pool_idx,
  criterion = "aic",
  min_ic_gain = 2,
  upper_bound = 10, mu = 0, sigma = 100,
  verbose = TRUE,
  retry_on_lrt_error = TRUE,
  retry_upper_bound = 10
)

fs_bic <- forward_select_maxent(
  df = lines_prose,
  base_idx = base_idx,
  pool_idx = pool_idx,
  criterion = "bic",
  min_ic_gain = 2,
  upper_bound = 10, mu = 0, sigma = 100,
  verbose = TRUE,
  retry_on_lrt_error = TRUE,
  retry_upper_bound = 10
)

fs_lrt <- forward_select_maxent(
  df = lines_prose,
  base_idx = base_idx,
  pool_idx = pool_idx,
  criterion = "lrt",
  upper_bound = 10, mu = 0, sigma = 100,
  verbose = TRUE,
  retry_on_lrt_error = TRUE,
  retry_upper_bound = 10
)

# What got selected?
fs_aic$selected_names
fs_aic$history
fs_aic$final_model

write.csv(fs_aic$history, "selection history aic null parse.csv")
write.csv(fs_aic$final_model$weights, "weights final model null parse.csv")

fs_bic$selected_names
fs_bic$history
fs_bic$final_model

write.csv(fs_bic$history, "selection history bic.csv")

fs_lrt$selected_names
fs_lrt$history
fs_lrt$final_model

write.csv(fs_lrt$history, "selection history lrt.csv")