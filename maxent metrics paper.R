library(dplyr)
library(tidyr)
library(maxent.ot)
library(stringr)
library(ggplot2)

options(scipen = 999)

lines <- read.csv("lines.csv", header = TRUE)  %>%
  filter(corpus == "real")
lines_prose <- read.csv("lines prose.csv", header = TRUE) %>%
  filter(corpus == "prose")

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
  
  return(paste(result, collapse = ""))
}

# adds position number
lines <- lines %>%
  mutate(enumerated_line = sapply(line, enumerate_positions)) 

# Stress->Strong: penalizes stressed syllables in W
lines <- lines %>%
  mutate(stress_strong = sapply(enumerated_line, function(x) {
    pattern <- "[rRfF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Heavy->Strong: penalizes heavies in W
lines <- lines %>%
  mutate(heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[URF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# High->Strong: penalizes Highs in W
lines <- lines %>%
  mutate(high_strong = sapply(enumerated_line, function(x) {
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
lines <- lines %>%
  mutate(strong_stressed = sapply(enumerated_line, function(x) {
    pattern <- "[uU](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy: penalizes lights in S
lines <- lines %>%
  mutate(strong_heavy = sapply(enumerated_line, function(x) {
    pattern <- "[ufr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->High: penalizes toneless syllables in S
lines <- lines %>%
  mutate(strong_high = sapply(enumerated_line, function(x) {
    # Extract all position numbers from the string
    all_positions <- as.numeric(str_extract_all(x, "\\d+")[[1]])
    
    if(length(all_positions) == 0) return(0)
    
    # Find odd positions
    odd_positions <- all_positions[all_positions %% 2 == 1]
    
    # Find High-toned positions
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
lines <- lines %>%
  mutate(stress_heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[RF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(Strong->Heavy): penalizes stressed lights in S
lines <- lines %>%
  mutate(stress_strongheavy = sapply(enumerated_line, function(x) {
    pattern <- "[fr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Stress->(High->Strong): penalizes stressed High-toned syllables in W
lines <- lines %>%
  mutate(stress_high_strong = sapply(enumerated_line, function(x) {
    pattern <- "[fF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(Strong->High): penalizes stressed toneless syllables in S
lines <- lines %>%
  mutate(stressed_stronghigh = sapply(enumerated_line, function(x) {
    pattern <- "[rR](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy_cadence: penalizes light syllables in the cadence
lines <- lines %>%
  mutate(strong_heavy_cadence = if_else((grepl("[urf]9", enumerated_line)), 1, 0))

# Heavy->Strong_cadence: penalizes heavies in W in cadence
lines <- lines %>%
  mutate(heavy_strong_cadence = if_else((grepl("[URF]10", enumerated_line)),1,0))

# Stressed->(Strong<->Heavy)_cadence: penalizes stressed lights in the cadence
lines <- lines %>%
  mutate(stressed_strongheavy_cadence = if_else((grepl("[rf]9", enumerated_line)), 1, 0))

# converts the file into maxent.ot format
lines_maxent <- lines %>%
  group_by(stress_strong, heavy_strong, high_strong, strong_stressed, strong_heavy, strong_high, stress_heavy_strong,
           stress_strongheavy, stress_high_strong, stressed_stronghigh, strong_heavy_cadence, heavy_strong_cadence,
           stressed_strongheavy_cadence) %>%
  summarise(N = n())

lines_maxent <- lines_maxent %>%
  ungroup() %>%
  mutate(Out = row_number())

lines_maxent$In = "X"

lines_maxent <- lines_maxent %>%
  dplyr::select(In,Out,N,stress_strong, heavy_strong, high_strong, strong_stressed, strong_heavy, strong_high, stress_heavy_strong,
                stress_strongheavy, stress_high_strong, stressed_stronghigh, strong_heavy_cadence, heavy_strong_cadence,
                stressed_strongheavy_cadence)

# Forward selection for maxent.ot (Section 6.3)
# df:input file
# base_idx: columns that are always included (In, Out, N and Intercept)
# pool_idx: columns of candidate constraints to consider adding
# criterion: "lrt", "aic", or "bic"
# alpha: significance threshold for LRT (see min_ic_gain for AIC/BIC-based selection)
# upper_bound, mu, sigma: passed to optimize_weights()
# max_steps: cap on number of constraints to add (not used in paper)
# retry_on_lrt_error: if TRUE, on LRT error refit both models with retry_* params
# retry_upper_bound, retry_sigma: enforced on retry refits
# min_ic_gain: for AIC/BIC selection, require at least this improvement for the added constraint (should be 2)
# verbose: print progress messages (useful for seeing and understanding selection history in real time)
forward_select_maxent <- function(df,
                                  base_idx,
                                  pool_idx,
                                  criterion = c("lrt","aic","bic"),
                                  alpha = 0.05,
                                  upper_bound = 10,
                                  mu = 0, sigma = 100, #default Gaussian prior
                                  max_steps = Inf,
                                  verbose = TRUE,
                                  retry_on_lrt_error = TRUE,
                                  retry_upper_bound = 10,
                                  retry_sigma = sigma,
                                  min_ic_gain = 2) {
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
  
  # compare with LRT, retrying with forced bounds if needed
  safe_compare_lrt <- function(m_small, m_large) {
    out <- try(compare_models(m_small, m_large, method = "lrt"), silent = TRUE)
    if (!inherits(out, "try-error")) {
      return(list(obj = out, retried = FALSE, m_small = m_small, m_large = m_large))
    }
    
    if (!retry_on_lrt_error) {
      return(list(error = attr(out, "condition"), retried = FALSE))
    }
    
    # refit both models with retry constraints
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
  
  # initialize base model
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
  log_tbl <- data.frame(step = integer(),
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
                        stringsAsFactors = FALSE)
  
  repeat {
    if (length(remaining_cols) == 0L) break
    if (step >= max_steps) break
    
    # evaluate each remaining candidate
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
          
          # if retry created new models, carry forward those fits/info
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
      
      list(idx = j,
           nm = colnames(df)[j],
           model = model_j,
           info = info_j,
           stat = stat,
           p = pval,
           score = score,
           delta_AIC = cur_info$AIC - info_j$AIC,
           delta_BIC = cur_info$BIC - info_j$BIC,
           retried = retried_flag)
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
    current_cols <- c(current_cols, best$idx)
    current_model <- best$model
    cur_info <- best$info
    remaining_cols <- setdiff(remaining_cols, best$idx)
    log_tbl <- rbind(log_tbl,
                     data.frame(step = step,
                                added = best$nm,
                                lrt_stat = ifelse(is.na(best$stat), NA_real_, best$stat),
                                p_value  = ifelse(is.na(best$p),   NA_real_, best$p),
                                delta_AIC = best$delta_AIC,
                                delta_BIC = best$delta_BIC,
                                ll = cur_info$ll,
                                k = cur_info$k,
                                AIC = cur_info$AIC,
                                BIC = cur_info$BIC,
                                retried = best$retried,
                                accepted = accept,
                                stringsAsFactors = FALSE))
    if (verbose) {
      if (criterion == "lrt") {
        message(sprintf("Step %d: +%s%s | LRT=%s, p=%s | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
                        step, best$nm, if (best$retried) " (refit with bound)" else "",
                        if (is.na(best$stat)) "NA" else sprintf("%.3f", best$stat),
                        if (is.na(best$p)) "NA" else format(best$p, digits = 3),
                        cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC))
      } else if (criterion == "aic") {
        message(sprintf("Step %d: +%s%s | \u0394AIC=%.3f | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
                        step, best$nm, if (best$retried) " (refit with bound)" else "",
                        best$delta_AIC, cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC))
      } else {
        message(sprintf("Step %d: +%s%s | \u0394BIC=%.3f | ll=%.4f, k=%d, AIC=%.2f, BIC=%.2f",
                        step, best$nm, if (best$retried) " (refit with bound)" else "",
                        best$delta_BIC, cur_info$ll, cur_info$k, cur_info$AIC, cur_info$BIC))
      }
    }
  }
  
  list(final_model = current_model,
       selected_indices = setdiff(current_cols, base_idx),
       selected_names = setdiff(colnames(df)[current_cols], colnames(df)[base_idx]),
       history = log_tbl)
}


# current setup
lines_maxent$intercept <- 0

# null model columns (only Input, Output, Candidate frequencies and Intercept)
base_idx <- c(1:3, 17)

# constraint pull (to consider)
pool_idx <- 4:16

# run AIC-based forward selection 
# can be time-consuming!
fs_aic <- forward_select_maxent(df = lines_maxent,
                                base_idx = base_idx,
                                pool_idx = pool_idx,
                                criterion = "aic",
                                min_ic_gain = 2,
                                upper_bound = 10, mu = 0, sigma = 100,
                                verbose = TRUE,
                                retry_on_lrt_error = TRUE,
                                retry_upper_bound = 10)

# run BIC-based forward selection
# can be time-consuming!
fs_bic <- forward_select_maxent(df = lines_maxent,
                                base_idx = base_idx,
                                pool_idx = pool_idx, 
                                criterion = "bic",
                                min_ic_gain = 2, # min_ic_gain must be set to 2 (else Delta_BIC < 2 considered an improvement)
                                upper_bound = 10, mu = 0, sigma = 100,
                                verbose = TRUE,
                                retry_on_lrt_error = TRUE,
                                retry_upper_bound = 10)

# run LRT-based forward selection with alpha = .05 (default threshold value)
# can be (excruciatingly) time-consuming!
fs_lrt <- forward_select_maxent(df = lines_maxent,
                                base_idx = base_idx,
                                pool_idx = pool_idx,
                                criterion = "lrt",
                                upper_bound = 10, mu = 0, sigma = 100,
                                verbose = TRUE,
                                retry_on_lrt_error = TRUE,
                                retry_upper_bound = 10)

# Which constraints got selected (crtierion: AIC)?
fs_aic$selected_names
# In what order were the constraints added (crtierion: AIC)?
fs_aic$history

# save selection history as a csv file
write.csv(fs_aic$history, "selection history aic.csv")


# Which constraints got selected (crtierion: BIC)?
fs_bic$selected_names
# In what order were the constraints added (crtierion: BIC)?
fs_bic$history

# save selection history as a csv file
write.csv(fs_bic$history, "selection history bic.csv")

# Which constraints got selected (crtierion: LRT)?
fs_lrt$selected_names
# In what order were the constraints added (crtierion: LRT)?
fs_lrt$history

# save selection history as a csv file
write.csv(fs_lrt$history, "selection history lrt.csv")

# fit final model using selected constraints as grouping factors (Section 6.3)
lines_maxent_new <- lines %>%
  group_by(heavy_strong, stress_heavy_strong, stress_high_strong, stressed_strongheavy_cadence, heavy_strong_cadence) %>%
  summarise(N = n())

lines_maxent_new <- lines_maxent_new %>%
  ungroup() %>%
  mutate(Out = row_number())

lines_maxent_new$In <- "X"

lines_maxent_new <- lines_maxent_new %>%
  dplyr::select(In,Out,N,heavy_strong, stress_heavy_strong,stressed_strongheavy_cadence, heavy_strong_cadence,stress_high_strong)

model_new <- optimize_weights(lines_maxent_new, mu = 0, sigma = 100)
model_new$weights
model_new_prob <- predict_probabilities(lines_maxent_new,model_new$weights)
model_new_performance <- model_new_prob$predictions

write.csv(model_new_performance, "oe final.csv")

# Scatterplot with obserbed vs. predicted probabilities (Section 6.3)
cor_test <- cor.test(model_new_performance$Observed,model_new_performance$Predicted)

r2 <- cor_test$estimate^2

model_new_performance %>%
  ggplot(aes(x = Observed, y = Predicted)) +
  geom_point(shape=21, fill=alpha("gray", 0.2), stroke=1, size=2) +
  geom_abline(slope=1, intercept=0, color="black", linetype = "dashed") +
  theme_bw() +
  xlim(c(0,.3)) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste0("r² = ", round(r2, 2)), hjust = -0.3, vjust = 3) -> op_plot

ggsave("op plot.png", op_plot, dpi = 600)

# prose comparison
#frequencies of line types as a function of constraint violations (section 6.4)
lines_prose <- lines_prose  %>%
  mutate(enumerated_line = sapply(line, enumerate_positions))

# adds constraint violations
# Heavy->Strong: penalizes heavies in W
lines_prose <- lines_prose %>%
  mutate(heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[URF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(High->Strong): penalizes stressed High-toned syllables in W
lines_prose <- lines_prose %>%
  mutate(stress_high_strong = sapply(enumerated_line, function(x) {
    pattern <- "[fF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))


# Stress->(Heavy->Strong): penalizes stressed heavies in W
lines_prose <- lines_prose %>%
  mutate(stress_heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[RF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Heavy->Strong_cadence: penalizes heavies in W in cadence
lines_prose <- lines_prose %>%
  mutate(heavy_strong_cadence = if_else((grepl("[URF]10", enumerated_line)),1,0))

# Stressed->(Strong<->Heavy)_cadence: penalizes stressed lights in the cadence
lines_prose <- lines_prose %>%
  mutate(stressed_strongheavy_cadence = if_else((grepl("[rf]9", enumerated_line)), 1, 0))

# calculates line type frequencies (Section 6.4)
lines_prose %>%
  group_by(heavy_strong, stress_heavy_strong,stressed_strongheavy_cadence, heavy_strong_cadence,stress_high_strong) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  mutate(total = sum(N), proportion = N/total) %>%
  arrange(desc(proportion)) -> line_type_freq_prose

# perfect, violation-free lines in epic vs. prose
lines_maxent_new %>%
  rowwise() %>%
  mutate(perfect = case_when(sum(heavy_strong,stress_heavy_strong,stressed_strongheavy_cadence,heavy_strong_cadence,stress_high_strong) == 0 ~ 1,
                             sum(heavy_strong,stress_heavy_strong,stressed_strongheavy_cadence,heavy_strong_cadence,stress_high_strong) > 0 ~ 0)) %>%
  group_by(perfect) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> perfect_lines_epic

line_type_freq_prose %>%
  rowwise() %>%
  mutate(perfect = case_when(sum(heavy_strong,stress_heavy_strong,stressed_strongheavy_cadence,heavy_strong_cadence,stress_high_strong) == 0 ~ 1,
                             sum(heavy_strong,stress_heavy_strong,stressed_strongheavy_cadence,heavy_strong_cadence,stress_high_strong) > 0 ~ 0)) %>%
  group_by(perfect) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> perfect_lines_prose

# Stress=>(Heavy=>Strong) violations in epic vs. prose
lines_maxent_new %>%
  mutate(stress_heavy_strong_violations = case_when(stress_heavy_strong < 1 ~ 0,
                                                    stress_heavy_strong >= 1 ~ 1)) %>%
  group_by(stress_heavy_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_heavy_strong_violations_epic

line_type_freq_prose %>%
  mutate(stress_heavy_strong_violations = case_when(stress_heavy_strong < 1 ~ 0,
                                                    stress_heavy_strong >= 1 ~ 1)) %>%
  group_by(stress_heavy_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_heavy_strong_violations_prose

stress_heavy_strong_matrix <- xtabs(cbind(stress_heavy_strong_violations_epic$n_lines, stress_heavy_strong_violations_prose$n_lines) ~ stress_heavy_strong_violations_epic$stress_heavy_strong_violations)

fisher.test(stress_heavy_strong_matrix)

# multiple violations of Stress=>(Heavy=>Strong) in epic vs. prose
lines_maxent_new %>%
  mutate(stress_heavy_strong_violations = case_when(stress_heavy_strong < 2 ~ 0,
                                                    stress_heavy_strong >= 2 ~ 1)) %>%
  group_by(stress_heavy_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_heavy_strong_multiple_violations_epic

line_type_freq_prose %>%
  mutate(stress_heavy_strong_violations = case_when(stress_heavy_strong < 2 ~ 0,
                                                    stress_heavy_strong >= 2 ~ 1)) %>%
  group_by(stress_heavy_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_heavy_strong_multiple_violations_prose

# Stress=>(High=>Strong) violations in epic vs. prose
lines_maxent_new %>%
  mutate(stress_high_strong_violations = case_when(stress_high_strong < 1 ~ 0,
                                                   stress_high_strong >= 1 ~ 1)) %>%
  group_by(stress_high_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_high_strong_violations_epic

line_type_freq_prose %>%
  mutate(stress_high_strong_violations = case_when(stress_high_strong < 1 ~ 0,
                                                    stress_high_strong >= 1 ~ 1)) %>%
  group_by(stress_high_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_high_strong_violations_prose

stress_high_strong_matrix <- xtabs(cbind(stress_high_strong_violations_epic$n_lines, stress_high_strong_violations_prose$n_lines) ~ stress_high_strong_violations_epic$stress_high_strong_violations)

fisher.test(stress_high_strong_matrix)

# multiple violations of Stress=>(Heavy=>Strong) in epic vs. prose
lines_maxent_new %>%
  mutate(stress_high_strong_violations = case_when(stress_high_strong < 2 ~ 0,
                                                    stress_high_strong >= 2 ~ 1)) %>%
  group_by(stress_high_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_high_strong_multiple_violations_epic

line_type_freq_prose %>%
  mutate(stress_high_strong_violations = case_when(stress_high_strong < 2 ~ 0,
                                                   stress_high_strong >= 2 ~ 1)) %>%
  group_by(stress_high_strong_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_high_strong_multiple_violations_prose

# Stress=>(Strong=>Heavy)_cadence violations in epic vs. prose
lines_maxent_new %>%
  mutate(stress_strongheavy_cadence_violations = if_else(stressed_strongheavy_cadence == 1, 1, 0)) %>%
  group_by(stress_strongheavy_cadence_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_strong_heavy_cadence_violations_epic

line_type_freq_prose %>%
  mutate(stress_strongheavy_cadence_violations = if_else(stressed_strongheavy_cadence == 1, 1, 0)) %>%
  group_by(stress_strongheavy_cadence_violations) %>%
  summarise(n_lines = sum(N)) %>%
  ungroup() %>%
  mutate(total = sum(n_lines), percentage = n_lines/total) -> stress_strong_heavy_cadence_violations_prose

stress_strong_heavy_cadence_matrix <- xtabs(cbind(stress_strong_heavy_cadence_violations_epic$n_lines, 
                                                  stress_strong_heavy_cadence_violations_prose$n_lines) ~ stress_strong_heavy_cadence_violations_epic$stress_strongheavy_cadence_violations)

fisher.test(stress_strong_heavy_cadence_matrix)

# Henriksson 2022-style analysis with Gaussian priors (Section 6.4)
line_type_freq_prose_maxent <- line_type_freq_prose %>%
  ungroup() %>%
  mutate(Out = row_number())

line_type_freq_prose_maxent$In <- "X"

line_type_freq_prose_maxent <- line_type_freq_prose_maxent %>%
  dplyr::select(In,Out,N,heavy_strong, stress_heavy_strong,stressed_strongheavy_cadence, heavy_strong_cadence,stress_high_strong)

model_prose <- optimize_weights(line_type_freq_prose_maxent[ , c(1:8)], mu = 0, sigma = 100, upper_bound = 10)
model_prose$weights

write.csv(line_type_freq_prose_maxent[ , c(1:8)], "prose check.csv")

write.csv(lines_maxent_new[ , c(1:8)], "epic check.csv")
lines_maxent_new_v1 <- lines_maxent_new

# test 1: Heavy=>S
model_epic_with_prose_prior_hvy_s <- optimize_weights(lines_maxent_new_v1, mu = c(.764, 0, 0, 0, 0), sigma = c(.01, 100, 100, 100, 100))
model_epic_with_prose_prior_hvy_s$weights

compare_models(model_new, model_epic_with_prose_prior_hvy_s, method = "bic")

# test 2: Stress=>(Heavy=>S)
model_epic_with_prose_prior_str_hvy_s <- optimize_weights(lines_maxent_new_v1, mu = c(0, .185, 0, 0, 0), sigma = c(100, .01, 100, 100, 100))
model_epic_with_prose_prior_str_hvy_s$weights

compare_models(model_new, model_epic_with_prose_prior_str_hvy_s, method = "bic")

# test 3: Stress=>(S=>Heavy)cadence
model_epic_with_prose_prior_str_s_hvy_cad <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, .768, 0, 0), sigma = c(100, 100, .01, 100, 100))
model_epic_with_prose_prior_str_s_hvy_cad$weights

compare_models(model_new, model_epic_with_prose_prior_str_s_hvy_cad, method = "bic")

# test 4: Heavy=>S(cadence)
model_epic_with_prose_prior_hvy_s_cad <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, 0, .14, 0), sigma = c(100, 100, 100, .01, 100))
model_epic_with_prose_prior_hvy_s_cad$weights

compare_models(model_new, model_epic_with_prose_prior_hvy_s_cad, method = "bic")

# test 5: Stress=>(High=>S)
model_epic_with_prose_str_high_s <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, 0, 0, .842), sigma = c(100, 100, 100, 100, .01))
model_epic_with_prose_str_high_s$weights

compare_models(model_new, model_epic_with_prose_str_high_s, method = "bic")

# counting cumulativity in epic vs. prose (Section 6.5)
# data preparation
line_type_freq_prose$corpus <- "prose"

lines_maxent_new_new <- lines_maxent_new %>%
  ungroup() %>%
  mutate(total = sum(N), proportion = N/total)

lines_maxent_new_new$corpus <- "epic"

cumulativity <- rbind(lines_maxent_new_new[ , c(4:8, 3, 10:11)],line_type_freq_prose[ , c(1:6, 8:9)])

#counting cumulativity plots
# Heavy=>Strong
cumulativity %>%
  group_by(heavy_strong,corpus) %>%
  summarise(proportion_overall = sum(proportion)) %>%
  ungroup() %>%
  complete(corpus, heavy_strong = 0:5, fill = list(proportion_overall = 0)) %>%
  rename(violations = heavy_strong) -> heavy_s_stats

heavy_s_stats %>%
  ggplot(aes(x = factor(violations),
             y = proportion_overall,
             group = corpus,
             linetype = corpus,
             shape = corpus)) +
  geom_point() +
  geom_line(linewidth = .3) +
  theme_bw() +
  labs(x = "Number of violations", y = "Line Frequency") +
  theme(legend.position = "bottom",
    legend.title = element_blank()) -> plot_heavy_strong_epic_prose

ggsave("heavy strong epic prose.png", plot_heavy_strong_epic_prose, dpi = 600)

# Stress=>(High=>S)
cumulativity %>%
  group_by(stress_high_strong,corpus) %>%
  summarise(proportion_overall = sum(proportion)) %>%
  ungroup() %>%
  complete(corpus, stress_high_strong = 0:5, fill = list(proportion_overall = 0)) %>%
  rename(violations = stress_high_strong) -> stress_high_s_stats
  
stress_high_s_stats%>%
  ggplot(aes(x = factor(violations),
             y = proportion_overall,
             group = corpus,
             linetype = corpus,
             shape = corpus)) +
  geom_point() +
  geom_line(linewidth = .3) +
  theme_bw() +
  labs(x = "Number of violations", y = "Line Frequency") +
  theme(legend.position = "bottom",
    legend.title = element_blank()) -> plot_stress_high_strong_epic_prose

ggsave("stress high strong epic prose.png", plot_stress_high_strong_epic_prose, dpi = 600)

# Stress=>(Heavy=>S)
cumulativity %>%
  group_by(stress_heavy_strong,corpus) %>%
  summarise(proportion_overall = sum(proportion)) %>%
  ungroup() %>%
  complete(corpus, stress_heavy_strong = 0:5, fill = list(proportion_overall = 0)) %>%
  rename(violations = stress_heavy_strong) -> stress_heavy_s_stats

stress_heavy_s_stats %>%
  ggplot(aes(x = factor(violations),
             y = proportion_overall,
             group = corpus,
             linetype = corpus,
             shape = corpus)) +
  geom_point() +
  geom_line(linewidth = .3) +
  theme_bw() +
  labs(x = "Number of violations", y = "Line Frequency") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) -> plot_stress_heavystrong_strong_epic_prose

ggsave("stress heavy strong epic prose.png", plot_stress_heavystrong_strong_epic_prose, dpi = 600)

# counting cumulativity:epic to prose ratio 
heavy_s_stats$constraint <- "heavy_strong"
stress_high_s_stats$constraint <- "stress_high_s"
stress_heavy_s_stats$constraint <- "stress_heavy_s"

count_cumulativity <- rbind(heavy_s_stats,stress_high_s_stats,stress_heavy_s_stats)

count_cumulativity %>%
  mutate(violations_collapsed = ifelse(violations >= 3, "3+", as.character(violations))) %>%
  group_by(corpus,constraint,violations_collapsed) %>%
  summarise(proportion_new = sum(proportion_overall)) %>%
  ungroup() %>%
  pivot_wider(names_from = corpus, values_from = proportion_new) %>%
  mutate(ratio_epic_prose = epic / prose) %>%
  ggplot(aes(x = violations_collapsed,
             y = ratio_epic_prose,
             group = factor(constraint,
                            levels = c("heavy_strong", "stress_high_s", "stress_heavy_s"),
                            labels = c("Hvy=>S", "Str=>(Hgh=>S)", "Str=>(Hvy=>S)")),
             linetype = factor(constraint,
                               levels = c("heavy_strong", "stress_high_s", "stress_heavy_s"),
                               labels = c("Hvy=>S", "Str=>(Hgh=>S)", "Str=>(Hvy=>S)")),
             shape = factor(constraint,
                            levels = c("heavy_strong", "stress_high_s", "stress_heavy_s"),
                            labels = c("Hvy=>S", "Str=>(Hgh=>S)", "Str=>(Hvy=>S)")))) +
  geom_point() +
  geom_line(linewidth = .3) +
  theme_bw() +
  labs(x = "Number of violations", y = "Epic to Prose Ratio") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_blank()) -> count_cumulativity_epic_prose_ratio
    
ggsave("epic prose counting cumulativity.png", count_cumulativity_epic_prose_ratio, dpi = 600)

# ganging-up cumulativity
# individual violation rates
cumulativity <- cumulativity %>%
  filter(corpus == "epic")

heavy_strong <- cumulativity %>%
  mutate(viol = if_else((heavy_strong >= 1), 1, 0)) %>%
  group_by(viol,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint = "hv_s") %>%
  filter(viol == 1)

stress_heavy_strong <- cumulativity %>%
  mutate(viol = if_else((stress_heavy_strong >= 1), 1, 0)) %>%
  group_by(viol,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint = "st_hv_s") %>%
  filter(viol == 1)

stress_high_strong <- cumulativity %>%
  mutate(viol = if_else((stress_high_strong >= 1), 1, 0)) %>%
  group_by(viol,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint = "st_hi_s") %>%
  filter(viol == 1)

stress_strong_heavy_cadence <- cumulativity %>%
  mutate(viol = if_else((stressed_strongheavy_cadence >= 1), 1, 0)) %>%
  group_by(viol,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint = "st_s_hv_c") %>%
  filter(viol == 1)

overall_violations <- rbind(heavy_strong, stress_heavy_strong, stress_high_strong,stress_strong_heavy_cadence)

# expected rates of coincident violations (by constraint pair)
expected_joint_violations <- overall_violations %>%
  dplyr::select(constraint,proportion,corpus) %>%
  pivot_wider(id_cols = corpus, names_from = "constraint", values_from = "proportion") %>%
  mutate(a_b = st_hv_s * st_hi_s,
         a_c = st_hv_s * st_s_hv_c,
         b_c = st_hi_s * st_s_hv_c,
         c_d = st_s_hv_c * hv_s,
         b_d = st_hi_s * hv_s) %>%
  dplyr::select(a_b, a_c, b_c, c_d, b_d) %>%
  pivot_longer(cols = c(a_b, a_c, b_c, c_d, b_d),
               names_to = "constraint_pair", 
               values_to = "proportion") %>%
  mutate(o_e = "expected") %>%
  dplyr::select(-corpus)

# observed rates of coincident violations (by constraint pair)
observed <- cumulativity %>%
  mutate(a_b = if_else((stress_heavy_strong == 1 & stress_high_strong == 1), 1, 0),
         a_c = if_else((stress_heavy_strong == 1 & stressed_strongheavy_cadence == 1), 1, 0),
         b_c = if_else((stress_high_strong == 1 & stressed_strongheavy_cadence == 1), 1, 0),
         c_d = if_else((stressed_strongheavy_cadence == 1 & heavy_strong == 1), 1, 0),
         b_d = if_else((stress_high_strong == 1 & heavy_strong == 1), 1, 0))

observed_ab <- observed %>%
  group_by(a_b,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint_pair = "a_b") %>%
  filter(a_b == 1) %>%
  dplyr::select(-a_b)

observed_ac <- observed %>%
  group_by(a_c,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint_pair = "a_c") %>%
  filter(a_c == 1) %>%
  dplyr::select(-a_c)

observed_bc <- observed %>%
  group_by(b_c,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint_pair = "b_c") %>%
  filter(b_c == 1) %>%
  dplyr::select(-b_c)

observed_cd <- observed %>%
  group_by(c_d,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint_pair = "c_d") %>%
  filter(c_d == 1) %>%
  dplyr::select(-c_d)

observed_bd <- observed %>%
  group_by(b_d,corpus) %>%
  summarise(viol_count = sum(N))%>%
  group_by(corpus) %>%
  mutate(total = sum(viol_count), proportion = viol_count/total) %>%
  mutate(lower = binom.wilson(viol_count, total)$lower,
         upper = binom.wilson(viol_count, total)$upper) %>%
  mutate(constraint_pair = "b_d") %>%
  filter(b_d == 1) %>%
  dplyr::select(-b_d)

observed_joint_violations <- rbind(observed_ab, observed_ac, observed_bc, observed_bd, observed_cd) %>%
  dplyr::select(corpus,constraint_pair, proportion) %>%
  mutate(o_e = "observed")

observed_expected <- rbind(expected_joint_violations, observed_joint_violations) %>%
  pivot_wider(id_cols = c(constraint_pair,corpus), names_from = "o_e", values_from = "proportion")

observed_expected <- observed_expected[ , -2]

# o/e ratios
observed_expected %>%
  rowwise() %>%
  mutate(oe_ratio = observed/expected) %>%
  arrange(constraint_pair)-> oe

# ganging-up cumulativity plot
# Stress=>(Heavy=>S) and Stress=>(High=>S)
overall_violations %>%
  filter(corpus == "epic") %>%
  filter(constraint == "st_hv_s" | constraint == "st_hi_s") -> st_hv_s_and_st_hi_s

st_hv_s_and_st_hi_s <- st_hv_s_and_st_hi_s[ , c(3:8)] %>%
  mutate(pair = "st_hv_s_and_st_hi_s") %>%
  mutate(violation = if_else((constraint == "st_hv_s"), "C1", "C2")) %>%
  dplyr::select(-constraint)

observed_ab %>%
  filter(corpus == "epic") %>%
  mutate(pair = "st_hv_s_and_st_hi_s") %>%
  mutate(violation = "C1&C2") -> conjoined_st_hv_s_and_st_hi_s

conjoined_st_hv_s_and_st_hi_s <- conjoined_st_hv_s_and_st_hi_s[ , c(2:6,8:9)]

st_hv_s_and_st_hi_s_all <- rbind(st_hv_s_and_st_hi_s,conjoined_st_hv_s_and_st_hi_s)

# Stress=>(Heavy=>S) and Stress=>(S=>Heavy)cad
overall_violations %>%
  filter(corpus == "epic") %>%
  filter(constraint == "st_hv_s" | constraint == "st_s_hv_c") -> st_hv_s_and_st_s_hv_c

st_hv_s_and_st_s_hv_c <- st_hv_s_and_st_s_hv_c[ , c(3:8)] %>%
  mutate(pair = "st_hv_s_and_st_s_hv_c") %>%
  mutate(violation = if_else((constraint == "st_hv_s"), "C1", "C2")) %>%
  dplyr::select(-constraint)

observed_ac %>%
  filter(corpus == "epic") %>%
  mutate(pair = "st_hv_s_and_st_s_hv_c") %>%
  mutate(violation = "C1&C2") -> conjoined_st_hv_s_and_st_s_hv_c

conjoined_st_hv_s_and_st_s_hv_c <- conjoined_st_hv_s_and_st_s_hv_c[ , c(2:6,8:9)]

st_hv_s_and_st_s_hv_c_all <- rbind(st_hv_s_and_st_s_hv_c,conjoined_st_hv_s_and_st_s_hv_c)

# Stress=>(High=>S) and Stress=>(S=>Heavy)cad
overall_violations %>%
  filter(corpus == "epic") %>%
  filter(constraint == "st_hi_s" | constraint == "st_s_hv_c") -> st_hi_s_and_st_s_hv_c

st_hi_s_and_st_s_hv_c <- st_hi_s_and_st_s_hv_c[ , c(3:8)] %>%
  mutate(pair = "st_hi_s_and_st_s_hv_c") %>%
  mutate(violation = if_else((constraint == "st_hi_s"), "C1", "C2")) %>%
  dplyr::select(-constraint)

observed_bc %>%
  filter(corpus == "epic") %>%
  mutate(pair = "st_hi_s_and_st_s_hv_c") %>%
  mutate(violation = "C1&C2") -> conjoined_st_hi_s_and_st_s_hv_c

conjoined_st_hi_s_and_st_s_hv_c <- conjoined_st_hi_s_and_st_s_hv_c[ , c(2:6,8:9)]

st_hi_s_and_st_s_hv_c_all <- rbind(st_hi_s_and_st_s_hv_c,conjoined_st_hi_s_and_st_s_hv_c)

# Stress=>(S=>Heavy)cad and Heavy=>S
overall_violations %>%
  filter(corpus == "epic") %>%
  filter(constraint == "st_s_hv_c" | constraint == "hv_s") -> st_s_hv_c_and_hv_s

st_s_hv_c_and_hv_s <- st_s_hv_c_and_hv_s[ , c(3:8)] %>%
  mutate(pair = "st_s_hv_c_and_hv_s") %>%
  mutate(violation = if_else((constraint == "st_s_hv_c"), "C1", "C2")) %>%
  dplyr::select(-constraint)

observed_cd %>%
  filter(corpus == "epic") %>%
  mutate(pair = "st_s_hv_c_and_hv_s") %>%
  mutate(violation = "C1&C2") -> conjoined_st_s_hv_c_and_hv_s

conjoined_st_s_hv_c_and_hv_s <- conjoined_st_s_hv_c_and_hv_s[ , c(2:6,8:9)]

st_s_hv_c_and_hv_s_all <- rbind(st_s_hv_c_and_hv_s,conjoined_st_s_hv_c_and_hv_s)

# Stress=>(S=>Heavy)cad and Heavy=>S
overall_violations %>%
  filter(corpus == "epic") %>%
  filter(constraint == "st_hi_s" | constraint == "hv_s") -> st_hi_s_and_hv_s

st_hi_s_and_hv_s <- st_hi_s_and_hv_s[ , c(3:8)] %>%
  mutate(pair = "st_hi_s_and_hv_s") %>%
  mutate(violation = if_else((constraint == "st_hi_s"), "C1", "C2")) %>%
  dplyr::select(-constraint)

observed_bd %>%
  filter(corpus == "epic") %>%
  mutate(pair = "st_hi_s_and_hv_s") %>%
  mutate(violation = "C1&C2") -> conjoined_st_hi_s_and_hv_s

conjoined_st_hi_s_and_hv_s <- conjoined_st_hi_s_and_hv_s[ , c(2:6,8:9)]

st_hi_s_and_hv_s_all <- rbind(st_hi_s_and_hv_s,conjoined_st_hi_s_and_hv_s)

# creates the plot
ganging_data <- rbind(st_hv_s_and_st_hi_s_all,st_hv_s_and_st_s_hv_c_all,st_hi_s_and_st_s_hv_c_all,
                      st_s_hv_c_and_hv_s_all,st_hi_s_and_hv_s_all)

labs_pair <- c(st_hv_s_and_st_hi_s   = "C1: Stress→(Heavy→S)\nC2: Stress→(High→S)",
               st_hv_s_and_st_s_hv_c = "C1: Stress→(Heavy→S)\nC2: Stress→(S→Heavy)cad",
               st_hi_s_and_st_s_hv_c = "C1: Stress→(High→S)\nC2: Stress→(S→Heavy)cad",
               st_s_hv_c_and_hv_s    = "C1: Stress→(S→Heavy)cad\nC2: Heavy→S",
               st_hi_s_and_hv_s = "C1: Stress→(High→S)\nC2: Heavy→S")

ganging_data %>%
  ggplot(aes(x = factor(violation,
                        levels = c("C1", "C2", "C1&C2")),
             y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "lightgray", width = .5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .3) +
  facet_wrap(~ pair, nrow = 2, labeller = as_labeller(labs_pair)) +
  theme_bw(base_size = 14) +
  labs(x = "Constraints violated", y = "Line Proportion") +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 1),
        strip.text = element_text(size = 11)) -> ganging_up_cumulativity_plot

ggsave("ganging plot.png", ganging_up_cumulativity_plot, dpi = 600)
