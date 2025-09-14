library(dplyr)
library(tidyr)
library(maxent.ot)
library(stringr)
library(ggplot2)

options(scipen = 999)

lines <- read.csv("lines.csv", header = TRUE)

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

# adds lines_prose if you want a prose baseline, put the last line of the loop under comment and 
lines1 <- lines %>%
  mutate(enumerated_line = sapply(line, enumerate_positions)) %>%
  filter(corpus == "real")

# Stress->Strong: penalizes stressed syllables in W
lines1 <- lines1 %>%
  mutate(stress_strong = sapply(enumerated_line, function(x) {
    pattern <- "[rRfF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Heavy->Strong: penalizes heavies in W
lines1 <- lines1 %>%
  mutate(heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[URF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# High->Strong: penalizes Highs in W
lines1 <- lines1 %>%
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
lines1 <- lines1 %>%
  mutate(strong_stressed = sapply(enumerated_line, function(x) {
    pattern <- "[uU](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy: penalizes lights in S
lines1 <- lines1 %>%
  mutate(strong_heavy = sapply(enumerated_line, function(x) {
    pattern <- "[ufr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->High: penalizes toneless syllables in S
lines1 <- lines1 %>%
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
lines1 <- lines1 %>%
  mutate(stress_heavy_strong = sapply(enumerated_line, function(x) {
    pattern <- "[RF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(Strong->Heavy): penalizes stressed lights in S
lines1 <- lines1 %>%
  mutate(stress_strongheavy = sapply(enumerated_line, function(x) {
    pattern <- "[fr](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Stress->(High->Strong): penalizes stressed High-toned syllables in W
lines1 <- lines1 %>%
  mutate(stress_high_strong = sapply(enumerated_line, function(x) {
    pattern <- "[fF](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 0, na.rm = TRUE)
  }))

# Stress->(Strong->High): penalizes stressed toneless syllables in S
lines1 <- lines1 %>%
  mutate(stressed_stronghigh = sapply(enumerated_line, function(x) {
    pattern <- "[rR](\\d+)"
    matches <- str_extract_all(x, pattern)[[1]]
    if(length(matches) == 0) return(0)
    numbers <- as.numeric(str_extract(matches, "\\d+"))
    sum(numbers %% 2 == 1, na.rm = TRUE)
  }))

# Strong->Heavy_cadence: penalizes light syllables in the cadence
lines1 <- lines1 %>%
  mutate(strong_heavy_cadence = if_else((grepl("[urf]9", enumerated_line)), 1, 0))

# Heavy->Strong_cadence: penalizes heavies in W in cadence
lines1 <- lines1 %>%
  mutate(heavy_strong_cadence = if_else((grepl("[URF]10", enumerated_line)),1,0))

# Stressed->(Strong<->Heavy)_cadence: penalizes stressed lights in the cadence
lines1 <- lines1 %>%
  mutate(stressed_strongheavy_cadence = if_else((grepl("[rf]9", enumerated_line)), 1, 0))

lines1$In <- "X"

# converts the file into maxent.ot format
lines_maxent <- lines1 %>%
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
# base_idx: integer indices of the always-included columns
# pool_idx: integer indices of candidate constraints to consider adding
# criterion: "lrt", "aic", or "bic"
# alpha: significance threshold for LRT (see min_ic_gain for AIC/BIC-based selection)
# upper_bound, mu, sigma: passed to optimize_weights()
# max_steps: cap on number of constraints to add (not used in paper)
# retry_on_lrt_error: if TRUE, on LRT error refit both models with retry_* params
# retry_upper_bound, retry_sigma: enforced on retry refits
# min_ic_gain: for AIC/BIC selection, require at least this improvement per step (should be 2)
# verbose: print progress messages (useful for seeing selection history in real time)
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


# current setup
lines_maxent$intercept <- 0

# null model columns (only Input, Output, Candidate frequencies and Intercept)
base_idx <- c(1:3, 17)

# constraint pull (to consider)
pool_idx <- 4:16

# run LRT-based forward selection 
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
lines_maxent_new <- lines1 %>%
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
model_new$loglik
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



# prose comparison: frequencies of line types as a function of constraint violations (section 6.4)
lines_prose <- read.csv("lines prose.csv", header = TRUE)
lines_prose <- lines_prose %>%
  filter(corpus == "prose") %>%
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

# calculates line type frequencies
lines_prose %>%
  group_by(heavy_strong, stress_heavy_strong,stressed_strongheavy_cadence, heavy_strong_cadence,stress_high_strong) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count), proportion = count/total) %>%
  arrange(desc(proportion)) -> line_type_freq_prose




###############
# counting cumulativity in epic vs. prose (Section 6.5)
# data preparation
line_type_freq_prose <- line_type_freq_prose %>%
  mutate(N = count)

line_type_freq_prose$corpus <- "prose"

lines_maxent_new_new <- lines_maxent_new %>%
  ungroup() %>%
  mutate(total = sum(N), proportion = N/total)

lines_maxent_new_new$corpus <- "epic"

cumulativity <- rbind(lines_maxent_new_new[ , c(4:8, 10:11)],line_type_freq_prose[ , c(1:5, 8,10)])

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


#################
# Henriksson 2022-style analysis with Gaussian priors

line_type_freq_prose_maxent <- line_type_freq_prose %>%
  ungroup() %>%
  mutate(Out = row_number())

line_type_freq_prose_maxent$In <- "X"

line_type_freq_prose_maxent <- line_type_freq_prose_maxent %>%
  dplyr::select(In,Out,N,heavy_strong, stress_heavy_strong,stressed_strongheavy_cadence, heavy_strong_cadence,stress_high_strong)

model_prose <- optimize_weights(line_type_freq_prose_maxent[ , c(1:8)], mu = 0, sigma = 100, upper_bound = 10)
model_prose$weights

write.csv(line_type_freq_prose_maxent[ , c(1:8)], "prose check.csv")

# test 1:
write.csv(lines_maxent_new[ , c(1:8)], "epic check.csv")
lines_maxent_new_v1 <- lines_maxent_new
model_epic_with_prose_prior_hvy_s <- optimize_weights(lines_maxent_new_v1, mu = c(.76, 0, 0, 0, 0), sigma = c(.01, 100, 100, 100, 100))
model_epic_with_prose_prior_hvy_s$weights

compare_models(model_new, model_epic_with_prose_prior_hvy_s, method = "bic")

# test 2: 
model_epic_with_prose_prior_str_hvy_s <- optimize_weights(lines_maxent_new_v1, mu = c(0, .18, 0, 0, 0), sigma = c(100, .01, 100, 100, 100))
model_epic_with_prose_prior_str_hvy_s$weights

compare_models(model_new, model_epic_with_prose_prior_str_hvy_s, method = "bic")

# test 3: 
model_epic_with_prose_prior_str_s_hvy_cad <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, .77, 0, 0), sigma = c(100, 100, .01, 100, 100))
model_epic_with_prose_prior_str_s_hvy_cad$weights

compare_models(model_new, model_epic_with_prose_prior_str_s_hvy_cad, method = "bic")

# test 4: 
model_epic_with_prose_prior_hvy_s_cad <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, 0, .14, 0), sigma = c(100, 100, 100, .01, 100))
model_epic_with_prose_prior_hvy_s_cad$weights

compare_models(model_new, model_epic_with_prose_prior_hvy_s_cad, method = "bic")

# test 5: 
model_epic_with_prose_str_high_s <- optimize_weights(lines_maxent_new_v1, mu = c(0, 0, 0, 0, .84), sigma = c(100, 100, 100, 100, .01))
model_epic_with_prose_str_high_s$weights

compare_models(model_new, model_epic_with_prose_str_high_s, method = "bic")