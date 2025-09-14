library(dplyr)
library(tidyr)
library(stringr)
library(lme4)
library(MASS)
library(emmeans)
library(ggplot2)
library(binom)

prose <- read.table("prose_with_rigged.txt", header = TRUE)
epic <- read.table("serbian_with_rigged.txt", header = TRUE)


### master file prep

# adds corpus column
prose$corpus <- "prose"
epic$corpus <- "epic"

# adds position strength variable ("1" for odd/strong, "0" for even/weak)
prose$position <- as.numeric(prose$position)
prose <- prose %>%
  mutate(odd = if_else((position %% 2 == 1), "1", "0"))

#adds accent type and vowel length in separate columns
prose <- prose %>%
  mutate(accent_type = if_else((tone == "f"), "2",
                               if_else((tone == "r"), "1", "0"))) %>%
  mutate(weight = as.factor(is_heavy))

#adds position strength variable ("1" for odd/strong, "0" for even/weak)
epic$position <- as.numeric(epic$position)
epic <- epic %>%
  mutate(odd = if_else((position %% 2 == 1), "1", "0"))

#adds accent type and vowel length in separate columns
epic <- epic %>%
  mutate(accent_type = if_else((tone == "f"), "2",
                               if_else((tone == "r"), "1", "0"))) %>%
  mutate(weight = as.factor(is_heavy))

# creates master file
data <- rbind(epic,prose)

# by-position proportion of accented syllables (section 2)
data %>%
  mutate(accented = if_else((accent_type == "0"), 0, 1)) %>%
  group_by(position, accented) %>%
  summarise(count = n()) %>%
  group_by(position) %>%
  mutate(total = sum(count), proportion = count/total) %>%
  filter(accented == 1) -> by_position_accentedness_rate


# wug plot (section 3.4)
chance_level <- 5/8

data %>%
  filter(!(position == "4" | position == "10")) %>%
  filter(corpus == "epic" & real == 1) %>%
  group_by(accent_type, weight, odd) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(accent_type, weight) %>%
  mutate(total = sum(count),
         percentage = count / total) %>%
  mutate(lower = binom.wilson(count, total)$lower,
         upper = binom.wilson(count, total)$upper) %>%
  filter(odd == "1") %>%
  ggplot(aes(x = factor(accent_type,
                        levels = c("2", "1", "0"),
                        labels = c("falling", "rising", "unaccented")),
             y = percentage,
             group = factor(weight,
                            levels = c("1", "0"),
                            labels = c("heavy", "light")),
             shape = factor(weight,
                            levels = c("1", "0"),
                            labels = c("heavy", "light")),
             linetype = factor(weight,
                               levels = c("1", "0"),
                               labels = c("heavy", "light")))) +
  geom_point(size = 2) +
  geom_line(linewidth = .5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, linetype = "solid")+
  geom_hline(yintercept = chance_level, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(y = "% in Odd Positions",
       x = "Accent Type",
       shape = "Weight",
       linetype = "Weight") +
  theme_bw() +
  theme(legend.position = "bottom") -> plot_accent_type_length

ggsave("wug plot epic.png", plot_accent_type_length, dpi = 600)


# regression analysis for the epic corpus (section 3.4)

# epic model with positions 1:3 and 5:9 (no hemistich-final positions)
epic_nonfin <- epic %>%
  filter(real == 1) %>%
  filter(!(position == 4 | position == 10))

# difference coding
epic_nonfin <- epic_nonfin %>%
  mutate(accent_type = as.factor(accent_type))
contrasts(epic_nonfin$accent_type) = contr.sdif(3)

# word shape recoding
epic_nonfin <- epic_nonfin %>%
  mutate(shape2 = str_replace_all(shape2, "[fr]", "u")) %>%
  mutate(shape2 = str_replace_all(shape2, "[FR]", "U"))

# runs the model
model_epic_nonfin <- glmer(as.factor(odd)  ~ accent_type*weight + (1 | shape2), data = epic_nonfin, family = "binomial")
summary(model_epic_nonfin)

# post hos pairwise tests
contrast(emmeans(model_epic_nonfin, ~ accent_type*weight), "tukey")



# proportion of syllables in odd positions  by Accent Type and Syllable Weight (Section 4.2.1)
chance_level <- 5/8

data %>%
  filter(!(position == "4" | position == "10")) %>%
  filter(real == 1) %>%
  group_by(accent_type, weight, odd, corpus) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(accent_type, weight, corpus) %>%
  mutate(total = sum(count),
         percentage = count / total) %>%
  mutate(lower = binom.wilson(count, total)$lower,
         upper = binom.wilson(count, total)$upper) %>%
  filter(odd == "1") %>%
  ggplot(aes(x = factor(accent_type,
                        levels = c("2", "1", "0"),
                        labels = c("falling", "rising", "unaccented")),
             y = percentage,
             group = factor(weight,
                            levels = c("1", "0"),
                            labels = c("heavy", "light")),
             shape = factor(weight,
                            levels = c("1", "0"),
                            labels = c("heavy", "light")),
             linetype = factor(weight,
                               levels = c("1", "0"),
                               labels = c("heavy", "light")))) +
  geom_point(size = 2) +
  geom_line(linewidth = .5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, linetype = "solid")+
  geom_hline(yintercept = chance_level, linetype = "dashed", color = "red", linewidth = 0.5) +
  facet_wrap(~ factor(corpus,
                      levels = c("epic", "prose"),
                      labels = c("Epic", "Prose"))) +
  labs(y = "% in Odd Positions",
       x = "Accent Type",
       shape = "Weight",
       linetype = "Weight") +
  theme_bw() +
  theme(legend.position = "bottom") -> plot_accent_type_length_epic_vs_prose

ggsave("general plot epic vs prose.png", plot_accent_type_length_epic_vs_prose, dpi = 600)

# prose model (section 4.2.1)
# filters out hemistich final syllables
prose_nonfin <- prose %>%
  filter(real == 1) %>%
  filter(!(position == 4 | position == 10))

# difference coding
prose_nonfin <- prose_nonfin %>%
  mutate(accent_type = as.factor(accent_type))
contrasts(prose_nonfin$accent_type) = contr.sdif(3)

# word shape recoding
prose_nonfin <- prose_nonfin %>%
  mutate(shape2 = str_replace_all(shape2, "[fr]", "u")) %>%
  mutate(shape2 = str_replace_all(shape2, "[FR]", "U"))


# run the model with shape2 as the random effect
model_prose <- glmer(as.factor(odd)  ~ accent_type*weight + (1 | shape2), data = prose_nonfin, family = "binomial")
summary(model_prose)

# post-hoc pairwise tests
contrast(emmeans(model_prose, ~ accent_type*weight), "tukey")


# epic model with positions 1:3 and 5:8 (without position 9) (section 4.2.2)
epic_regression <- epic %>%
  filter(real == 1) %>%
  filter(!(position == 4 | position == 9 | position == 10))

# difference coding
epic_regression <- epic_regression %>%
  mutate(accent_type = as.factor(accent_type))
contrasts(epic_regression$accent_type) = contr.sdif(3)

# word shape recoding
epic_regression <- epic_regression %>%
  mutate(shape2 = str_replace_all(shape2, "[fr]", "u")) %>%
  mutate(shape2 = str_replace_all(shape2, "[FR]", "U"))

model_epic_without_9 <- glmer(as.factor(odd)  ~ accent_type*weight + (1 | shape2), data = epic_regression, family = "binomial")
summary(model_epic_without_9)


# weight in stressed syllables (section 4.2.2)
data %>%
  filter(!real == 0) %>%
  filter(!accent_type == 0) %>%
  group_by(corpus,position,odd, weight) %>%
  summarise(count = n()) %>%
  group_by(corpus,position,odd) %>%
  mutate(total = sum(count), proportion = count/total) %>%
  mutate(lower = binom.wilson(count, total)$lower,
         upper = binom.wilson(count, total)$upper) %>%
  ungroup() %>%
  filter(!(position == 4 | position == 10)) %>%
  complete(corpus, position = 1:10, odd, 
           fill = list(count = 0, total = 0, proportion = NA, lower = NA, upper = NA)) %>%
  filter(!weight == 0) -> prop_heavy

prop_heavy %>%
  ggplot(aes(x = factor(position, levels = 1:10), 
             y = proportion, 
             fill = factor(odd, levels = c(1,0), labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = .7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  scale_fill_grey(start = .4, end = .8) +
  scale_x_discrete(drop = FALSE) +
  facet_wrap(~corpus, ncol = 1) +
  theme_bw() +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(x = "Position", y = "% Stressed Syllables that are Heavy") +
  theme(legend.title = element_blank()) -> plot_stress_heavy

ggsave("stressed heavies.png", plot_stress_heavy, dpi = 600)

# Create the complete grid correctly - only valid position/odd combinations
position_odd_grid <- tibble(
  position = 1:10,
  odd = position %% 2  # Keep as numeric
)

ratio_weight <- prop_heavy %>%
  dplyr::select(corpus, proportion, position, odd) %>%
  dplyr::mutate(odd = as.numeric(odd)) %>%  # Convert odd to numeric
  tidyr::pivot_wider(names_from = corpus, values_from = proportion, names_prefix = "corpus_") %>%
  dplyr::mutate(ratio = corpus_epic / corpus_prose) %>%
  right_join(position_odd_grid, by = c("position", "odd"))

ratio_weight %>%
  ggplot(aes(x = factor(position), y = ratio,
             fill = factor(odd, levels = c(1,0), labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = .7) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_grey(start = .4, end = .8) +
  guides(fill = guide_legend(na.translate = FALSE)) +
  labs(x = "Position", y = "Epic/Prose Ratio") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) -> epic_prose_stressed_heavies

ggsave("stressed heavies ratio.png", epic_prose_stressed_heavies, dpi = 600)

# % heavy stressed in prose (section 4.2.2)
prose %>%
  filter(real == 1) %>%
  filter(!accent_type == 0) %>%
  group_by(is_heavy) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count), proportion = count/total) %>%
  filter(is_heavy == 1) -> prop_heavy_stressed_prose

# % heavy stressed in epic (section 4.2.2)
epic %>%
  filter(real == 1) %>%
  filter(!accent_type == 0) %>%
  group_by(is_heavy) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count), proportion = count/total) %>%
  filter(is_heavy == 1) -> prop_heavy_stressed_epic

# fisher's exact test
fisher.test(matrix(c(3810,13178,860,2805), ncol = 2))



# tone disyllable initials (section 4.2.3)
data %>%
  mutate(corpus_new = paste(real,corpus)) %>%
  filter(!accent_type == 0) %>%
  filter(pos == 1 & of == 2) %>%
  group_by(corpus_new, of, position, is_heavy, odd, accent_type) %>% 
  summarise(count = n()) %>% 
  group_by(corpus_new, of, position, is_heavy, odd) %>% 
  mutate(total = sum(count), proportion = count/total) %>% 
  ungroup() %>%
  mutate(lower = binom.wilson(count, total)$lower, 
         upper = binom.wilson(count, total)$upper) %>% 
  filter(accent_type == 2) %>% 
  filter(!corpus_new == "0 prose") %>%
  filter(!(corpus_new == "1 epic" & total < 10)) %>%
  filter(!(corpus_new == "0 epic" & total < 100)) %>%
  filter(!(corpus_new == "1 prose" & total < 10)) %>%
  ggplot(aes(x = factor(position, levels = 1:10), 
             y = proportion,
             fill = factor(odd,
                           levels = c(1,0),
                           labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", width = .5, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  facet_grid(factor(corpus_new,
                    levels = c("1 epic", "0 epic", "1 prose"),
                    labels = c("Epic Real", "Epic Shuffle", "Prose")) ~ 
               factor(is_heavy, levels = c(0,1), labels = c("light", "heavy"))) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_fill_grey(start = .4, end = .8) +
  scale_x_discrete(drop = FALSE) +  
  labs(y = "% Stressed Syllables that are H-toned", x = "Position") +
  theme_bw() +
  theme(#legend.position = "bottom",
        legend.title = element_blank()) -> bar_plot_disyllables_tone

ggsave("tone disyllables all corpora.png", bar_plot_disyllables_tone, dpi = 600)

# tone trisyllable initials (section 4.2.3)
data %>%
  mutate(corpus_new = paste(real,corpus)) %>%
  filter(!accent_type == 0) %>%
  filter(pos == 1 & of == 3) %>%
  group_by(corpus_new, of, position, is_heavy, odd, accent_type) %>% 
  summarise(count = n()) %>% 
  group_by(corpus_new, of, position, is_heavy, odd) %>% 
  mutate(total = sum(count), proportion = count/total) %>% 
  ungroup() %>%
  mutate(lower = binom.wilson(count, total)$lower, 
         upper = binom.wilson(count, total)$upper) %>% 
  filter(accent_type == 2) %>% 
  filter(!corpus_new == "0 prose") %>%
  filter(!(corpus_new == "1 epic" & total < 10)) %>%
  filter(!(corpus_new == "0 epic" & total < 100)) %>%
  filter(!(corpus_new == "1 prose" & total < 10)) %>%
  ggplot(aes(x = factor(position, levels = 1:10), 
             y = proportion,
             fill = factor(odd,
                           levels = c(1,0),
                           labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", width = .5, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  facet_grid(factor(corpus_new,
                    levels = c("1 epic", "0 epic", "1 prose"),
                    labels = c("Epic Real", "Epic Shuffle", "Prose")) ~ 
               factor(is_heavy, levels = c(0,1), labels = c("light", "heavy"))) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_fill_grey(start = .4, end = .8) +
  scale_x_discrete(drop = FALSE) +  
  labs(y = "% Stressed Syllables that are H-toned", x = "Position") +
  theme_bw() +
  theme(#legend.position = "bottom",
    legend.title = element_blank()) -> bar_plot_trisyllables_tone

ggsave("tone trisyllables all corpora.png", bar_plot_trisyllables_tone, dpi = 600)

# real to scrambled ratio for disyllable and trisyllable initials (section 4.2.3)
summary_data_initials <- data %>%
  filter((pos == 1 & of == 2) | (pos == 1 & of == 3)) %>%
  filter(!tone == "u") %>%
  filter(corpus == "epic") %>%
  group_by(real, of, position, is_heavy, odd, accent_type) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(real, of, position, is_heavy, odd) %>% 
  mutate(total = sum(count), proportion = count / total) %>% 
  ungroup() %>%
  filter(accent_type != "1") %>%
  filter(!(real == 1 & total < 10)) %>%
  filter(!(real == 0 & total < 100)) %>%
  complete(real, of, position = 1:10, is_heavy, 
           fill = list(count = 0, total = 0, proportion = NA, lower = NA, upper = NA)) %>%
  mutate(strong = if_else((position == "1" | position == "3" | position == "5" | position == "7" | position == "9"), "1", "0"))

ratio_data <- summary_data_initials %>%
  dplyr::select(of, real, proportion, position, is_heavy, strong) %>%
  tidyr::pivot_wider(names_from = real, values_from = proportion, names_prefix = "real_") %>%
  dplyr::mutate(ratio = real_1 / real_0)

ratio_data %>%
  ggplot(aes(x = factor(position), y = ratio,
                         fill = factor(strong, levels = c(1,0), labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = .7) +
  facet_grid(factor(is_heavy, levels = c(0,1), labels = c("light", "heavy")) ~ 
               factor(of, levels = c(2,3), labels = c("disyllables", "trisyllables"))) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_grey(start = .4, end = .8) +
  guides(fill = guide_legend(na.translate = FALSE)) +
  labs(x = "Position", y = "Real/Scrambled Ratio") +
  theme_bw() +
  theme(legend.title = element_blank(),
        #legend.position = "bottom"
        ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) -> real_scrambled

ggsave("real scrambled ratio.png", real_scrambled, dpi = 600)


# epic to prose ratio (section 4.2.3)
summary_data_initials_epic_prose <- data %>%
  filter((pos == 1 & of == 2) | (pos == 1 & of == 3)) %>%
  filter(!tone == "u") %>%
  filter(real == 1) %>%
  group_by(corpus, of, position, is_heavy, odd, accent_type) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(corpus, of, position, is_heavy, odd) %>% 
  mutate(total = sum(count), proportion = count / total) %>% 
  ungroup() %>%
  filter(accent_type != "1") %>%
  filter(!total < 10) %>%
  complete(corpus, of, position = 1:10, is_heavy, 
           fill = list(count = 0, total = 0, proportion = NA, lower = NA, upper = NA)) %>%
  mutate(strong = if_else((position == "1" | position == "3" | position == "5" | position == "7" | position == "9"), "1", "0"))

ratio_data_epic_prose <- summary_data_initials_epic_prose %>%
  dplyr::select(of, corpus, proportion, position, is_heavy, strong) %>%
  tidyr::pivot_wider(names_from = corpus, values_from = proportion, names_prefix = "corpus_") %>%
  dplyr::mutate(ratio = corpus_epic / corpus_prose)

ratio_data_epic_prose %>%
  ggplot(aes(x = factor(position), y = ratio,
             fill = factor(strong, levels = c(1,0), labels = c("strong", "weak")))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = .7) +
  facet_grid(factor(is_heavy, levels = c(0,1), labels = c("light", "heavy")) ~ 
               factor(of, levels = c(2,3), labels = c("disyllables", "trisyllables"))) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_grey(start = .4, end = .8) +
  guides(fill = guide_legend(na.translate = FALSE)) +
  labs(x = "Position", y = "Epic/Prose Ratio") +
  theme_bw() +
  theme(legend.title = element_blank(),
        #legend.position = "bottom"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) -> epic_prose

ggsave("epic prose ratio.png", epic_prose, dpi = 600)

# percentage of disyllable and trisyllable initials in odd/even positions (section 4.2.3)
epic %>%
  filter(real == 1) %>%
  filter((pos == 1 & of == 2) | (pos == 1 & of == 3)) %>%
  group_by(of,odd) %>%
  summarise(count = n()) %>%
  group_by(of) %>%
  mutate(total = sum(count), percentage = count/total) %>%
  filter(odd == 1) -> disyll_trisyll_initials

# percentage of syllable types in weak positions (section 6.1)
data %>%
  filter(!(position == "4" | position == "10")) %>%
  filter(corpus == "epic" & real == 1) %>%
  group_by(accent_type, weight, odd) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(accent_type, weight) %>%
  mutate(total = sum(count),
         percentage = count / total) %>%
  mutate(lower = binom.wilson(count, total)$lower,
         upper = binom.wilson(count, total)$upper) %>%
  filter(odd == "0") -> perc_in_weak_positions
