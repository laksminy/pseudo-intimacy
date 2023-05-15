library(tidyverse)
library(ggtext)
library(readxl)
library(vcd)

# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-1", range = "A1:Y5271")
# 
# laks %>% 
#   write_tsv("data/raw-1.tsv")

laks <- read_tsv("data/raw-1.tsv")

# data for figure 1 =======
df1 <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)", 
         str_detect(`katagori ProN 1`, "^1(a|b)\\."),
         str_detect(`Fungsi 4`, "(^1\\. aku|^2\\. saya)"),
         `jender 1` %in% c("F", "M"))

## count =====
df1_count <- df1 %>% 
  rename(gender = `jender 1`, 
         direction_of_speech = `speaker mdl dede`, 
         pronouns = `Fungsi 4`) %>%
  count(gender, direction_of_speech, pronouns) %>%
  mutate(direction_of_speech = str_replace(direction_of_speech, "->", "→"),
         direction_of_speech = str_replace(direction_of_speech, "(host)", "\\1s"),
         pronouns = str_replace(pronouns, "^\\d\\.\\s", ""),
         direction_of_speech = factor(direction_of_speech, 
                                      levels = c("younger_male_guest → older_hosts (M < H)", "older_male_guest → younger_hosts (M > H)", "younger_female_guest → older_hosts (F < H)", "older_female_guest → younger_hosts (F > H)")))

## visualisation =====
df1_count %>% 
  ggplot(aes(x = direction_of_speech, y = n, fill = pronouns, group = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5), size = 2.7) +
  # facet_wrap(~gender, drop = TRUE, scales = "free_x") + 
  labs(y = "proportion", 
       x = NULL, 
       fill = "1st sing. pronouns", 
       subtitle = "The '→' indicates the direction of speech\n(i.e., from the guests to the hosts)",
       caption = "Values inside the bars are the raw frequencies.") +
  scale_fill_discrete(breaks = c("aku/gue/-ku (intimate form)", "saya (respect distant form)"), labels = c("*aku/gue/-ku* (intimate form)", "*saya* (respect distant form)")) +
  theme(legend.text = element_markdown(),
        plot.caption = element_markdown(size = 7))
  ggsave('figs/fig1.png', width = 7.5, height = 3.5, dpi = 600)

  
## data for percentage =====
df1_count %>% group_by(direction_of_speech) %>% mutate(perc = n/sum(n) * 100)
  
  
df1_matrix <- df1_count %>% 
  select(-gender) %>% 
  pivot_wider(names_from = pronouns, 
              values_from = n) %>% 
  mutate(direction_of_speech = str_extract(direction_of_speech, "(?<=\\s)\\(. [<>] .\\)$")) %>% 
  data.frame(row.names = 1, check.names = F) %>% 
  as.matrix()
assocstats(df1_matrix)
assoc(df1_matrix, shade = T)


# data for figure 2 =====
df2 <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)", 
         str_detect(`katagori ProN 1`, "^1(a|b)\\."),
         str_detect(`Fungsi 4`, "(^1\\. aku|^2\\. saya)"),
         `jender 1` %in% c("HF", "HM"))

## count =====
df2_count <- df2 %>% 
  rename(gender = `jender 1`, 
         direction_of_speech = `speaker mdl dede`, 
         pronouns = `Fungsi 4`) %>%
  count(gender, direction_of_speech, pronouns) %>%
  mutate(direction_of_speech = str_replace(direction_of_speech, "->", "→"),
         direction_of_speech = str_replace(direction_of_speech, "(guest)", "\\1s"),
         pronouns = str_replace(pronouns, "^\\d\\.\\s", ""),
         direction_of_speech = factor(direction_of_speech, 
                                      levels = c("older_male_host → younger_guests (HM > G)", "younger_male_host → older_guests (HM < G)", "older_female_host → younger_guests (HF > G)", "younger_female_host → older_guests (HF < G)")))

## visualisation ====
df2_count %>% 
  ggplot(aes(x = direction_of_speech, y = n, fill = pronouns, group = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5), size = 2.5) +
  # facet_wrap(~gender, drop = TRUE, scales = "free_x") + 
  labs(y = "proportion", 
       x = NULL, 
       fill = "1st sing. pronouns", 
       subtitle = "The '→' indicates the direction of speech\n(i.e., from the hosts to the guests)",
       caption = "Values inside the bars are the raw frequencies.") +
  scale_fill_discrete(breaks = c("aku/gue/-ku (intimate form)", "saya (respect distant form)"), labels = c("*aku/gue/-ku* (intimate form)", "*saya* (respect distant form)")) +
  theme(legend.text = element_markdown(),
        plot.caption = element_markdown(size = 7))
ggsave('figs/fig2.png', width = 7.5, height = 3.5, dpi = 600)



# data for figure 3 =====
df3 <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)", 
         str_detect(`katagori ProN 2`, "1st person"),
         str_detect(`Fungsi 4`, "(kita|kami)"),
         `jender 1` %in% c("HF", "HM", "F", "M"))

## count =====
df3_count <- df3 %>% 
  rename(gender = `jender 1`, 
         pronouns = `Fungsi 4`) %>%
  count(gender, pronouns) %>%
  mutate(pronouns = str_replace_all(pronouns, "^\\d\\.\\s", ""),
         pronouns = str_replace_all(pronouns, "^([^ ]+)(?=\\s)", "*\\1*"),
         pronouns = factor(pronouns),
         gender = replace(gender, gender == "F", "Female guests (F)"),
         gender = replace(gender, gender == "HF", "Female host (HF)"),
         gender = replace(gender, gender == "M", "Male guests (M)"),
         gender = replace(gender, gender == "HM", "Male host (HM)"),
         gender = factor(gender, levels = c("Female host (HF)", "Female guests (F)", "Male host (HM)", "Male guests (M)")))

## visualisation =====
df3_count %>% 
  ggplot(aes(x = gender, y = n, fill = pronouns, group = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5), size = 2.5) +
  # facet_wrap(~gender, drop = TRUE, scales = "free_x") + 
  labs(y = "proportion", 
       x = NULL, 
       fill = "1st pl. pronouns",
       caption = "Values inside the bars are the raw frequencies.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.text = element_markdown(),
        plot.caption = element_markdown(size = 7))
ggsave('figs/fig3.png', width = 6, height = 3.5, dpi = 600)

## data for percentage ======
df3_count %>% 
  group_by(gender) %>% 
  mutate(perc = n/sum(n))




# data for figure 4 =====

## count =====
int <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)",
         str_detect(`katagori ProN 2`, "1st"),
         str_detect(bentuk, "^([Aa])?ku|gue|kita")) %>%
  count(`jender kuMkul`, name = "intimate_form (*aku*, *-ku*, *gue*, *kita*)") %>% 
  filter(!is.na(`jender kuMkul`)) %>%
  rename(gender_cumulative = `jender kuMkul`)

resp <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)",
         str_detect(`katagori ProN 2`, "1st"),
         str_detect(bentuk, "^([Aa])?ku|gue|kita", negate = TRUE)) %>%
  count(`jender kuMkul`, name = "respect_form (*saya*, *kami*)") %>% 
  filter(!is.na(`jender kuMkul`)) %>%
  rename(gender_cumulative = `jender kuMkul`)

df4 <- left_join(int, resp) %>% 
  pivot_longer(-gender_cumulative, names_to = "pronoun_type", values_to = "n") %>% 
  mutate(gender_cumulative = str_replace(gender_cumulative, "Fk", "Female host and guests"),
         gender_cumulative = str_replace(gender_cumulative, "Mk", "Male host and guests"),
         pronoun_type = factor(pronoun_type, levels = c("respect_form (*saya*, *kami*)", "intimate_form (*aku*, *-ku*, *gue*, *kita*)")))

## visualisation =====
df4 %>% 
  ggplot(aes(x = gender_cumulative, y = n, fill = pronoun_type)) + 
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.text = element_markdown()) +
  labs(y = "proportion", x = "participants", fill = "pronouns", caption = "Values inside the bars are the raw frequencies.")
ggsave('figs/fig4.png', width = 7.85, height = 3.5, dpi = 600)

df4_mtx <- df4 %>% 
  pivot_wider(names_from = gender_cumulative, values_from = n) %>% 
  data.frame(check.names = FALSE, row.names = 1) %>% 
  as.matrix()
chisq.test(df4_mtx)
chisq.test(df4_mtx)$residuals


# data for figure 5 ====
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-3", range = "A1:Y5271")
# 
# laks %>% 
#   write_tsv("data/raw-2.tsv")

laks <- read_tsv("data/raw-2.tsv")

df5 <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)", 
         str_detect(`katagori ProN 2`, "2nd peson"),
         str_detect(`Fungsi 4`, "(1\\. kamu|2\\. anda)"),
         `jender 1` %in% c("F", "M"),
         str_detect(`speaker mdl dede`, "\\([MF] [<>] H"))


df5_count <- df5 %>% 
  rename(gender = `jender 1`, 
         direction_of_speech = `speaker mdl dede`, 
         pronouns = `Fungsi 4`) %>%
  count(gender, direction_of_speech, pronouns) %>%
  mutate(direction_of_speech = str_replace(direction_of_speech, "->", "→"),
         direction_of_speech = str_replace(direction_of_speech, "(host)", "\\1s"),
         pronouns = str_replace(pronouns, "^\\d\\.\\s", ""),
         pronouns = str_replace_all(pronouns, "([a-z]+(?=\\,)|[a-z]+(?=\\s[(]))", "*\\1*"),
         pronouns = str_replace_all(pronouns, "\\, ", "/"),
         pronouns = str_replace_all(pronouns, "\\*mu\\*", "\\-*mu*"),
         pronouns = str_replace_all(pronouns, "\\*sampean", "<br>*sampean"),
         pronouns = str_replace_all(pronouns, "anda\\*", "anda*<br>"),
         direction_of_speech = factor(direction_of_speech, 
                                      levels = c("younger_male_guest → older_hosts (M < H)", "older_male_guest → younger_hosts (M > H)", "younger_female_guest → older_hosts (F < H)", "older_female_guest → younger_hosts (F > H)")))

## visualisation ====
df5_count %>% 
  ggplot(aes(x = direction_of_speech, y = n, fill = pronouns, group = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5), size = 4) +
  # facet_wrap(~gender, drop = TRUE, scales = "free_x") + 
  labs(y = "proportion", 
       x = NULL, 
       fill = "2nd pronouns", 
       subtitle = "The '→' indicates the direction of speech\n(i.e., from the guests to the hosts)",
       caption = "Values inside the bars are the raw frequencies.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.text = element_markdown(size = 9),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        plot.caption = element_markdown(size = 8))
ggsave('figs/fig5.png', width = 7.7, height = 5, dpi = 600)

# data for figure 6 ======
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-4", range = "A1:Y5271")
# 
# laks %>% write_tsv("data/raw-3.tsv")
laks <- read_tsv("data/raw-3.tsv")

df6 <- laks %>%
  filter(str_detect(`katagori ProN 2`, "2nd peson"),
         `jender 1` %in% c("HF", "HM"),
         str_detect(`speaker mdl dede`, "\\(H[FM] [<>] G"))

## count =====
df6_count <- df6 %>% 
  rename(gender = `jender 1`, 
         direction_of_speech = `speaker mdl dede`, 
         pronouns = `Fungsi 4`) %>%
  count(gender, direction_of_speech, pronouns) %>%
  mutate(direction_of_speech = str_replace(direction_of_speech, "->", "→"),
         direction_of_speech = str_replace(direction_of_speech, "(guest)", "\\1s"),
         pronouns = str_replace(pronouns, "^\\d\\.\\s", ""),
         pronouns = str_replace_all(pronouns, "([a-z]+(?=\\,)|[a-z]+(?=\\s[(]))", "*\\1*"),
         pronouns = str_replace_all(pronouns, "\\, ", "/"),
         pronouns = str_replace_all(pronouns, "\\*mu\\*", "\\-*mu*"),
         pronouns = str_replace_all(pronouns, "\\*sampean", "<br>*sampean"),
         pronouns = str_replace_all(pronouns, "anda\\*", "anda*<br>"),
         direction_of_speech = factor(direction_of_speech, 
                                      levels = c("younger_female_host → older_guests (HF < G)", "older_female_host → younger_guests (HF > G)", "younger_male_host → older_guests (HM < G)", "older_male_host → younger_guests (HM > G)")))

## visualisation =====
df6_count %>% 
  ggplot(aes(x = direction_of_speech, y = n, fill = pronouns, group = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5), size = 4) +
  # facet_wrap(~gender, drop = TRUE, scales = "free_x") + 
  labs(y = "proportion", 
       x = NULL, 
       fill = "2nd pronouns", 
       subtitle = "The '→' indicates the direction of speech\n(i.e., from the hosts to the guests)",
       caption = "Values inside the bars are the raw frequencies.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.text = element_markdown(size = 9),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        plot.caption = element_markdown(size = 8))
ggsave('figs/fig6.png', width = 7.7, height = 5, dpi = 600)


# data for figure 7 =======
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-5", range = "A1:Z5249")
# 
# laks %>% write_tsv("data/raw-5.tsv")
laks <- read_tsv("data/raw-5.tsv")

int <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)",
         str_detect(`katagori ProN 2`, "2nd"),
         `Fungsi 3` %in% c("Intimacy Marker")) %>%
  count(`jender kuMkul`, name = "intimate_form") %>% 
  filter(!is.na(`jender kuMkul`)) %>%
  rename(gender_cumulative = `jender kuMkul`)

resp <- laks %>%
  filter(`katagori bentuk 1` == "9. Pronomina (Pronoun)",
         str_detect(`katagori ProN 2`, "2nd"),
         `Fungsi 3` %in% c("Respective Marker")) %>%
  count(`jender kuMkul`, name = "respect_form") %>% 
  filter(!is.na(`jender kuMkul`)) %>%
  rename(gender_cumulative = `jender kuMkul`)

df7 <- left_join(int, resp) %>% 
  pivot_longer(-gender_cumulative, names_to = "pronoun_type", values_to = "n") %>% 
  mutate(gender_cumulative = str_replace(gender_cumulative, "Fk", "Female host and guests"),
         gender_cumulative = str_replace(gender_cumulative, "Mk", "Male host and guests"),
         pronoun_type = factor(pronoun_type, levels = c("intimate_form", "respect_form")))

# *kamu*, *kau*, *kue*, *lu*, *-mu*, *sampen*, and *kalian*
# *anda*

## visualisation =====
df7 %>% 
  ggplot(aes(x = gender_cumulative, y = n, fill = reorder(pronoun_type, n))) + 
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(aes(label = paste("n=", n, sep = "")), position = position_fill(.5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = "proportion", x = "participants", fill = "pronouns",
       caption = "Values inside the bars are the raw frequencies.<br>The intimate forms are *kamu*, *kue*, *lu*, -*mu*, *sampean*, and *kalian*.<br>The respect form is *Anda*.") +
  theme(legend.text = element_markdown(),
        plot.caption = element_markdown())
ggsave('figs/fig7.png', width = 7, height = 5, dpi = 600)

# data for figure 8 =======
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-6", range = "A1:Z5431")
#  
# laks %>% write_tsv("data/raw-6.tsv")
laks <- read_tsv("data/raw-6.tsv")

df8 <- laks %>% 
  filter(`jender 1` %in% c("F", "HF", "HM", "M"),
         str_detect(`Fungsi 4`, "\\b(dia|beliau)\\b")) %>% 
  count(`jender 1`, `Fungsi 4`) %>% 
  mutate(`Fungsi 4` = str_replace_all(`Fungsi 4`, "^\\d\\.\\s+", ""),
         `jender 1` = str_replace_all(`jender 1`, "HF", "Female Host (HF)"),
         `jender 1` = str_replace_all(`jender 1`, "HM", "Male Host (HM)"),
         `jender 1` = str_replace_all(`jender 1`, "^F$", "Female Guests (F)"),
         `jender 1` = str_replace_all(`jender 1`, "^M$", "Male Guests (M)"),
         `jender 1` = factor(`jender 1`, 
                             levels = c("Female Host (HF)", "Male Host (HM)", 
                                        "Female Guests (F)", "Male Guests (M)")),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, ",\\s", "/"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, "beliau", "*beliau*"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, 
                                      "\\b(dia|ia|mereka|nya)\\b", "*\\1*"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, "nya", "-nya")) %>% 
  arrange(`jender 1`)
df8

## visualisation ======
df8 %>% 
  ggplot(aes(x = `jender 1`, y = n, fill = `Fungsi 4`)) + 
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(aes(label = if_else(n > 10, paste("n=", n, sep = ""), "")), 
            position = position_fill(.5)) +
  theme(legend.text = element_markdown(),
        legend.title = element_markdown()) +
  labs(fill = "3^rd pers. pronouns", 
       x = "participants",
       y = "proportion",
       caption = "Values inside the bars are raw frequencies.")
ggsave('figs/fig8.png', width = 7, height = 4, dpi = 300)


# data for figure 9 =======
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-6", range = "A1:Z5431")
#  
# laks %>% write_tsv("data/raw-6.tsv")
laks <- read_tsv("data/raw-6.tsv")

df9 <- laks %>% 
  filter(`jender kuMtul` %in% c("Ft", "Mt"),
         str_detect(`Fungsi 4`, "\\b(dia|beliau)\\b")) %>% 
  count(`jender kuMtul`, `Fungsi 4`) %>% 
  mutate(`Fungsi 4` = str_replace_all(`Fungsi 4`, "^\\d\\.\\s+", ""),
         `jender kuMtul` = str_replace_all(`jender kuMtul`, 
                                           "Ft", 
                                           "Female host and guests"),
         `jender kuMtul` = str_replace_all(`jender kuMtul`, 
                                           "Mt", 
                                           "Male host and guests"),
         `jender kuMtul` = factor(`jender kuMtul`,
                                  levels = c("Female host and guests", 
                                             "Male host and guests")),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, ",\\s", "/"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, "beliau", "*beliau*"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, 
                                      "\\b(dia|ia|mereka|nya)\\b", "*\\1*"),
         `Fungsi 4` = str_replace_all(`Fungsi 4`, "nya", "-nya")) %>% 
  arrange(`jender kuMtul`)
df9

## visualisation ======
df9 %>% 
  ggplot(aes(x = `jender kuMtul`, y = n, fill = `Fungsi 4`)) + 
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(aes(label = if_else(n > 10, paste("n=", n, sep = ""), "")), 
            position = position_fill(.5)) +
  theme(legend.text = element_markdown(),
        legend.title = element_markdown()) +
  labs(fill = "3^rd pers. pronouns", 
       x = "participants",
       y = "proportion",
       caption = "Values inside the bars are raw frequencies.")
ggsave('figs/fig9.png', width = 7, height = 4, dpi = 300)


# data for figure 10 =======
# laks <- read_xlsx("pseudo-intimacy raw and analysed data.xlsx", sheet = "raw-7", range = "A1:Z5435")
# 
# laks %>% write_tsv("data/raw-7.tsv")
laks <- read_tsv("data/raw-7.tsv")

df10 <- laks %>% 
  filter(`jender kuMtul` %in% c("Ft", "Mt"),
         str_detect(`katagori bentuk 1`, "^9\\.\\sPronomina"),
         str_detect(`Fungsi 3`, "Marker$")) %>% 
  count(`jender kuMtul`, `Fungsi 3`) %>% 
  mutate(`Fungsi 3` = str_replace_all(`Fungsi 3`, 
                                      "Respective", 
                                      "Respect"),
         `jender kuMtul` = str_replace_all(`jender kuMtul`, 
                                           "Ft", 
                                           "Female host and guests"),
         `jender kuMtul` = str_replace_all(`jender kuMtul`, 
                                           "Mt", 
                                           "Male host and guests"),
         `jender kuMtul` = factor(`jender kuMtul`,
                                  levels = c("Female host and guests", 
                                             "Male host and guests")))
df10

## visualisation ======
df10 %>% 
  ggplot(aes(x = `jender kuMtul`, y = n, fill = `Fungsi 3`)) + 
  geom_col(position = "fill") +
  # coord_flip() +
  geom_text(aes(label = paste("n=", n, sep = "")), 
            position = position_fill(.5)) +
  theme(legend.text = element_markdown(),
        legend.title = element_markdown()) +
  labs(fill = "marker types", 
       x = "gender\n(across age and roles)",
       y = "proportion",
       caption = "Values inside the bars are raw frequencies.")
ggsave('figs/fig10.png', width = 7, height = 4, dpi = 300)

## Chi-square test ======
df10 %>% 
  pivot_wider(names_from = `Fungsi 3`, values_from = n) %>% 
  data.frame(row.names = 1) %>% 
  chisq.test()
df10 %>% 
  pivot_wider(names_from = `Fungsi 3`, values_from = n) %>% 
  data.frame(row.names = 1) %>% 
  chisq.test() %>% 
  .$p.value
### standardised residuals ====
df10 %>% 
  pivot_wider(names_from = `Fungsi 3`, values_from = n) %>% 
  data.frame(row.names = 1) %>% 
  chisq.test() %>% 
  .$stdres
