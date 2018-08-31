#######################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Analysis and Plotting
#######################################
require(broom)
require(ggplot2)
require(RColorBrewer)
require(scales)
require(gridExtra)
require(stringr)
require(dplyr)
require(tidyr)

########################################################################
# American Sociological Rreview - keyword search
########################################################################
# Load "ASRjoined.RData" - 14231 obs of 25 vars.
load(file.choose())

names(merged)
# Save as asr_arts
asr_arts <- filter(merged, type == "research-article" & title != "Book Review" & title != "Book Reviews" & title != "Book Reviewers" & title != "Commentary and Debate" & title != "Book Notes")
asr_arts$year <- as.numeric(as.character(asr_arts$year))


# "Holy Trinity" search
asr_arts$race <- str_count(asr_arts$text, "race")
asr_arts$class <- str_count(asr_arts$text, "class")
asr_arts$gender <- str_count(asr_arts$text, "gender")

# Create yearly totals
ht_plot <- asr_arts %>% 
  group_by(year) %>% 
  summarize(race = sum(race),
            class = sum(class),
            gender = sum(gender)) %>% 
  ungroup() %>% 
  gather("ht", "n", race:gender)

ht_plot_p <- filter(ht_plot, n > 0)

# Save variable as factor
ht_plot_p$ht <- factor(ht_plot_p$ht, levels = c("class", "race", "gender"))

# Choosing colors
colors <- hue_pal()(3)
colors2 <- c(colors[2], colors[3], colors[1])

# Final Tweaking
asr_ht <- ggplot(ht_plot_p, aes(year, log2(n))) +
  geom_point(aes(color = ht, alpha = n), size = 3) +
  scale_alpha_continuous(range = c(0.3, 0.8), guide = F) +
  geom_smooth(aes(color = ht), se = F, size = 2) +
  scale_color_manual(values = colors2, name = "") +
  scale_y_continuous(breaks = seq(0, 13, by = 1), labels = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192))  +
  scale_x_continuous(breaks = seq(1900, 2000, by = 20)) +
  labs(title = "American Sociological Review", subtitle = "Word frequencies per year (log2 scale)", y = "Yearly Mentions", x = NULL, caption = "") +
  theme(text=element_text(size = 14, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        #legend.position = "bottom",
        legend.title=element_blank(),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = c(.9,.5))
asr_ht


########################################################
# American Journal of Sociology - Page length as function of date * author 
########################################################
# Load "AJSjstor4.RData" - 23885 obs of 36 vars.
load(file.choose())

# Filter by type
ajs_arts <- filter(merged, type == "research-article")

# Eliminate problem titles
'%ni%' <- Negate('%in%')

problem_titles <- c("Book Review", "Book Reviews", "Book Reviewers", "Commentary and Debate", "Book Notes")

ajs_arts <- ajs_arts %>% 
  filter(title %ni% problem_titles) %>% 
  filter(str_detect(title, "Acknowledgments") == FALSE) %>% 
  filter(str_detect(title, "Contents of Volume") == FALSE) %>% 
  filter(str_detect(title, "Call for Papers") == FALSE) %>% 
  filter(str_detect(title, "Errata") == FALSE)
  
ajs_arts$year <- as.numeric(as.character(ajs_arts$year))

# Create authorship variable from number of author columns != NA
ajs_arts$nauth <- 1
ajs_arts$nauth[!is.na(ajs_arts$auth2)] <- 2
ajs_arts$nauth[!is.na(ajs_arts$auth3)] <- 3
ajs_arts$nauth[!is.na(ajs_arts$auth4)] <- 4
ajs_arts$nauth <- as.integer(ajs_arts$nauth)
ajs_arts$title <- as.character(ajs_arts$title)

# Run linear model with interaction term
ajs_mod <- lm(length ~ year + factor(nauth) + year:factor(nauth), data = ajs_arts)
# Save R-squared
rsq <- summary(ajs_mod)$r.squared
# Add to existing data
ajs_res <- augment(ajs_mod)

# Choose colors
colz <- brewer.pal(8, "Blues")
colz <- colz[5:8]

# Plot results
inter <- ggplot(ajs_res, aes(year, length)) +
  geom_point(position = "jitter", aes(alpha = .resid, fill = .resid, size = .resid), pch = 21) +
  scale_size(guide = F) +
  scale_alpha(range = c(0.3, 0.8), guide = F) +
  scale_fill_continuous(low = "black", high = "red", guide = F) +
  geom_line(size = 2, aes(y = .fitted, color = factor.nauth.)) +
  scale_color_manual(values = colz, name = "Authors") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  scale_x_continuous(limits = c(1897, 2015)) +
  labs(title = "American Journal of Sociology", subtitle = "Article length as function of \"date x author\"", caption = paste("R-Squared =", round(rsq, 2)), x = NULL, y = "Page Length") +
  theme(text=element_text(size = 14, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  geom_text(aes(label = ifelse(length>125, "Albion Small (1916, 21:6)", "")), 
            nudge_x = 43)

inter



g <- grid.arrange(asr_ht, inter, ncol = 2)

ggsave(g, file = "~/Desktop/socius.tiff", width = 10, height = 5, units = "in", dpi = 1200)

ggsave(g, file = "~/Desktop/socius.tiff", width = 10, height = 5, units = "in", dpi = 600)
