##----------------------------------------------------------------------------------##
##                         STARSET ALBUM LYRICS ANALYSIS                            ##
##----------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Inspired by https://www.johnmackintosh.com/2018-01-30-hardwired-for-tidy-text/?utm_content=buffereffed&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer


#-------#
# Setup #
#-------#

# Install package geniusR from GitHub
## Source: https://github.com/josiahparry/geniusR 
install.packages("devtools")
library(devtools)
devtools::install_github("josiahparry/geniusR")
library(geniusR)

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(ggraph, ggrepel, ggthemes, grid, igraph, jpeg, magrittr, reshape2, tidyr, tidytext, tidyverse, wordcloud)


#----------------------#
# Download album texts #
#----------------------#

# Transmissions
transmissions <- genius_album(artist = "Starset", album = "Transmissions", nested = FALSE)

# Vessels
vessels <- genius_album(artist = "Starset", album = "Vessels", nested = FALSE)

# Add columns for album and merge
transmissions$album <- "Transmissions"
vessels$album <- "Vessels"
starset <- rbind(transmissions, vessels)


#---------------#
# Text cleaning #
#---------------#

# Remove punctuation (except for apostrophes) and numbers
starset$text <- gsub("[^[:alpha:][:blank:]']", "", starset$text)

# Unnest and tokenize text
starset_tidy <- starset %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#--------------------------#
# Theme for visualizations #
#--------------------------#

# Set theme for visualizations
viz_theme <- theme(
  strip.background = element_rect(colour = "grey20", fill = "#92a1a9"),
  axis.line = element_line(colour = "grey20"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "#4e5975"),
  text = element_text(family = "Avenir"))

# Color palette based on Vessels album cover
starset_cols <- c("#363241", "#3b2d2d", "#483845", "#4d3b4b", "#332f3e", "#2a2a2c", "#353140", 
                  "#2c2a37", "#2f282f", "#363346", "#806c88", "#2f3b4b", "#282832", "#293241",
                  "#245d68", "#1c2f36", "#16211d", "#172a31", "#0b2735", "#0d2434", "#102d3d", 
                  "#295860", "#1f383d", "#232f25", "#162f34", "#0b1e2f", "#192e43", "#133654", 
                  "#12202b", "#253b48", "#101a1b", "#223941", "#0e1b24", "#101922", "#0d1a23", 
                  "#0e1619", "#131418", "#10171d")


#-------------------#
# Most common words #
#-------------------#

# Plot most common words
starset_tidy %>%  
  group_by(album) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = starset_cols[1:17], alpha = 0.9) +
  facet_wrap( ~ album, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 20, color = "#1f232e")) + 
  xlab("") + ylab("") + ggtitle("Most common words on STARSET albums", subtitle = " ") +
  ylim(0, 60) + coord_flip() + viz_theme

# Most common words (lollipop chart)
starset_tidy %>%  
  group_by(album) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_segment(aes(x = word, 
                   xend = word, 
                   y = 0, 
                   yend = n), col = "grey50", alpha = 0.9) +
  geom_point(col = starset_cols[1:17], size = 4, alpha = 0.9) +  
  facet_wrap( ~ album, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 20, color = "#1f232e")) + 
  xlab("") + ylab("") + ggtitle("Most common words on STARSET albums", subtitle = " ") +
  ylim(0, 60) + coord_flip() + viz_theme

ggsave("plot1.png", width = 12, height = 8, units = "in", dpi = 100)


#--------------------#
# Sentiment analysis #
#--------------------#

# Plot NRC sentiment scores
starset_tidy %>%
  group_by(album) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n)) +
  geom_bar(aes(fill = sentiment), stat = "identity", alpha = 0.9) +
  scale_fill_manual(values = starset_cols[25:35]) +
  facet_wrap( ~ album, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 20, color = "#1f232e"), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("Sentiment scores for STARSET albums", subtitle = " ") +
  ylim(0, 300) + theme(legend.position = "none") + viz_theme

ggsave("plot2.png", width = 12, height = 8, units = "in", dpi = 100)


#-------------------------#
# Positive/negative words #
#-------------------------#

# Get Bing sentiment scores
starset_bing <- starset_tidy %>%
  group_by(album) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
# Get top word contributors
starset_bing_top <- starset_bing %>%
  group_by(album, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

# Plot most common positive and negative words: Transmissions
starset_bing_top %>%
  filter(album == "Transmissions")  %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
    geom_col(alpha = 0.9, show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    scale_fill_manual(values = starset_cols[c(14, 4)]) +
    xlab("") + ylab("") + 
    theme(text = element_text(size = 20, color = "#1f232e")) + 
    ggtitle("Most common positive and negative words", subtitle = "Transmissions by STARSET") +
    ylim(0, 20) + coord_flip() + viz_theme

ggsave("plot3.png", width = 12, height = 8, units = "in", dpi = 100)

# Plot most common positive and negative words: Vessels
starset_bing_top %>%
  filter(album == "Vessels")  %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
    geom_col(alpha = 0.9, show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    scale_fill_manual(values = starset_cols[c(14, 4)]) +
    xlab("") + ylab("") + 
    theme(text = element_text(size = 20, color = "#1f232e")) + 
    ggtitle("Most common positive and negative words", subtitle = "Vessels by STARSET") +
    ylim(0, 60) + coord_flip() + viz_theme

ggsave("plot4.png", width = 12, height = 8, units = "in", dpi = 100)


#---------#
# N-grams #
#---------#

# Get bigrams
starset_bigrams <- starset %>%
  group_by(album) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

starset_bigram_counts <- starset_bigrams %>%
  group_by(album) %>%
  count(bigram, sort = TRUE)

# Plot bigrams
starset_bigram_counts %>%
  group_by(album) %>%
  filter(n > 10) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = starset_cols[1:20], alpha = 0.9) +
  facet_wrap(~album, scales = "free_y") +
  theme(text = element_text(size = 20, color = "#1f232e")) + 
  xlab("") + ylab("") + ggtitle("Most common bigrams on STARSET albums", subtitle = " ") +
  ylim(0, 40) + coord_flip() + viz_theme

# Plot bigrams (lollipop chart)
starset_bigram_counts %>%
  group_by(album) %>%
  filter(n > 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
    geom_segment(aes(x = bigram, 
                   xend = bigram, 
                   y = 0, 
                   yend = n), col = "grey50", alpha = 0.9) +
    geom_point(col = starset_cols[1:27], size = 4, alpha = 0.9) +  
    facet_wrap(~album, scales = "free_y") +
    theme(text = element_text(size = 20, color = "#1f232e")) + 
    xlab("") + ylab("") + ggtitle("Most common bigrams on STARSET albums", subtitle = " ") +
    ylim(0, 40) + coord_flip() + viz_theme

ggsave("plot5.png", width = 12, height = 8, units = "in", dpi = 100)

# Split bigrams by album and remove album column
starset_bigrams_tm <- starset_bigram_counts %>%
  filter(album == "Transmissions") %>%
  separate(bigram, c("word1", "word2"), sep = " ")
starset_bigrams_tm <- starset_bigrams_tm[, -1]

starset_bigrams_vs <- starset_bigram_counts %>%
  filter(album == "Vessels") %>%
  separate(bigram, c("word1", "word2"), sep = " ")
starset_bigrams_vs <- starset_bigrams_vs[, -1]

# Convert to igraph objects for plotting and filter by frequency
starset_bigram_tm_graph <- starset_bigram_tm %>%
  filter(n > 5) %>%
  graph_from_data_frame()

starset_bigram_vs_graph <- starset_bigram_vs %>%
  filter(n > 5) %>%
  graph_from_data_frame()


#------------------#
# Image processing #
#------------------#
  
# Download background image
## Image credit: Marcio De Assis (https://commons.wikimedia.org/wiki/User:Marcio_De_Assis)
download.file("https://upload.wikimedia.org/wikipedia/commons/4/4d/Milky_Way_from_S%C3%A3o_Louren%C3%A7o.jpg", 
              destfile = "milky_way.jpg", mode = "wb")

# Load picture and render
milky_way <- readJPEG("milky_way.jpg", native = TRUE)
milky_way <- rasterGrob(milky_way, interpolate = TRUE)
  
  
#-----------------#
# Circle function #
#-----------------#

## Circle design inspired by https://www.c82.net/blog/?id=73 

# Adapted and extended function to draw circle
## Source: https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
## Thanks to Z. Lin at Stack Overflow for helping (https://stackoverflow.com/questions/48549897/positioning-nodes-and-edges-in-network-graph-using-ggraph-ggplot2)!

gg_circle_from_position <- function(data, rsize = NA, 
                                    color = "black", fill = NA, 
                                    lty = NA, size = NA, ...){
  coord.x <- data[, "x"]
  coord.y <- data[, "y"]
  
  xc = mean(range(coord.x))
  yc = mean(range(coord.y))
  r = max(sqrt((coord.x - xc)^2 + (coord.y - yc)^2)) * rsize

  x <- xc + r*cos(seq(0, pi, length.out = 100))
  ymax <- yc + r*sin(seq(0, pi, length.out = 100))
  ymin <- yc + r*sin(seq(0, -pi, length.out = 100))
  annotate("ribbon", x = x, ymin = ymin, ymax = ymax, 
           color = color, fill = fill, lty = lty, size = size, ...)
}


#----------#
# Starplot #
#----------#

## Transmissions 
p <- ggraph(starset_bigram_tm_graph, layout = "fr") +
  annotation_custom(milky_way, xmin = -30, xmax = 30, ymin = -30, ymax = 30) +  
  geom_edge_link(aes(edge_alpha = n*2), colour = "#fafdff") +
  geom_node_point(color = "#fafdff", aes(size = degree(starset_bigram_tm_graph)*8)) +
  geom_node_text(aes(label = name), color = "white", size = 5, check_overlap = TRUE, repel = TRUE,
                 nudge_x = 0.1, nudge_y = 0.1) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks.length = unit(0, "cm"),
        legend.position = "none")

p.positions <- layer_data(p, i = 2L)

p + 
  gg_circle_from_position(data = p.positions, rsize = 1.23, color = "#fafdff", lty = 1, size = 0.4) +
  gg_circle_from_position(data = p.positions, rsize = 1.24, color = "#fafdff", lty = "3313", size = 0.4) +
  gg_circle_from_position(data = p.positions, rsize = 1.25, color = "#fafdff", lty = 1, size = 0.4) +
  annotate("text", x = 14, y = -4, hjust = 0, size = 8, 
           label = paste("STARSET"), color = "white", alpha = 0.8) +
  annotate("text", x = 14, y = -5, hjust = 0, size = 6, 
           label = paste("Transmissions"), color = "white", alpha = 0.6) +
  coord_equal()

ggsave("Starplot_Transmissions.png", width = 10, height = 10, units = "in", dpi = 100)


## Vessels
p <- ggraph(starset_bigram_vs_graph, layout = "fr") +
  annotation_custom(milky_way, xmin = -30, xmax = 30, ymin = -30, ymax = 30) +  
  geom_edge_link(aes(edge_alpha = n*2), colour = "#fafdff") +
  geom_node_point(color = "#fafdff", aes(size = degree(starset_bigram_vs_graph)*8)) +
  geom_node_text(aes(label = name), color = "white", size = 5, check_overlap = TRUE, repel = TRUE,
                 nudge_x = 0.1, nudge_y = 0.1) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks.length = unit(0, "cm"),
        legend.position = "none")

p.positions <- layer_data(p, i = 2L)

p + 
  gg_circle_from_position(data = p.positions, rsize = 1.23, color = "#fafdff", lty = 1, size = 0.4) +
  gg_circle_from_position(data = p.positions, rsize = 1.24, color = "#fafdff", lty = "3313", size = 0.4) +
  gg_circle_from_position(data = p.positions, rsize = 1.25, color = "#fafdff", lty = 1, size = 0.4) +
  annotate("text", x = 10, y = -9, hjust = 0, size = 8, 
           label = paste("STARSET"), color = "white", alpha = 0.8) +
  annotate("text", x = 10, y = -10, hjust = 0, size = 6, 
           label = paste("Vessels"), color = "white", alpha = 0.6) +
  coord_equal()

ggsave("Starplot_Vessels.png", width = 10, height = 10, units = "in", dpi = 100)
