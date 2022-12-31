

q_data <- dplyr::right_join(admins, q_data, by = "cluster_id") |>
  dplyr::left_join(loca_qual_clust) |>
  dplyr::filter(!is.na(name), area_cat == "area_c")

text_df <- dplyr::select(q_data, loc_id = cluster_id, name, area_cat, cluster_cat, text) |>
  dplyr::distinct() |>
  dplyr::filter(text != "NULL")

words <- tidytext::unnest_tokens(text_df, word, text)

custom_stop <- c("tuqu", "qaffin", "beida", "na’ja", "kober", "ya’bad", "hhs", "hh", 
                 "fraseen", "marj", "nis", "ein", "yatta", "el", "rahhal", "wadi",
                 "rantis", "ras", "efrat", "nuwe’ima", "barta'a", "jiftlik",
                 "zabda", "muntar", "khirbet", "m3", "zeibad", "bqei’a", "kufr",
                 "arij", "mazra’a", "it’s", "alfe", "dab’a", "hermish", "barta’a",
                 "toura", "imreiha", "dura", "idna", "khallet", "gvc", "ww",
                 "tayasir", "abu", "njiem", "alsharqeiya", "annazleh", "jubara",
                 "adh", "bism", "mas’ud", "janubi", "tuwani", "butum", "bardala",
                 "imnaizel", "tuwani", "shoshahleh", "jubara", "adh")

stop_words <- c(tidytext::get_stopwords(source = "snowball")$word,
  tidytext::get_stopwords(source = "marimo")$word,
  tidytext::get_stopwords(source = "nltk")$word,
  tidytext::get_stopwords(source = "smart")$word,
  tidytext::get_stopwords(source = "stopwords-iso")$word,
  custom_stop)
stop_words <- stop_words[!duplicated(stop_words)]

words <- dplyr::anti_join(words, tibble::tibble(stop_words), by  = c("word" = "stop_words"))

word_count <- dplyr::count(words, loc_id, name, word) |>
  dplyr::arrange(desc(n))

source("_sentiment.r")

# Locality/area sentiments
locality_sentiment <- word_sent |>
  dplyr::select(-sentiment) |>
  dplyr::count(loc_id, sentiment_cat)  |>
  tidyr::pivot_wider(names_from = sentiment_cat, values_from = n, values_fill = 0) %>% 
  dplyr::mutate(sentiment = positivo - negativo, 
                sent_cat = dplyr::if_else(sentiment > 0, "positivo", "negativos"))

# Word frequencies
word_freq <- word_sent %>%
  dplyr::group_by(sentiment_cat) %>%
  dplyr::slice_head(n = 10) %>% 
  dplyr::ungroup() |>
  dplyr::mutate(word = forcats::fct_reorder(as.factor(word), n))

# ggplot2::ggplot(word_freq, ggplot2::aes(n, forcats::fct_reorder(word, n), fill = sentiment_cat)) +
#   ggplot2::geom_col(show.legend = FALSE) +
#   ggplot2::facet_wrap(sentiment_cat ~ area_cat, scales = "free_y") +
#   ggplot2::labs(x = "Contribution to sentiment",
#        y = NULL)

# Network of bigrams
bigrams <- tidytext::unnest_tokens(text_df, "trigram", text, token = "ngrams", n = 2) |>
  dplyr::filter(!is.na(trigram)) |>
  tidyr::separate(trigram, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  dplyr::filter(!word1 %in% stop_words, !word2 %in% stop_words) 


# Correlation co-occuring within sections
word_cors <- words %>%
  dplyr::group_by(word) %>%
  dplyr::filter(dplyr::n() >= 10) %>%
  widyr::pairwise_cor(word, loc_id, sort = TRUE)

word_cors <- dplyr::filter(word_cors, !is.nan(correlation), !is.infinite(correlation))

