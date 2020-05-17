# Alex Van Plantinga
# 5/16/2020


# use of gutenbergr to compare
# 3 big Continental Rationalists 
# Descartes, Leibniz, Spinoza
# vs. 3 big British Empiricists
# Locke, Berkeley, Hume

# Inspired by ....
# https://www.tidytextmining.com/tidytext.html
# https://books.psychstat.org/textmining/association-of-words.html
# https://www.tidytextmining.com/nasa.html


library(dplyr)
library(tidytext)
data(stop_words)

library(dplyr)
library(stringr)
library(ggplot2)
library(gutenbergr)

gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Spinoza") ),]
#919 THE ETHICS
gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Leibniz") ),]
# 17147 Theodicy
gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Descartes") ),]
#59 DISCOURSE ON THE METHOD...

rationalism = gutenberg_download(c(17147, 919, 59))
spinoza = gutenberg_download(919)
leibniz = gutenberg_download(17147)
descartes = gutenberg_download(59)

tidy_spinoza = spinoza %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_leibniz = leibniz %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_descartes = descartes %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

library(tidyr)

frequency <- bind_rows(mutate(tidy_spinoza, author = "Spinoza"),
                       mutate(tidy_leibniz, author = "Leibniz"), 
                       mutate(tidy_descartes, author = "Descartes")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Spinoza`:`Leibniz`)


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Descartes`, color = abs(`Descartes` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Descartes", x = NULL)

########
# British Empiricism
gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Hume") ),]
#4320 AN ENQUIRY CONCERNING THE PRINCIPLES OF MORALS
gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Berkeley") ),]
# 4723 A Treatise Concerning the Principles of Human Knowledge
gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Locke") ),]
# 10615 AN ESSAY CONCERNING HUMANE UNDERSTANDING

epiricism = gutenberg_download(c(4320, 4723, 10615))

hume = gutenberg_download(4320)

berkeley = gutenberg_download(4723)

locke = gutenberg_download(10615)

tidy_hume = hume %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tidy_berkeley = berkeley %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tidy_locke = locke %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


library(tidyr)

frequency <- bind_rows(mutate(tidy_hume, author = "Hume"),
                       mutate(tidy_berkeley, author = "Berkeley"), 
                       mutate(tidy_locke, author = "Locke")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Hume`:`Berkeley`)


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Locke`, color = abs(`Locke` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Locke", x = NULL)


