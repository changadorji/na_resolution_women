library(tidytext)
library(stringr)
library(tidyverse)       
library(quanteda)     
library(readtext)         
library(extrafont)

files <- list.files(pattern = 'pdf')
files_1 <- readtext(files)

#convert to corpus
na_resolution <- corpus(files_1)
na_data <- convert(na_resolution, to='data.frame')

#usetidy text to convert to sentences
na_token_sentence <- na_data %>% 
  unnest_tokens(sentence, text, token = 'sentences')

#look at women
na_token_sentence <- na_token_sentence %>%
  mutate(sentence = str_to_lower(sentence),
         rownumber = row_number(),
         women = ifelse(str_detect(sentence, 'women|girls|woman|girl'), 1, 0),
         word_ocur = rownumber/max(rownumber))

#select the 349 sentences that contain women
women_sentence <- 
  na_token_sentence %>% 
  filter(women==1) %>% 
  mutate(word_ocur = round(word_ocur*100, 0))


women_sentence %>%
  count(word_ocur) %>%
  ggplot(aes(word_ocur, n))+
  geom_line(size=1, colour='tomato')+
  geom_point(size=3, colour='tomato', alpha=.7)+
  annotate('text', x = 50, y = 30, 
           label='the number of word occurrence \n such as women and girls \n increases after three quater \n section of the NA resolution',
           size=6.5, colour='blue', alpha=.7)+
  geom_curve(aes (x=50, xend = 65, y = 23, yend = 10), arrow = arrow(), size=2, colour='blue', alpha=.7)+
  labs(title = 'Word Occurrence by Section of NA Resolution',
       x='Section of NA Resolution',
       y='')