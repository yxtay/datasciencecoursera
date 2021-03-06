Data Science Capstone Pitch
========================================================
author: kitdia

N-Gram Language Model
========================================================

- based on the n-grams obtained from blogs, news and Twitter sources
- texts were preprocessed to remove special characters, split into sentences before punctuations were removed
- the sentences were then tokenized into bigrams and trigrams which were then counted
- the n-grams were then split into leading words and suggestions, with counts for each suggestion normalised by leading words to compute probabilities
- n-grams with count of 1 and leading words with only 1 suggestion were treated as noise and removed
- these were all contained in 2 data frames separately for bigrams and trigrams

Prediction Algorithm
========================================================

- input text is cleaned in the similar manner as the sources were and the last 2 words are extracted
- the words are then used to search through data frames previously contructed to obtain suggestions
- the suggestion with the highest probability from both data frames is used
- in the event that no suggestion was obtained in the previous step, then the unigram with the highest probability is used
- unigram probabilities were computed by normalising the counts of unique leading words by suggestions from the bigrams obtained

My App
========================================================

https://kitdia.shinyapps.io/capstone

- intuitive and simple interface
- generates next-word prediction in short amount of time
- saves keystrokes for user

In the Pipeline
========================================================

- retain contractions in predictions (i.e. I'd, she's, etc.)
- extend ngram model to quadgram for better contextual prediction
- implement a more elegant backoff algorithm that incorporates information from all the levels of n-grams
- multiple word suggestions

The amount of self-learning required for this project, coupled with starting out in my new job,
has certainly made it quite challenging. Nonetheless, I enjoyed completing the Data Science Specialisation.
Hope it has been a great journey for you too!
