stopwords_en <- stopwords::stopwords(language = "en", source = "stopwords-iso")

save(stopwords_en, file = "data/stopwords_en.rda", compress = "xz")
