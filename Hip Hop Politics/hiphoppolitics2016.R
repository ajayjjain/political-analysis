hiphop = read.csv("genius_hip_hop_lyrics.csv")
ggplot(hiphop) + geom_bar(aes(x = candidate, fill = candidate)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1))

hiphopPositive = subset(hiphop, sentiment == "positive")
ggplot(hiphopPositive) + geom_bar(aes(x = candidate, fill = candidate)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)) + ylab("Number of times mentioned positively")

ggplot(hiphop) + geom_freqpoly(aes(x = album_release_date)) + xlim(1988,2016)

trump = subset(hiphop, candidate == "Donald Trump")

ggplot(trump) + geom_bar(aes(x = sentiment, fill = theme)) + xlab("Sentiment of Donald Trump in a Hip Hop Song") + ylab("Number of Songs")
ggplot(trump) + geom_bar(aes(x = theme, fill = sentiment)) + xlab("Context of which Donald Trump is talked about") + ylab("Number of Songs")
trumpPositive = subset(trump, sentiment == "positive")
trumpNegative = subset(trump, sentiment == "negative")
trumpNeutral = subset(trump, sentiment == "neutral")
ggplot() + geom_smooth(data = trumpPositive, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
  geom_smooth(data = trumpNeutral, aes(x = album_release_date, y = ..count..), stat = "bin", color = "black") + 
  geom_smooth(data = trumpNegative, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + xlab("Year") + ylab("Number of Trump mentions")
trumpMoneyHotels = subset(trump, theme == "money" | theme == "hotel")
trumpPolitics = subset(trump, theme == "political")
ggplot() + geom_smooth(data = trumpMoneyHotels, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
  geom_smooth(data = trumpPolitics, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + xlab("Album Release Date") + ylab("Number of Trump mentions")

clinton = subset(hiphop, candidate == "Hillary Clinton")
clintonPositive = subset(clinton, sentiment == "positive")
clintonNegative = subset(clinton, sentiment == "negative")
clintonNeutral = subset(clinton, sentiment == "neutral")
ggplot(clinton) + geom_bar(aes(x = theme, fill = sentiment)) + xlab("Context of which Hillary Clinton is talked about") + ylab("Number of Songs")
ggplot(clinton) + geom_bar(aes(x = sentiment, fill = theme ))  + xlab("Sentiment of Hillary Clinton in a Hip Hop Song") + ylab("Number of Songs")
ggplot(clinton) + geom_bar(aes(x = album_release_date, fill = sentiment)) + xlab("Year") + ylab("Number of Hillary Clinton mentions")

othercandidates = subset(hiphop, candidate != "Hillary Clinton" & candidate != "Donald Trump")
ggplot(othercandidates) + geom_bar(aes(x = candidate, fill = sentiment)) + xlab("Candidate") + ylab("Number of times mentioned") + scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

filterHipHop = hiphop %>% group_by(artist) %>% filter(n() >= 3)
ggplot(filterHipHop) + geom_bar(aes(x = artist, fill = sentiment)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)) + xlab("Artist") + ylab("Number of political candidate mentions")
ggplot(filterHipHop) + geom_bar(aes(x = artist, fill = candidate)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)) + xlab("Artist") + ylab("Number of political candidate mentions")
ggplot(filterHipHop) + geom_bar(aes(x = artist, fill = theme)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)) + xlab("Artist") + ylab("Number of political candidate mentions")

filterHipHop2 = hiphop %>% group_by(artist) %>% filter(n() < 3)
ggplot(filterHipHop2) + geom_bar(aes(x = artist, fill = sentiment)) + theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)) + xlab("Artist") + ylab("Number of political candidate mentions")
