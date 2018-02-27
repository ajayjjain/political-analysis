fairfaxElectionData = read.csv('2016_Election_Results__USPresidentVicePresident.csv')

clintonPrecincts = subset(fairfaxElectionData, WINPARTY == "Clinton / Kaine (D)")
trumpPrecincts = subset(fairfaxElectionData, WINPARTY == "Trump / Pence (R)")

numberOfClintonPrecinctsWon = nrow(clintonPrecincts)
numberOfTrumpPrecinctsWon = nrow(trumpPrecincts)
precinctsWon = data.frame(candidate = c("Clinton", "Trump"), numberOfPrecinctsWon = c(numberOfClintonPrecinctsWon, numberOfTrumpPrecinctsWon))
ggplot(precinctsWon) + geom_bar(aes(x = candidate, y = numberOfPrecinctsWon), stat = "identity", fill = c("darkblue", "red")) + xlab("Presidential Candidate") + ylab("Number of Precincts Won in Fairfax County")

clintonVotesTotal = sum(fairfaxElectionData$NUMVOTES1, na.rm = TRUE)
trumpVotesTotal = sum(fairfaxElectionData$NUMVOTES2, na.rm = TRUE)
johnsonVotesTotal = sum(fairfaxElectionData$NUMVOTES3, na.rm = TRUE)
steinVotesTotal = sum(fairfaxElectionData$NUMVOTES4, na.rm = TRUE)
mcmullinVotesTotal = sum(fairfaxElectionData$NUMVOTES5, na.rm = TRUE)

votes = data.frame(candidate = c("Clinton", "Trump", "Johnson", "Stein", "McMullin"), votesObtained = c(clintonVotesTotal, trumpVotesTotal, johnsonVotesTotal, steinVotesTotal, mcmullinVotesTotal))
ggplot(votes) + geom_bar(aes(x = candidate, y = votesObtained), stat = "identity", fill = c("darkblue", "yellow", "purple", "lightgreen", "red")) + scale_y_discrete(limits = c(5000, 10000, 15000, 20000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)) + xlab("Presidential Candidate") + ylab("Votes Received in Fairfax County")
options(scipen=10)


ggplot(fairfaxElectionData, aes(x = OBJECTID, y = PERCVOTE1)) + geom_boxplot() + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ ylab("Percent of Vote") + scale_y_continuous(labels = function(x)x * 100) + coord_flip() + ggtitle("Boxplot of the Percentage of Votes Cast for Hillary Clinton")
ggplot(fairfaxElectionData, aes(x = OBJECTID, y = PERCVOTE2)) + geom_boxplot() + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ ylab("Percent of Vote") + scale_y_continuous(labels = function(x)x * 100) + coord_flip() + ggtitle("Boxplot of the Percentage of Votes Cast for Donald Trump")
ggplot(fairfaxElectionData, aes(x = OBJECTID, y = PERCVOTE3)) + geom_boxplot() + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ ylab("Percent of Vote") + scale_y_continuous(labels = function(x)x * 100) + coord_flip() + ggtitle("Boxplot of the Percentage of Votes Cast for Gary Johnson")
ggplot(fairfaxElectionData, aes(x = OBJECTID, y = PERCVOTE4)) + geom_boxplot() + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ ylab("Percent of Vote") + scale_y_continuous(labels = function(x)x * 100) + coord_flip() + ggtitle("Boxplot of the Percentage of Votes Cast for Jill Stein")
ggplot(fairfaxElectionData, aes(x = OBJECTID, y = PERCVOTE5)) + geom_boxplot() + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ ylab("Percent of Vote") + scale_y_continuous(labels = function(x)x * 100) + coord_flip() + ggtitle("Boxplot of the Percentage of Votes Cast for Evan McMullin")

