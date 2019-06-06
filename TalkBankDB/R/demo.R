

# Load from files prefetched on the web.
loadFromWeb <- function () {
	print('Loading...  Please wait...');

	# Load participant table and add column names.
	participants <<- read.table("https://sla.talkbank.org/Rdata/TBDB_participants.xls", sep="\t", quote = "", comment.char = "", header=FALSE);
	colnames(participants) <<- c("filename", "path", "nameCode", "name", "role", "lang", "monthAge", "age", "gender", "type");


	# Load token table and add column names.
	tokens <<- read.table("https://sla.talkbank.org/Rdata/TalkBankDB.xls", sep="\t", quote = "", comment.char = "", header=FALSE);
	colnames(tokens) <<- c("filename", "path", "word", "nameCode", "uttNum", "wordIndex", "mor", "gra");


	# Only include rows with roles of Target_Child including a monthAge.
	participants <<- participants[participants$role=="Target_Child" & participants$monthAge!="-", ];


	# Join participants/tokens on path and nameCode columns.
	joined <<- merge(tokens, participants, by=c("filename", "nameCode"), all.x=FALSE, all.y=FALSE);


	print('Loading complete!');
}


# Load and initialize from local tab-delimited files downloaded from TalkBankDB
loadLocalFiles <- function (participants, tokens) {
	print('Loading...  Please wait...');

	# Load participant table and add column names.
	participants <- read.table(participants, sep="\t", quote = "", comment.char = "", header=FALSE);
	colnames(participants) <- c("filename", "path", "nameCode", "name", "role", "lang", "monthAge", "age", "gender", "type");


	# Load token table and add column names.
	tokens <- read.table(tokens, sep="\t", quote = "", comment.char = "", header=FALSE);
	colnames(tokens) <- c("filename", "path", "word", "nameCode", "uttNum", "wordIndex", "mor", "gra");


	# Only include rows with roles of Target_Child including a monthAge.
	participants <- participants[participants$role=="Target_Child" & participants$monthAge!="-", ];


	# Join participants/tokens on path and nameCode columns.
	joined <<- merge(tokens, participants, by=c("filename", "nameCode"), all.x=FALSE, all.y=FALSE);


	print('Loading complete!');
}



# Calculate and plot for each monthAge: (num occurrences of target word) / (number of words spoken at that age).
freqByAge <- function(targetWord) {
	ages <<- c();
	freqs <<- c();

	# For every monthAge in range 1:100 (Macwhinney dataset range is 13-92 months).
	for (i in 1:100) {
		# If there is data at this monthAge, calculate freq.
		if(sum(joined$monthAge == i) != 0) {
			targetWordCount <- sum(joined$word == targetWord & joined$monthAge == i);
			totalWordCount <- sum(joined$monthAge == i);

			frac <- targetWordCount / totalWordCount;

			ages <- c(ages, i);
			freqs <- c(freqs, frac);

			print(paste(targetWordCount, " / ", totalWordCount, " monthAge: ", i, " frac: ", frac));
		}
	}

	# Plot age vs word freq.
	plot(ages, freqs, pch = 16, cex = 1.3, col = "orange", main = paste("Target Word (", targetWord, ") Frequency by Age"), xlab = "Age in Months", ylab = "(FREQ TARGET WORD) / (ALL WORDS)")

	# Add regression line.
	abline(lm(freqs ~ ages), col = "blue")
}

