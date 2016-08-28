library(rvest)
library(knitr)
library(tm)
library(SnowballC)
library(wordcloud)


CompileInformation <- function(url) {
	page <- read_html(url)
	info <- data.frame(t(html_text(html_nodes(page, ".job-posting-dd"))))
	colnames(info) <- gsub(" ","_",html_text(html_nodes(page, ".job-posting-dt")))
	info$url=url
	return(info)
}

ExtractJobs <- function(existing.jobs=data.frame(url="none")) {

	urls <- c("http://wfscjobs.tamu.edu/job-board/", paste("http://wfscjobs.tamu.edu/job-board/page/", c("2", "3", "4"), sep=""))
	breakrun=FALSE
	for (i in sequence(length(urls))) {
		if(breakrun) {
			break()
		}
		links <- html_text(html_nodes(read_html(urls[i]), xpath= "//a/@href"))
		links <- links[grepl('http://wfscjobs.tamu.edu/jobs/', links)]
		for (l in sequence(length(links))) {
			if(links[l] %in% existing.jobs$url) {
				#breakrun=TRUE
				#break()
			} else {
				new.row <- CompileInformation(links[l])
				Sys.sleep(runif(1,5,15))
				existing.jobs <- merge(existing.jobs, new.row, all=TRUE)
			}
			print(paste("done",l,"of",length(links),"on page",i))

			save(existing.jobs, file="jobs.rda")
		}
		save(existing.jobs, file="jobs.rda")

	}
	return(existing.jobs)
}

ProcessForWordCloud <- function(x) {
	#borrowing liberally from https://www.r-bloggers.com/building-wordclouds-in-r/
	corpus <- Corpus(VectorSource(x))
	corpus <- tm_map(corpus, PlainTextDocument)
	corpus <- tm_map(corpus, removePunctuation)
	corpus <- tm_map(corpus, removeNumbers)
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, removeWords, c('the', 'this', stopwords('english')))
	return(corpus)
}


load("jobs.rda")
existing.jobs <- ExtractJobs(existing.jobs)
#library(knitr)
#cat(kable(existing.jobs, format="markdown"), file="jobs.md")

write.csv(existing.jobs, file="jobs.csv")

png(file="Qualifications.png")
wordcloud(ProcessForWordCloud(existing.jobs$Qualifications), max.words=100)
dev.off()

png(file="Description.png")
wordcloud(ProcessForWordCloud(existing.jobs$Description), max.words=100)
dev.off()

system('git commit -m"update from run" -a')
system('git push')
