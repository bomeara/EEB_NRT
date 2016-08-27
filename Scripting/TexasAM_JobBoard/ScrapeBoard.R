library(rvest)


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
			new.row <- CompileInformation(links[l])
			Sys.sleep(runif(1,5,15))
			if(new.row$url %in% existing.jobs$url) {
				breakrun=TRUE
				break()
			} else {
				existing.jobs <- merge(existing.jobs, new.row, all=TRUE)
			}
			print(paste("done",l,"of",length(links),"on page",i))
			
			save(existing.jobs, file="jobs.rda")
		}
		save(existing.jobs, file="jobs.rda")

	}
	return(existing.jobs)
}

existing.jobs <- ExtractJobs()

