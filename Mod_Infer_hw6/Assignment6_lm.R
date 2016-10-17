options(stringsAsFactors = FALSE)
library(plyr)
library(magrittr)
course.url <- "http://kinglab.eeb.lsa.umich.edu/480"
urls <- paste0(course.url,"/data/",
               c("Thrips_imaginis_Davidson1948.csv",
                 "Parus_major_Wytham_Wood.csv"))
dat <- lapply(urls,read.csv,comment.char="#")
dat[[1]] %>%
  mutate(year=Sample.Date%/%1,
         month=round(100*(Sample.Date%%1)),
         time=year+month/12,
         pop=Population,
         Population=NULL,
         Sample.Date=NULL) -> thrips

dat[[2]] %>% rename(c(Sample.Date="year",Population="pop")) -> parus

parus = parus %>% 
  mutate(pop_1=c(log(parus[,"pop"][2:nrow(parus)]/parus[,"pop"][1:(nrow(parus)-1)]), "NA"))