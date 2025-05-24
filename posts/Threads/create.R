library(tidyverse)
load("~/Documents/Misc/twitterstuff/twitterarchive/tweets_posts.rda")

setwd("~/Documents/Websites/blog/posts/Threads")

threads <- character(0)
for (thread in posts){
  if (nrow(thread) > 1){
    id <- thread$id[1]
    threads <- c(threads, str_glue("\n\n{{{{< tweet ed_hagen {id} >}}}}\n\n"))
  }
}
write_lines(rev(threads), "_threads.qmd")

# Threads since I downloaded the twitter archive

latest <- 
"https://x.com/ed_hagen/status/1921260022433989026
https://x.com/ed_hagen/status/1920125741422825587
https://x.com/ed_hagen/status/1917714960685359184
https://x.com/ed_hagen/status/1911070467445932282
https://x.com/ed_hagen/status/1908278623569949173
https://x.com/ed_hagen/status/1905082536684126500
https://x.com/ed_hagen/status/1903906149621190827
https://x.com/ed_hagen/status/1865470420927025326
https://x.com/ed_hagen/status/1859716008556233174
https://x.com/ed_hagen/status/1855398741827875033
https://x.com/ed_hagen/status/1853815811783184630
https://x.com/ed_hagen/status/1832423877622014003"

x <- 
  latest |> 
  str_remove_all("https://x.com/ed_hagen/status/") |> 
  str_split_1("\n") 

x2 <- paste("{{< tweet ed_hagen", x, " >}}\n\n")

cat(x2)
