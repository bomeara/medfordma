library(RSelenium)
# rvest to convert to xml for easier parsing
library(rvest)
try(system("open -a Docker"))
try(system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1"))
# start a server and open a navigator (firefox by default)
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open() 
remDr$navigate("https://flynashville.com/flights")
print(remDr$findElement(using = "id", value = "receiving_div_id"))
# get source code
page <- remDr$getPageSource()

# convert to xml for easier parsing
page_xml <- read_html(page[[1]])
table <- html_table(page_xml %>% html_elements("table"))[[1]]
try(write.csv(table, file=paste0("/Users/bomeara/Dropbox/Nashville_Flight", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), ".csv")))
remDr$close()
