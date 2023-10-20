library(RSelenium)

#"https://cdxapps.epa.gov/cdx-glenda/action/linkdownloads/ExcelDownloads?downloadFile=&uploadFileId=128"
# App sketch
# 1. navigate to https://cdx.epa.gov/
# 2. User ID and Password to login
# 3. Nav to https://cdxapps.epa.gov/cdx-glenda/action/querytool/querySystem
# 4. Interact with boxes to enter information
# 5. Click Query GLENDA Database
# 6. Click Download Results

# 1. navigate to https://cdx.epa.gov/
cdxURL <- "https://cdx.epa.gov/" 
pJS <- wdman::phantomjs(port = 4837L) # start phantomjs
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate(appURL)
remDr$findElement("id", "login")$sendKeysToElement(list("myusername"))
remDr$findElement("id", "pass")$sendKeysToElement(list("mypass"))
remDr$findElement("css", ".am-login-form input[type='submit']")$clickElement()

appURL <- 'http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=2'
remDr$navigate(appURL)
tableElem<- remDr$findElement("css", "table.datamedium")
res <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]])
pJS$stop()



