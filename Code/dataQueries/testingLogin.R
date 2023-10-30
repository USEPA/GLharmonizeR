library(rvest)
library(httr)
username  <- obfuscate(.rs.api.askForPassword("Username: "))
password <- obfuscate(.rs.api.askForPassword("Password: "))
url <- "https://practice.expandtesting.com/login"
url <- "https://cdx.epa.gov/CDX/login"
# passthruURL <- "https://cdxapps.epa.gov/cdx-glenda/action/querytool/querySystem"
s <- session(url) 
filled_form<-html_form_set(html_form(s)[[1]], 
                          LoginUserId = username,
                          LoginPassword= password
                          # username= obfuscate("practice"),
                          # password= obfuscate("SuperSecretPassword!")
)
test <- html_form_submit(filled_form)
html_text2(read_html(test))

# html_text2(read_html(s))
session_jump_to(s, logoutlink)
session_jump_to(s, "https://practice.expandtesting.com/secure")


library(httr2)
req <- request("https://practice.expandtesting.com") %>%
  req_url_path("/login") %>% 
  req_body_form(username=  "practice", password = "SuperSecretPassword!") %>%
  req_perform()


# test on basic
library(httr2)

req <- request("https://r-project.org") %>%
  req_headers("Accept" = "applications/json")
req_perform(req)
