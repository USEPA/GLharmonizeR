library(rvest)
library(httr)


url <- "https://owshiny.epa.gov/nars-data-download/"
s <- session(url)
form <- html_form(s)[[2]]
filled_form<-html_form_set(form,
                          ,
                          LoginPassword= password
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
