# Two queries
# First send user credentials and receive token
# x make xml template
# x set new values
# - send query
# - recieve response
# - loop it
# Second, query database and recive data
# - make xml template
# - input token
# - send query
# - recieve response
# - store data
# - loop it

library(httr2)
library(xml2)
# Read xml2 template
text <- xml2::read_xml("Data/glendaRequestTokenTemplate.xml")
# Set the username and password
idNode <- xml2::xml_find_first(text, ".//ns0:userId")
xml2::xml_text(idNode) <- "christiancoffman"
credNode <- xml2::xml_find_first(text, ".//ns0:credential")
xml2::xml_text(credNode) <- "CDXDevcc01!"

xml2::write_xml(text, "Data/glendaRequestTokenTemporary.xml")

# Send query
req <- httr2::request("https://cdxnodengn.epa.gov/ngn-enws20/services/NetworkNode2Service?wsdl")
token <- req %>%
  httr2::req_headers(
    `User-Agent` = "Zeep/4.2.1 (www.python-zeep.org)",
    `Accept-Encoding` = c("gzip", "deflate", "br", "zstd"),
    Accept = "*/*",
    Connection =  "keep-alive",
    SOAPAction =  "",
    `Content-Type` =  'application/soap+xml; charset=utf-8; action=""',
    Cookie =  'JSESSIONID=63751690DC5CDF49F835014AE28C004E; cdx-prod-coreservices=1707921987.09.32.283202|aabbe373016d0cf673fd1f826041c98d',
    `Content-Length` =  495
  ) %>%
  httr2::req_body_raw("Data/glendaRequestTokenTemporary.xml") %>%
  # Dry run looks like xml is properly changed
  # httr2::req_dry_run()
  httr2::req_perform()

test <- resp_raw(token)
test <- httr2::resp_body_xml(token)

xml2::xml_find_first(test, ".//securityToken")

write_xml(test, "test.xml")



### CURL attempt
library(RCurl)

headerFields =c(
  `User-Agent` = "Zeep/4.2.1 (www.python-zeep.org)",
  `Accept-Encoding` = c("gzip", "deflate", "br", "zstd"),
  Accept = "*/*",
  Connection =  "keep-alive",
  SOAPAction =  "",
  `Content-Type` =  'application/soap+xml; charset=utf-8; action=""',
  Cookie =  'JSESSIONID=63751690DC5CDF49F835014AE28C004E; cdx-prod-coreservices=1707921987.09.32.283202|aabbe373016d0cf673fd1f826041c98d',
  `Content-Length` =  495
)

body = '<?xml version="1.0" encoding="utf-8"?>
<soap-env:Envelope xmlns:soap-env="http://www.w3.org/2003/05/soap-envelope">
  <soap-env:Body>
    <ns0:Authenticate xmlns:ns0="http://www.exchangenetwork.net/schema/node/2">
      <ns0:userId>christiancoffman</ns0:userId>
      <ns0:credential>CDXDevcc01!</ns0:credential>
      <ns0:domain xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true"/>
      <ns0:authenticationMethod>password</ns0:authenticationMethod>
    </ns0:Authenticate>
  </soap-env:Body>
</soap-env:Envelope>'

test <- curlPerform(url = "https://cdxnodengn.epa.gov/ngn-enws20/services/NetworkNode2Service?wsdl",
                          httpheader = headerFields,
                          postfields = body
                          )
