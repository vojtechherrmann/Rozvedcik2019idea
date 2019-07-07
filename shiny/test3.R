# from <- "<herrmann.vojtech@gmail.com>"
# to <- "<herrmann.vojtech@gmail.com>"
# subject <- "Performance Result"
# body <- "This is the result of the test:"                     
# mailControl=list(smtpServer="cz-prg01a-dns01.chello.cz")
# 
# sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
library(gmailr)
use_secret_file("credentials.json")

mail_message <- mime() %>%
  to("barbora.hornikova@outlook.com") %>%
  from("herrmann.vojtech@gmail.com") %>%
  subject("test") %>%
  text_body("test_body")

send_message(mail_message)

