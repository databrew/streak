setwd('/home/joebrew/Documents/streak/')
options(repos = c(CRAN = "https://cran.rstudio.com/")) 
message('Getting new data from strava')
system('/home/joebrew/.virtualenvs/streak/bin/python2 get_data.py', intern = TRUE)
message('Deploying app')
rsconnect::deployApp(appDir = getwd(), appId = 265904)

library(mailR)
library(yaml)
pass = yaml.load_file('credentials.yaml')
pass = pass$email_password
sender <- "joebrew@gmail.com"
recipients <- c("joebrew@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Streak app was just redeployed",
          body = "Just letting you know",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "joebrew@gmail.com",            
                      passwd = pass, ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
  
