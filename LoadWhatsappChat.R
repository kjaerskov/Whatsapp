# title: "Get Whatsapp chat into R"
# author: "Frederik Holmelund Kjaerskov"
# date: "7 March 2018"

cFile <- read.csv("Whatsappchat.csv",   header = FALSE, sep = ";",
                  na.strings="", stringsAsFactors = FALSE, fill=TRUE, encoding = "UTF-8")