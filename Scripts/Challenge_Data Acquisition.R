Library(jsonlite)
#---Prajwal Kumar's comments
#This API returns the latest currency conversion from one base currency to another
#Here the base currency is EUR
#To/converting currency is INR
#Option to enter the amount (in base currency) is given too)
#Change the API request params (base, to & amount) to check for other popular currencies
#Try for USD to EUR
token <- "hafkonmbi2gq2jta8s6jo8551pppnma5gamas1kh6r84pubbep7um8"
curr <- GET(glue("https://anyapi.io/api/vl/exchange/convert?base=EUR&to=INR&amount=10&apiKey={tok en}"))
curr_list<- fromJSON(rawToChar(curr$content))
curr_list
