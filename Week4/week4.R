library(dplyr);

#outcome <- read.csv("D:/MyCourses/RProgramming/Week4/outcome-of-care-measures.csv", colClasses = "character");

#head(outcome);
#nrow(outcome);
#names(outcome);

#outcome.11 <- as.numeric(outcome[,11]);
#hist(outcome.11);

data <- read.csv("D:/MyCourses/RProgramming/Week4/outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE );

best <- function(state, outcome) {
  
  #check whether state and outcome are valid
  if(length(which(data$State == state)) == 0) stop("invalid state");
  
  o <- c("heart attack", "heart failure", "pneumonia");
  if(length(which(o == outcome)) == 0) stop("invalid outcome");
  
  #if valid, get rows where state = state
  data.sub <- filter(data, State == state);
  
  opt <- -1;
  if(outcome == "heart attack") opt = 11;
  if(outcome == "heart failure") opt = 17;
  if(outcome == "pneumonia") opt = 23;
  
    
  minMortality <- min(na.omit(data.sub[,opt]));
  #print(minMortality);
  
  data.sub <- data.sub[data.sub[,opt] == minMortality,];  
  res <- data.sub[,2];
  res <- sort(res);
  
  return(res);
}




rankhospital <- function(state, outcome, num="best") {
  
  #check whether state and outcome are valid
  o <- c("heart attack", "heart failure", "pneumonia");
  if(length(which(o == outcome)) == 0) stop("invalid outcome");
  if(length(which(data$State == state)) == 0) stop("invalid outcome");
  
  
  opt <- -1;
  if(outcome == "heart attack") opt = 11;
  if(outcome == "heart failure") opt = 17;
  if(outcome == "pneumonia") opt = 23;
  
  
  data.sub <- filter(data, State == state);
  data.sub <- data.sub[data.sub[, opt] != "Not Available", ];
  data.sub <- data.sub[order(data.sub[, opt], data.sub[, 2]), ];
  data.sub <- data.sub[(!duplicated(data.sub[, opt]) | !duplicated(data.sub[, opt], fromLast=TRUE)), ];
  
  df <- data.frame(name=data.sub[, 2], mortality=data.sub[, opt], state=data.sub[, 7], rank=c(1:nrow(data.sub)));
  
  if(num == "worst") { df <- tail(na.omit(df), n=1); }
  else if(num == "best") { df <- head(na.omit(df), n = 1); }
  else {
    if(num > max(df$rank)) stop("num is greater than rank in state. ");
    
    df <- df[df$rank==num, ];
  }
  
  return(df);
}

rankall <- function(outcome, num="best") {
  
  #check whether state and outcome are valid
  
  o <- c("heart attack", "heart failure", "pneumonia");
  if(length(which(o == outcome)) == 0) stop("invalid outcome");
  
  opt <- -1;
  if(outcome == "heart attack") opt = 11;
  if(outcome == "heart failure") opt = 17;
  if(outcome == "pneumonia") opt = 23;
  
  #if valid, get rows where state = state
  
  data.sub <- data[data[, opt] != "Not Available", ];
  data.sub <- data.sub[order(data.sub[, 7], data.sub[, opt], data.sub[, 2]), ];
  #data.sub <- data.sub[order(data.sub[, opt], data.sub[, 7], data.sub[, 2]), ];
  df <- data.frame(name=data.sub[, 2], mortality=data.sub[, opt], state=data.sub[, 7]);
  #return(df);
  #df[!(duplicated(df[c("mortality","state")]) | !duplicated(df[c("mortality","state")], fromLast=TRUE)) , ];
  
  
  if(num == "best")
  {
    d <- by(df, df["state"], head, n=1);
    df <- Reduce(rbind, d);
    
    return(df);
  }
  
  else if(num == "worst")
  {
    d <- by(df, df["state"], tail, n=1);
    df <- Reduce(rbind, d);
    
    return(df);
  }
  
  else
  {
    d <- by(df, df["state"], getRow, n=num);
    df <- Reduce(rbind, d);
    
    return(df);
  }
  
  return (NULL);
}

getRow <- function (df, n) {
  
  res <- df[n, ];
  
  if(nrow(res) == 0) return(NULL);
  
  return(res);
}




best("TX", "heart attack");
best("TX", "heart failure");
best("MD", "heart attack");
best("MD", "pneumonia");
best("BB", "heart attack");
best("NY", "hert attack");

#res <- rankhospital("TX", "heart failure", 4);
#res

#res <- rankhospital("MD", "heart attack", "worst");
#res

#res <-  rankhospital("MN", "heart attack", 5000);
#res


#res3 <- head(rankall("heart attack", 20), 10)
#res3

#res3 <- tail(rankall("pneumonia", "worst"), 3)
#res3

#res3 <- tail(rankall("heart failure"), 10)
#res3