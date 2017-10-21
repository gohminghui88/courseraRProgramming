library("dplyr");

pollutantmean <- function(directory, pollutant, id=1:332) {
  
  files <- c(paste("00", 1:9, ".csv", sep=""), 
             paste("0", 10:99, ".csv", sep=""),
             paste(100:332, ".csv", sep=""));
  
  data <- read.csv(paste(directory, "001.csv", sep="/"));
  
  print("Reading files...");
  for (f in files) {
    print(f);
    data <- rbind(data, read.csv(paste(directory, f, sep="/")));
    
  }
  print("Read Completed. ")
  
  
  res <- -1;
  
  if("sulfate" %in% pollutant) {
    print("Lower Monitor ID: ")
    print(min(id));
    
    print("Upper Monitor ID: ")
    print(max(id));
    
    good <- complete.cases(data);
    data.one <- data[good, ];
    low <- min(id);
    high <- max(id);
    subset <- filter(data.one, ID >= low);
    subset2 <- filter(subset, ID <= high);
    
    res <- mean(subset2$sulfate);
    
    print("\n\nMean: ");
    print(res);
  }
  
  else if("nitrate"  %in% pollutant) {
    print("Lower Monitor ID: ")
    print(min(id));
    
    print("Upper Monitor ID: ")
    print(max(id));
    
    good <- complete.cases(data);
    data.one <- data[good, ];
    low <- min(id);
    high <- max(id);
    subset <- filter(data.one, ID >= low);
    subset2 <- filter(subset, ID <= high);
      
    res <- mean(subset2$nitrate);
    
    print("Mean: ");
    print(res);
    
  }
  
  return(res);
}


complete <- function(directory, id=1:332) {
  
  files <- c(paste("00", 1:9, ".csv", sep=""), 
             paste("0", 10:99, ".csv", sep=""),
             paste(100:332, ".csv", sep=""));
  
  temp <- data.frame(ID=integer(), nobs=integer());
  
  i <- 1;
  for(f in files) {
    
    print(f);
    data <- read.csv(paste(directory, f, sep="/"));  
    ID <- c(i);
    nobs <- c(nrow(na.omit(data)));
    row <- data.frame(ID, nobs);
    temp <- rbind(temp, row);
    
    i <- i + 1;
  }
  
  res <- data.frame(ID=integer(), nobs=integer());
  for(a in id) {
    res <- rbind(res, filter(temp, ID == a));
  }
  
  
  return(res);
  
  
  
}



corr <- function(directory, threshold=0) {
  
  files <- c(paste("00", 1:9, ".csv", sep=""), 
             paste("0", 10:99, ".csv", sep=""),
             paste(100:332, ".csv", sep=""));
  
  temp <- data.frame(ID=integer(), corr=double(), nobs=integer());
  
  i <- 1;
  for(f in files) {
    
    print(f);
    data <- read.csv(paste(directory, f, sep="/"));  
    data <- na.omit(data);
    
    ID <- c(i);
    corr <- c(cor(data$sulfate, data$nitrate));
    nobs <- c(nrow(data));
    row <- data.frame(ID, corr, nobs);
    
    temp <- rbind(temp, row);
    
    i <- i + 1;
  }
  
  temp <- filter(temp, nobs > threshold);
  res <- temp$corr;
  res2 <- as.vector(res);
  
  #return vector of length 0, not vector with 0 as element
  #if(length(res2) == 0) return(c(0));
  
  if(length(res2) == 0) { 
    res2 <- c(0);
    return(res2[-1]);
  }
  
  return(res2);
  
}