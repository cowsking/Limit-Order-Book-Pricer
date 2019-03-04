pricer <- function(infile, outfile, targetsize){
  input <- read.table(infile, fill = TRUE, stringsAsFactors = FALSE)
  colnames(input) <- c("timestamp", 'type', "order-id", "side", "price", "size")
  totalBidSize <- 0
  totalAskSize <- 0
  bidBook <- data.frame()
  askBook <- data.frame()
  #for(i in 1:nrow(input)){
  for(i in 1:3){
    if(input[i,'type'] == 'A'){
      if(input[i, 'side'] == 'B'){
        bidBook <- rbind(bidBook,input[i,c(2,4,5)])
        print(bidBook)
        print("bidBook\n")
      }
      if(input[i, 'side'] == 'S'){
        askBook <- rbind(askBook,input[i,c(2,4,5)])
        print(askBook)
        print("askBook\n")
      }
    #这里少了一个排序方法
    }
    if(input[i, 'type'] == 'R'){
      
    }
    
  }
  
  
}

pricer("sample1_in.txt", 1,0)


