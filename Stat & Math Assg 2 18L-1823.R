

main <- function() {
  
  
  datAll=read.table("trainData.txt") 	# Read train data
  testX = read.table("testData.txt") # Read test data
  
  f1 = datAll[,1]  
  r1 = datAll[1,]  	
  ncol(datAll)	
  x= datAll[,-1] 		
  
  labels = datAll[,ncol(datAll)]  	
  trainX = datAll[,-ncol(datAll)]	
  
  oneClass = labels==1		
  zeroClass = labels==0		
  
  oneDat = trainX[oneClass,]			
  zeroDat = trainX[zeroClass,]		
  
  probOf1Class1 = colMeans(oneDat) 		
  probOf1Class0 = colMeans(zeroDat)
  
  mle1_array = rep(0, dim(testX)[1]) # Array to store MLE values for class 1
  mle0_array = rep(0, dim(testX)[1]) # Array to store MLE values for class 0
  
  ################################ MLE ###################################################
  
  for (i in 1:dim(testX)[1]) #loop trough complete test data
  {
    
    Row1 = testX[i,]			# Get first row
    ind = Row1 == 1			
    Row1[ind] = probOf1Class1[ind]	 # Probability given 1

    
    index0 = Row1 == 0
    
    Row1[index0] = 1 - probOf1Class1[index0] # Probability given 0
    print('Class 1')
    mle1 = prod(Row1) # product gives MLE value
    print(mle1) 
    mle1_array[i] = mle1 # Store MLE value
    
    
    Row1_one = testX[i,] 
    
    index_one = Row1_one == 1		
    
    Row1_one[index_one] = probOf1Class0[index_one]	 # Probability given 1
   
    index0_one = Row1_one == 0
    
    Row1_one[index0_one] = 1 - probOf1Class0[index0_one] # Probability given 0
    
    mle0 = prod(Row1_one)
    print(mle0) 
    mle0_array[i] = mle0 # Store MLE value
    print(i)
    
  }
  
  

  
  print(mle1_array)
  
  ################################## MAP ###################################################
  
  labels = datAll[,ncol(datAll)] 
  
  prior1 = mean(labels) # get mean for 1 probility
  print(prior1)
  
  prior0 = (1 - prior1) # subtract 1 to get prior0
  print(prior0)
  
  map0_array = rep(0, dim(testX)[1])
  map1_array = rep(0, dim(testX)[1])
  
  for (i in 1:dim(testX)[1])
  {
    evidence = (mle0_array[i] * prior0) + (mle1_array[i] * prior1) 
    map1_array[i] = (mle1_array[i] * prior1)/evidence # Store values for 1 
    map0_array[i] = (mle0_array[i] * prior0)/evidence # Store values for 0
  }

  
  print(map0_array)
  print(map1_array)
  
}


main()






