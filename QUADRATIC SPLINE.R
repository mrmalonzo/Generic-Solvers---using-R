GaussJordan <- function(system){ #main Gauss Jordan Function
  TrueAug=system#get its numeric equivalent
  #print(TrueAug)
  for(i in 1:nrow(TrueAug)){ #first i loop
    if(i!=nrow(TrueAug)){ #if it is not the last
      # print(i);
      pivot_row=max(abs(TrueAug[i:nrow(TrueAug),i]))+i-1; #get the pivot row
      #print(TrueAug[i:nrow(TrueAug),i]);
      
      #print(pivot_row);
      
      
      
      if(TrueAug[pivot_row,i]==0){ #if pivot is equal to 0 then no solution
        print("Your Equation has no solution!")
        break
      }
      else{ 
        
        temp=TrueAug[i,]; #swap
        TrueAug[i,]=TrueAug[pivot_row,];
        TrueAug[pivot_row,]=temp;
        #  print(TrueAug);
      }
    }
    TrueAug[i,]=TrueAug[i,]/TrueAug[i,i]; #divide it to make your diags only 1
    #print(TrueAug[i,])
    for(j in 1:nrow(TrueAug)){ #nested loop
      if(i==j){ #don't change the diags
        next;
      }
      #print(TrueAug[j,i])
      normalized_row=TrueAug[j,i]*TrueAug[i,] #get the normalized row
      # print(normalized_row)
      TrueAug[j,]=TrueAug[j,]-normalized_row #subtract the normalized row to the original
      # print(TrueAug)
    }
    
  }
  #print("Answer:") #print the answers
  print(TrueAug);
  return(TrueAug[,ncol(TrueAug)]) #return the answer
  
}

Sort <- function(matrix){ #function that sorts my matrix
  #print(matrix); 
  new_matrix=matrix[order(matrix[,1]),]
  #print (new_matrix);
  # print(new_matrix)
  return (new_matrix)
}


QuadraticSpline <- function (myData, estimatedvalue){
  
  Data=Sort(myData); #sort the data in ascending order
  print(Data);
  
  independent=Data[,1]; #get the dependent and independent data
  dependent=Data[,2];
  print(independent)
  print(dependent)
  
  n=length(independent); #data points
  numofintervals=n-1; #length of intervals
  
  whatformulatouse=0;
  for(i in 1:numofintervals){
    if(independent[i]<=estimatedvalue && independent[i+1]>=estimatedvalue){ #to know which interval the estimated value belongs to
      whatformulatouse=i; #what interval the estimated value belongs to
      break;
    }else{ 
      print("ESTIMATED VALUE MUST BE BETWEEEN THE INTERVALS");
    }
  }
  if(whatformulatouse==0){#if the given estimated value by the user is not in the interval of the given x, return immediately and ask for another interval between the values
    wronganswerlist=list("PLEASE GIVE AN ESTIMATED VALUE BETWEEN THE INTERVALS OF GIVEN X", "PLEASE GIVE AN ESTIMATED VALUE BETWEEN THE INTERVALS OF GIVEN X");
    return( wronganswerlist);
  }
  print("this is the interval to use:")
  print(whatformulatouse);
  
  Matrix=matrix(c(0), nrow = (3*(numofintervals))-1, ncol = 3*numofintervals);
  
  # print(Matrix);
  
  counter=1; #used for my col value updater
  counter2=1; #used for my row value updater
  
  for(i in 2:(n-1)){ #insert the internal nodes using the formula given and directly putting them into the matrix
    for(j in 1:2){
      if(i==2 && j==1){ #beacause a1=0, adjust
        Matrix[counter2, counter] = independent[i]; #place th variables according to the formula made
        counter = counter + 1; #adjust the counter
        Matrix[counter2, counter] = 1;
        counter = counter + 1;
      }
      else{ #if its not the first, solve it normally
        Matrix[counter2, counter] = (independent[i])^2;
        counter=counter+1;
        Matrix[counter2, counter] = independent[i];
        counter = counter + 1;
        Matrix[counter2, counter] = 1;
        counter = counter + 1;
        # print(counter)
      }
      Matrix[counter2, ncol(Matrix)]= dependent[i];
      counter2=counter2 + 1; #update counter 2
    }
    # print(i)
    if(i==2){ #if its the first iteration, updatee the coutner differently
      counter=i+1; #update the counter
    }else{ #update normally
      counter=counter-3;
    }
    
  }
  
  #insert end knots
  Matrix[counter2, 1] = independent[1]; #insert the first end knot
  Matrix[counter2, 2] = 1;
  Matrix[counter2, ncol(Matrix)] = dependent[1]
  counter2=counter2+1;
  Matrix[counter2, counter] = independent[n] ^ 2; #insert the 2nd end knot
  counter=counter+1;
  Matrix[counter2, counter] = independent[n];
  counter=counter+1;
  Matrix[counter2, counter] = 1;
  Matrix[counter2, ncol(Matrix)] = dependent[n]
  counter2=counter2 + 1;
  
  print(Matrix);
  
  counter=1; #insert the last few equations in the table
  print(counter2)
  
  for(i in 2:(n-1)){
    for(j in 1:2){
      if(i==2 && j==1){ #if its the first iteration since, a1=0
        Matrix[counter2, counter] = 1;
        counter = counter + 2;
      }else if(j==2){ #if its the equal to formula
        Matrix[counter2, counter] = -(2*(independent[i]));
        counter = counter + 1;
        Matrix[counter2, counter] = -1;
      }else{ # if its the normal formula
        Matrix[counter2, counter] = (2*(independent[i]));
        counter = counter + 1;
        Matrix[counter2, counter] = 1;
        counter = counter + 2;
      }
      
    }
    counter2 = counter2 + 1; #update the counter
    if(i==2){ #if its the first iteration, update it differently since a1=0
      counter=i+1;
    }else{ # if its the second or above, update normally
      counter=counter-1;
    }
  }
  print(Matrix);
  solutions=GaussJordan(Matrix); #use gauss jordan to solve the matrix
  
  list=list(); #create a list to be the temporary storage of the functions that will be built
  
  counter=3; #counter per function
  for(i in 1:numofintervals){
    list[i]<-paste("function (x) ", sep=""); #first paste the function
    if(i==1){ #if its the first formula
      list[i]<-paste(list[i], solutions[1], " * x +" ," ",sep="");
      list[i]<-paste(list[i], solutions[2] ," ",sep="");
    }else{ #if its the second or more formula
      list[i]<-paste(list[i], solutions[counter], " * x ^ 2 +" ," ",sep="");
      counter=counter+1;
      list[i]<-paste(list[i], solutions[counter], " * x +" ," ",sep="");
      counter=counter+1;
      list[i]<-paste(list[i], solutions[counter] ," ",sep="");
      counter=counter+1;
    }
  }
  print(list[whatformulatouse]);
  f=eval(parse(text=list[whatformulatouse])); #parse the formmula that is needed for the answer
  functionanswer=f(estimatedvalue); #get the answer
  print(functionanswer)
  answers=list(f, functionanswer); #store the funtion and answer in a list
  return(answers); #return the answers for the use of the GUI
  
}