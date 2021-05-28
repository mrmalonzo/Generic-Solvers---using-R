#pasok yung d to d-1
#if hindi pantay yung length and if degree is less than 1 then NA

# MARLON RIVERA MALONZO           05/09/2019    10:31pm
# 2018-07148  AB-3L


AugCoeffMatrix <- function (system){
  constants=c(); #vectors needed
  cnames=c();
  rnames=c();
  counter=0;
  
  #========================================================================= //getting the variables
  for(x in 1:length(system)){
    x=deparse(system[[x]])[2]
    #print(x);
    eq=strsplit(x, "\\+"); #split my equations into +
    
    
    #print(eq[[1]][2]);
    for(y in 1:length(eq[[1]])){ #get my eq 1 one by one divided by the plus symbol
      final=strsplit(eq[[1]][y], "\\*");
      #print(final[[1]][1]);
      
      if(y!=length(eq[[1]]) && counter==0){
        constants=c(constants, final[[1]][2]);
        
      }
    }
    counter=counter+1;
  }
  
  print("Variables:"); ## print the variables
  print(constants);
  
  for(p in 1:length(system)){ #to get the column names
    cnames=c(cnames, p);
  }
  rnames=c(constants,"RHS"); # to get the row names
  #print(cnames);
  #print(rnames);
  variables=matrix(c(0), nrow=length(system), ncol=length(constants)+1, dimnames=list(c(cnames),c(rnames))); #create the matrix
  #======================================================================== // getting the constants
  for(i in 1:length(system)){ ##putting the values in the matrix
    z=deparse(system[[i]])[2]
    #print(z);
    eqq=strsplit(z, "\\+"); #split my equations into +
    
    
    #print(eqq[[1]][1]);
    for(j in 1:length(eqq[[1]])){ #get my eq 1 one by one divided by the plus symbol
      finals=strsplit(eqq[[1]][j], "\\*"); # further split it into *
      #print(finals);
      for(m in 1:length(constants)){ #if equal sila sa variables, then dun sila malalagay sa matrix
        if(identical(finals[[1]][2],constants[m])){
          variables[i,m]=finals[[1]][1];
        }
      }
      
      if(j==length(constants)+1){ #if its the final value
        value=as.numeric(finals[[1]][1])*(-1); 3 #negate the value
        #print(value);
        variables[i,j]=value;
      }
    }
    
  }
  print("Augmented Coefficient Matrix"); #finally print the augement coefficient matrix
  print(variables);
  return (variables);
}
swap <- function(pivot_row, a_row){
  
  
}

max <- function (a){ #function that determines the max term in your aufcoeffmatrix
  max=1; #dummy values
  truemax=0;
  for(j in 1:length(a)){
    if(a[j]>truemax){ #if the value is greater than the max right now
      truemax=a[j] #the value becomes the truemax
      max=j; #its index becomes the max
    }
  }
  return (max); #return the index of the max
}

as_numeric <- function(AugMatrix){ #function that converts my augcoeffmatrix into numerical
  TrueAug=matrix(c(0), nrow=nrow(AugMatrix), ncol=ncol(AugMatrix)); #create a new augcoeffmatrix
  for(i in 1:nrow(AugMatrix)){ #nested loops to access each elem
    for(j in 1:ncol(AugMatrix)){
      TrueAug[i,j]=as.numeric(AugMatrix[i,j]) #convert it as a numeric
    }
  }
  return (TrueAug); #return the new numerical aug coeff matrix
}

Gaussian <- function (system){ #Main Gaussian Function
  TrueAug=system#translate it as a numercial version
  #print(TrueAug)
  for(i in 1:(nrow(TrueAug))){ #for loop for the rows
    pivot_row=max(abs(TrueAug[1:nrow(TrueAug),i])); #getting the pivot row by getting the max values 
    #print(pivot_row)
    if(TrueAug[pivot_row, i]==0){ #if the eq has no sol
      print("No Solution!");
      break;
    }
    else{
      #print(pivot_row);
      temp=TrueAug[i,]; #swap
      TrueAug[i,]=TrueAug[pivot_row,];
      TrueAug[pivot_row,]=temp;
      #print(TrueAug);
      for(j in (i+1):nrow(TrueAug)){
        if(j>nrow(TrueAug)){ #if its the end, stop
          break;
        }
        pivot_elem=TrueAug[i,i]; #determine the pivot element
        #print(pivot_elem)
        multiplier=TrueAug[j,i]/pivot_elem; #get the multiplier
        normalized_row=multiplier*(TrueAug[i,]); #get the normalized row by multiplying it to the orig row
        TrueAug[j,]=TrueAug[j,]-normalized_row; #subtract the normalized row to the original eq
        #print(TrueAug);
      } 
    }
    
    
  }
  #print(TrueAug);     BACKWARD SUBSTITUTION
  final=matrix(c(0), nrow=nrow(TrueAug), ncol=1); #create the matrix
  final[nrow(TrueAug)]=TrueAug[nrow(TrueAug),ncol(TrueAug)]/TrueAug[nrow(TrueAug), ncol(TrueAug)-1]; # to get the value of the easiest variable to solve which is the last
  #print(final[3])
  for(i in (nrow(TrueAug)-1):1){ #for loop to find the answer
    hehe=sum(TrueAug[i, nrow(TrueAug):(i-1)] * final[nrow(TrueAug):(i-1)] ) #get the sum of the variables times the constants first 
    final[i]=(TrueAug[i,ncol(TrueAug)]-hehe)/TrueAug[i,i] #finally get the answer
    #print(final[i]);
    #print(hehe)
  }
  print("Answer:");
  print(final);
  return(final);
}

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


PolynomialRegression <- function(myData, degree, estimatedvalue){ # function that solves the polynomial regression of a given variables and degree
  # myData <- read.csv(file="C:/Users/Marlon/Documents/Try.csv", header=TRUE, sep=",");
  
  independent=myData[,1];
  dependent=myData[,2];
  print(independent);
  print(dependent)
  
  if(length(independent)!=length(dependent)){ #if the length is not the same
    return (NA);
  }
  else{
    if(degree<1){#invalid degree
      return (NA);
    }
    else{
      if((length(independent)-1)<degree){ #invalid degree (degree too large)
        return (NA);
      }
      else{ #all good
        Summation_matrix=matrix(c(0), nrow=degree+1, ncol=degree+2)#create a matrix where you will put the values
        raisedto=c();
        
        
       
        for(i in 0:(degree)){ #get the summations //nested for loops for the row
          col=1; #column counter
          for(j in i:(degree+i)){ #for loop for the increase in degree
              for(k in 0:length(independent)){ #to raise the values by the degree
                raisedto[k]=(independent[k])^j
              }
            #print(raisedto);
            #print(sum(raisedto))
            Summation_matrix[i+1,col]=sum(raisedto); #add all the raised values
            col=col+1; #update the col count
          }
          
        }
       #print(Summation_matrix);
       raised2=c();
       product=c();
        for(i in 0:degree){ #get the RHS
          for(j in 0:length(independent)){ #get the raised x
            raised2[j]=(independent[j])^i
          }
          for(k in 0:length(dependent)){ #get the product of the raised x and y
            product[k]=raised2[k]*dependent[k]; 
          }
          Summation_matrix[i+1,((degree+2))]=sum(product);#get the summation of the product
        }
       print(Summation_matrix) #finally have the full aug coeff matrix
      

       Answer=GaussJordan(Summation_matrix); #solve the matrix by using the gauss jordan method
       print(Answer); #print the answer
       
       
       eq<-paste("function(x) ", sep=""); #paste and concatinate to create a function
       for(i in 1:(degree+1)){ #loop to combine all the variables and coefficients
         if(i==1){ #if its the constant without the variables
           eq<-paste(eq, Answer[1], " ",sep="");
         }else{
           eq<-paste(eq," + ", Answer[i], " * ", "x^", i-1 , sep="")
         }
       }
      # print(Answer[1])
      # print(eq);
       #print(x=eval(parse(text=eq))) #print the parsed
       x=eval(parse(text=eq))
       # print("Final Equation:")
       functionanswer=x(estimatedvalue) #get the answer
       answer=list(eq,functionanswer);
       return(answer); #return the formed equation
     
       #eq <-paste(paste(lhs, rhs, sep="<-"),collapse=";")
       #eval(parse(text=eq))
      } 
    }
  }
}