BiggestNegative <- function (rightestcol){ #get the biggest negative value in the RHS
  lowestnegtive=-1;
  min=0; #set 0 as a tempoary minimum
  for(i in 1:length(rightestcol)){
    if(rightestcol[i]<min){ #if the current value is lower than the value in minimum
      lowestnegtive=i;
      min=rightestcol[i];
    }
  }
  return(lowestnegtive); #return the index of the minimum
}

Minimum <- function (testratio){ #function that returns the minimum positive number
  minimumcol=-1;
  min=999999; #set a temportary minimum
  for(i in 1:length(testratio)){
    if(testratio[i]!=0 && testratio[i]<min){ #if the value is not equal to 0 and is leser than the minimum
      min=testratio[i];
      minimumcol=i;
    }
  }
  return(minimumcol); #return the minimum number
}

TestRatio <- function(pivotrow, BHS){ #function that solves my test ratio
    testratio=c()
    for(i in 1:(length(pivotrow)-1)){ #fill the test ratio with 0's temporatrily. To be used later
      testratio[i]=0;
    }

    for(i in 1:(length(pivotrow)-1)){ #get the test ratio
      if(BHS[i]<0 && pivotrow[i]!=0){ #the bottom hand side should be less than 0 and the pivot row element your dividing should not be 0 to avoid imaginar numbers
        testratio[i]=abs(BHS[i]/pivotrow[i]);
      }
    }
    Min=Minimum(testratio); #get the minimum among those test ratio
    return(Min);
}


Simplex <- function(M){ #function that solves my simplex problem. I used the dual Simplex method so it may be weird but its just the reverse of the process in the normal simplex
  print(M);
  # M=matrix(c(Matrix), nrow=9, ncol=25)
  # print(M)
  pivotrow=0;
  Tables=list();  #this is where ill put my list of tables
  iterations=1;
  while(TRUE){ #while there is a negative in RHS of the matrix
    pivotrow=BiggestNegative(M[,ncol(M)]); #get the pivot row
    if(pivotrow==-1){ #if there is no negative then break the infiniteloop
      break;
    }
    pivotcol=TestRatio(M[pivotrow,],M[nrow(M),]);  #get the pivot column
    pivotelement=M[pivotrow,pivotcol] #get the pivot element

    for(i in 1:(ncol(M))){ #normalize the row
      M[pivotrow,i]=M[pivotrow,i]/pivotelement;
      

    }
    # print(M[pivotrow,]);
    for(j in 1:(nrow(M))){ #normalize the pivot col and the rest of its rows
      if(j!=pivotrow){ #to preserve the pivot row cause its already normalized
       
        minusrow=vector(mode="integer", length=ncol(M)); 
        
        for(k in 1:ncol(M)){
          minusrow[k]= M[j,pivotcol]*M[pivotrow,k] #get the minus vector that you will subtract to the current row
        }
        print(M[j,])
        for(l in 1:ncol(M)){
          M[j, l] = M[j,l] - minusrow[l] # get the row that you will subtract to normalize
        
        }
      }
    }
    print(M)
    Tables[[iterations]]=M; #place the matrix per iteration in the list
    iterations=iterations+1;
  }
  Tables[iterations]=iterations-1 #put the number of iterations it took to solve the problem as the last element in the list
  print(Tables)
  return(Tables)
  
}