t<-function(a){
   n<-length(a)
  xc <-c()
   for(i in 1:(n-1)){
      if (all(a[i]+a[i+1] > 10)) {xc <- c(xc,i)}
     }
   return(xc)
}


makemissing<-function(var){
  n<-length(var)
  for(i in 1:n){
    if(i%%2==1)
    {var[i]<- NA}
  }
  return(var)
}

makemissing<-function(var){
  n<-length(var)
  for(i in 1:n){
    nob<-runif(1)
    if(nob > 0.5)
    {var[i]<- NA}
  }
  return(var)
}

countNA <-function(x){
  n<-length(x)
  count<-0
  for(i in 1:n){
    if(is.na(x[i])){
      count<-count+1
    }
    return(count)
  }
}


findruns<-function(x,k){
  n<-length(x)
  count<-0
  runs<-c()
  for (i in 1:(n-k+1)){
    if(all(x[i:(i+k-1)]==1)) {
      count<-count+1
      runs <-c(runs,i) }
  }
  return(list(runs=runs,count=count))
}

sumofeightcheck<-function(y1,k){
  y1<-sort(y1)
  n<-length(y1)
  d<-0
  for (i in 1:n){
    print(cbind(i,"i"))
      if(d==1) {break}
      for (j in n:(i+1)){
        print(cbind(j,"j"))
          if(d==1) {break}
          if(y[i]+y[j]== k) {d <- 1}
        } 
      }
  return(d)
}


#Maximum sum of 3 Non Overlapping Subarrays


matrx<-c(1,2,1,2,6,7,5,1)

maxSumOfThreeSubarrays<-function(mats, k){


# Best single, double, and triple sequence found so far
bestSeq = 1
bestTwoSeq = c(1, k)
bestThreeSeq = c(1, k, k*2)

# Sums of each window

seqSum = sum(mats[1:k])
seqTwoSum = sum(mats[(k+1):(k*2)])
seqThreeSum = sum(mats[((k*2)+1):(k*3)])

# Sums of combined best windows
bestSeqSum = seqSum
bestTwoSum = seqSum + seqTwoSum
bestThreeSum = seqSum + seqTwoSum + seqThreeSum

# Current window positions
seqIndex = 1
twoSeqIndex = k + 1
threeSeqIndex = (k*2) + 1
while(threeSeqIndex < (length(mats) - k)){
  # Update the three sliding windows
seqSum = seqSum - mats[seqIndex] + mats[seqIndex + k ]
seqTwoSum = seqTwoSum - mats[twoSeqIndex] + mats[twoSeqIndex + k]
seqThreeSum = seqThreeSum - mats[threeSeqIndex] + mats[threeSeqIndex + k]

# Update best single window
if(seqSum > bestSeqSum){
  bestSeq = seqIndex
bestSeqSum = seqSum
}
# Update best two windows
if((seqTwoSum + bestSeqSum) > bestTwoSum){
  bestTwoSeq[1] <- bestSeq
  bestTwoSeq[2] <- twoSeqIndex + 1
bestTwoSum = seqTwoSum + bestSeqSum
}
# Update best three windows
if((seqThreeSum + bestTwoSum) > bestThreeSum){
  bestThreeSeq[1] = bestTwoSeq[1]
  bestThreeSeq[2] = bestTwoSeq[2]
  bestThreeSeq[3] = threeSeqIndex + 1
bestThreeSum = seqThreeSum + bestTwoSum}

# Update the current positions
seqIndex = seqIndex + 1
twoSeqIndex = twoSeqIndex + 1
threeSeqIndex = threeSeqIndex + 1
}
return(bestThreeSeq)
}


maxSumOfThreeSubarrays(matrx,2)





##Search a rotated array


Input: nums = [4,5,6,7,0,1,2], target = 0
Output: 4

Input: nums = [4,5,6,7,0,1,2], target = 3
Output: -1


lo=1
hi=7
mid=0


A=c(4,5,6,7,8,0,1,2)
SearchArray<-function(nums,target){
  low=1
  high=length(nums)
  while(low <= high){
    print(paste0(low,"low"))
    print(paste0(high,"hi"))
    mid = as.integer((low + high) / 2)
    print(paste0(mid,"mid"))
    if(target == nums[mid]){
    return(mid)}

    if(nums[low] <= nums[mid]){
      if(nums[low] <= target & target <= nums[mid]){
        high = mid - 1}
      else{
        low = mid + 1}}
    else{
    if(nums[mid] <= target & target <= nums[high]){
    low = mid + 1}
      else{
    high = mid - 1}
    }
  }
  return(-1)
}




##Intervals in R - how many meeting rooms needed

Intervals = list(
  c(0,30),
  c(5,10),
  c(15,20)
)
MeetingRooms<-function(Intervals){
start=sort(sapply(Intervals, function(i)i=min(i)))
end=sort(sapply(Intervals,function(i)i=max(i)))
rooms=0
endsItr=1
i=1
while(i < length(start)){
  if(start[i]<end[endsItr]){
    rooms=rooms+1}
  else{endsItr=endsItr+1}
  i=i+1
  }
return(rooms)
}




##Phone problem a bc

Phoneroblem<-function(string){
x=list(NULL,c("a","b","c"),c("d","e","f"),c("g","h","i"),c("j","k","l"),c("m","n","o"),c("p","q","r","s"),c("t","u","v"),c("w","x","y","z"))
a=as.integer(strsplit(word,"")[[1]])
n=length(a)
results<-c("")
k=1
for(k in 1:n){
  print(paste(k,"kth iteration",sep=" - "))
  list<-x[[a[k]]]
  newresult<-c("")
 for(i in list){
   for(j in results){
     print(paste(i,"ith iteration",sep=" - "))
     print(paste(j,"jth iteration",sep=" - "))
    
    newresult<-c(newresult,paste0(i,j))
    print(paste(newresult,"newresult updated",sep=" - "))
    #sum<-paste0(sum,paste0(x[[a[k]]][i]))
   }
 }
  results = newresult
}
result_final<<-results[nchar(results)==n]
return(result_final)
}



##An island in R

A=matrix(c(1,0,1,0,1,1),nrow=3,ncol=3)


countIslands<-function(mat){
  row=nrow(mat)
  col=ncol(mat)
  count=0
  for(i in 1:row){
    for(j in 1:col){
      if(A[i,j]==1){
        islandCheck(mat,i,j)
        count=count+1
      }
    }
  }
  return(count)
}

islandCheck<-function(matr,x,y){
  nr=nrow(matr)
  nc=ncol(matr)
  matr[x,y]<-0
  if(x!=1){
    islandCheck(matr,x-1,y)}
  if(x+1 < nr){
    islandCheck(matr,x+1,y)}
  if(y!=1){
    islandCheck(matr,x,y-1)}
  if(y+1 < nc){
    islandCheck(matr,x,y+1)}
}

  




##Binary Search Tree

##No R Solution


class Solution(object):
  def isValidBST(self, root, lessThan = float('inf'), largerThan = float('-inf')):
  if not root:
  return True
if root.val <= largerThan or root.val >= lessThan:
  return False
return self.isValidBST(root.left, min(lessThan, root.val), largerThan) and \
self.isValidBST(root.right, lessThan, max(root.val, largerThan))

    
tobt<-function(vec,start,end){
  if(start > end){return(NULL)}
  mid<-start + (end-start)%/%2
  left<-tobt(vec,start+1,end-2)
  parent<-vec[start]
  right<-tobt(vec,start+2,end)
  return(list(left=left,node=parent,right=right))
}

vec<-c(5,1,4,2,3,5,6,7,6,4,3,2,1,2,3)
tobt(vec,1,15)





#####################MAX WATER HEIGHT

##Dynamic Programming

trap<-function(height)
{
   left = 1
   right = length(height)
   water = 0
   left_wall =0
   right_wall = 0

   while(left < right){
     print(paste(left,"left"))
     print(paste(right,"right"))
     if(height[left] < height[right]){
       if(height[left] > left_wall){
         left_wall=height[left]}
       else{water = water + left_wall - height[left]}
      left=left+1
     }
    else{
        if(height[right]>right_wall){
          right_wall=height[right]}
        else{water = water + right_wall - height[right]}
      
        right=right-1
         
    }
     
   }
   return(water)
}


q<-c(0,1,0,2,1,0,1,3,2,1,2,1)
q1<-c(2,1,0,1,2)



## Palindrome
text<-"mama"
text2<-"abca"
text3<-"abcdef"
text4<-"madam"





validPalindrom2<-function(txt){
  s<-strsplit(txt,"")[[1]]
  i = 1
  j = length(s)
  h = floor(length(s)/2)
  k=0
  while(i<h)
    print(paste(i,'as i',sep=" "))
    if(s[i]!=s[j+1-i]){
      k = (j + 1) - i
      return(isPalindromeRange(s,i+1,k) || isPalindromeRange(s,i,k-1))
    }
  i=i+1
  return(TRUE)
}

isPalindromeRange<-function(s,i,k){
  t=0
  u=0
  t=i
  u=floor(i+(k-i)/2)
  while(t < u)
    print(paste(t,'as t',sep=" "))
    if(s[t]!=s[k-t+i]){return(FALSE)}
  return(TRUE)
  t=t+1
}
 


validPalindrome<-function(txt){
  s<-strsplit(txt,"")[[1]]
  i = 1
  j = length(s)
  deleted = FALSE
  isPalindrome = FALSE
  mid<-floor(j/2)
if(s[mid]!=s[mid+1]){deleted = TRUE
    s<-s[-mid]
    j = length(s)}
while(i < j){
  if(s[i]!= s[j]){
    if(s[i+1] != s[j] & s[j-1] != s[i]){
      deleted = FALSE
      isPalindrome = FALSE
      break }
  else if(s[i+1] == s[j] & s[i+1] == s[j-1]){
      i = i + 1
      deleted = TRUE 
      isPalindrome = TRUE}
  else if(s[j-1] == s[i] & s[j-1] == s[i+1]){
      j = j + 1
      deleted = TRUE 
      isPalindrome = TRUE}
  }
  else{isPalindrome = TRUE}
i = i + 1
j = j - 1}

return(list(deleted=deleted,Palindrome=isPalindrome))
}

validPalindrome(text)

validPalindrome(text4)
validPalindrome(text2)

#String function

dic<-function(){}
  belowTen = C("", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")
  belowTwenty = c("Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen")
  belowHundred =c("", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety")
  
numberToWords <-function(num){
    if (num == 0) return "Zero"
    return helper(num)
  }
  
helper<-function(int) {
    result = c();
    if (num < 10) result = belowTen[num];
    else if (num < 20) result = belowTwenty[num -10];
    else if (num < 100) result = belowHundred[num/10] + " " + helper(num % 10);
    else if (num < 1000) result = helper(num/100) + " Hundred " +  helper(num % 100);
    else if (num < 1000000) result = helper(num/1000) + " Thousand " +  helper(num % 1000);
    else if (num < 1000000000) result = helper(num/1000000) + " Million " +  helper(num % 1000000);
    else result = helper(num/1000000000) + " Billion " + helper(num % 1000000000);
    return result.trim();
  }
}


##Integer to words

int_to_words <- function(x) {
  
  index <- as.integer(x) + 1
  result <-c()
  if (x <= 10){
  words <- c('', 'one', 'two', 'three', 'four',
             'five', 'six', 'seven', 'eight', 'nine',
             'ten')
  result<-words[index]}
  else if(x < 20 ) {
  words<- c('Ten', 'Eleven', 'Twelve', 'Thirteen', 'Fourteen', 'Fifteen', 'Sixteen', 'Seventeen', 'Eighteen', 'Nineteen')
  result<-words[index-10]
  }
  
  else if(x < 100 ) {
    words<- c('Ten', 'Twenty', 'Thirty', 'Forty', 'Fifty', 'Sixty', 'Seventy', 'Eighty', 'Ninety')
    result<-paste(words[index/10],int_to_words((index-1)%%10),sep=' ')
  }
  
  else if(x < 1000 ) {
    words<- c('Ten', 'Twenty', 'Thirty', 'Forty', 'Fifty', 'Sixty', 'Seventy', 'Eighty', 'Ninety')
    result<-paste(int_to_words(index/100),'Hundred',int_to_words((index-1)%%100),sep=' ')
  }
  
  return(result)
}

int_to_words(11)

int_to_words(90)
aq<-c("Eleven")



## Moving non Zeroes


##Moving Zeroes to the end



moveZeroes<-function(nums){
#count the zeros
num_zeroes = 1
res<- c()
for (n in 1:length(nums)){
  if (nums[n] != 0){
#Make all the non-zero elements retain their original order 
  res[num_zeroes] <- nums[n]
num_zeroes=num_zeroes+1
  }
}
while(num_zeroes < n+1){
  res[num_zeroes]= 0
num_zeroes=num_zeroes+1
}
return(res)
}
  

## Finding intersection
nums1 = c(1,2,2,1)
nums2 = c(2,2)





intersection<-function(a,b){
a<-sort(a)
b<-sort(b)
inda<-1
indb<-1
k<-length(a)+1
k2<-length(b)+1
samevalues<-c()

while(inda < k & indb < k2){
    if(a[inda]==b[indb]){
      samevalues[indb]<-b[inda]
      indb<-indb+1
      inda<-inda+1
    }
   if (a[inda] < b[indb]){
    indb<-indb- 1
   }
  if (a[inda] > b[indb]){
    inda<-inda- 1
}

return(samevalues)
}
}


#Given n = 5, and version = 4 is the first bad version.

#call isBadVersion(3) -> false
#call isBadVersion(5) -> true
#call isBadVersion(4) -> true

#Then 4 is the first bad version. 


FindingBadVersion<-function(n){
  n<-as.integer(n)
  version_check=1
  while(version_check <= n){
    mid<-as.integer((1+n)/2)
    if(isBadVersion(mid)==FALSE & ){
      version_check<-mid+1}
    else{
      version_check<-version_check-1
      break}
    return(version_check)
  }
  



FindShortRange<-function(v,k){
  pointers<-matrix(NA,nrow =)
  min<-0
  max<-0
  range<-0
  store<-c()
  
for (i in 1:length(k)){
 v[[i]][pointers]<-c[i]

 

#Threee Sum
 
zaza<-c(-1,0,1,2,-1,-4)
 
 
findTriplets<- function(arr){
found = "False"
n <- length(arr)
position=1
d<-list()
 for(i in 1:(n-2)){ 
   for(j in (i+1):(n-1)){ 
      for(k in (j+1):(n)){  
    print(i)
    print(j)
    print(k)
    if  arr[]
    if (sum(arr[i],arr[j],arr[k]) == 0){ 
          d[[position]]<-c(arr[i], arr[j], arr[k]) 
          found = "True" 
          position = position + 1 
          }
      }
   }
 } 
 return(d)
}
 

threeSum<-function(nums){
res = list()
x<-sort(nums)
length <- length(nums)
l<-2
r<-length
total<-0
position<-1
  for(i in 1:(length-1)){ #[8]
    print(paste(i," i"))           
#     if nums[i]>0: break #[7]
      if(i>1){
        #if(x[i]==x[i-1]){ #[1]
              l = i + 1
#              r = length - 1
#              length=length-1 }
      }
    
    
        while(l<r){
            print(paste(l," l"))
            print(paste(r," r"))
            total = x[i]+x[l]+x[r]
            if(total<0){ #[3]
              l=l+1}
            else if(total>0){ #[4]
              r=r-1}
            else{ #[5]
            res[[position]]<-c(x[i],x[l],x[r])
              position = position + 1
              while(l<r & x[l]==x[l+1]){ #[6]
                l=l+1}
              while(l<r & x[r]==x[r-1]){ #[6]
                r=r-1}
                l=l+1
                r=r-1}
          }
  }
return(res)
}
  

nums<-c(-1, 0, 1, 2, -1, -4)
 
arr = c(0, -1, 2, -3, 1)  
  a = c(1,2,3)
  b= c(3,4,5)
  c = c(5,7,8)








S = "abcdebdde"
T = "bde"


minWindow<-function(S,T){
  s<-strsplit(S, "")[[1]]
  t<-strsplit(T, "")[[1]]
  sindex<-1
  tindex<-1
  slength<-length(s)
  tlength<-length(t)
  
}



##Smallest Range

TT<-list(c(4,10,15,24,26),c(0,9,12,20), c(5,18,22,30))

XX<-list(c(3,10,15,24),c(0,1,2,20), c(1,18,21,30))


SmallestRange<-function(TT){
 minx = 0
 miny = 100000
 nexto = c()
 nexto[1:length(TT)]<-1
  flag = TRUE
  i=1
  j=1
  k=1
    for(i in 1:length(TT)){
        if(flag){
      for(j in 1:length(TT[[i]])){
        if(flag){
        min_s = 1 
        max_s = 1
        for(k in 1:length(TT)){
          print(paste0(i,"i"))
          print(paste0(j,"j"))
          print(paste0(k,"k"))
          print(paste0(min_s,"min_s"))
          print(paste0(max_s,"max_s"))
          print(paste0(miny,"miny"))
          print(paste0(minx,"minx"))
          print(paste0(TT[[max_s]][nexto[max_s]],"TTmax"))
          print(paste0(TT[[min_s]][nexto[min_s]],"TTmin"))
          if(TT[[min_s]][nexto[min_s]] > TT[[k]][nexto[k]]){
            min_s = k}
          if(TT[[max_s]][nexto[max_s]] < TT[[k]][nexto[k]]){
            max_s = k}
          print(paste0(min_s,"min_s"))
          print(paste0(max_s,"max_s"))
        }
        if(miny - minx > TT[[max_s]][nexto[max_s]] - TT[[min_s]][nexto[min_s]]){
          miny = TT[[max_s]][nexto[max_s]]
          minx = TT[[min_s]][nexto[min_s]]
          print(paste0(miny,"miny - NEW"))
          print(paste0(minx,"minx - NEW"))
        }
        nexto[min_s]<-nexto[min_s]+1
          print(paste0(nexto,"look - Minimum Value Of K"))
        if(nexto[min_s] > length(TT[[min_s]])){
          flag = FALSE
        } } } }
    }
    return(list(interval=c(minx, miny,TT[[max_s]][nexto[max_s]],TT[[min_s]][nexto[min_s]])))
  }

