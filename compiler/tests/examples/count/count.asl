@f2 +!fact(X,Y):X<5<-+fact(X+1,(X+1)*Y).
@f3 +!fact(X,Y):X==5<-.print("fact 5 ==", Y).

!count.

@start +!count:true<-+fact(0,1).