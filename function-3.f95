program fun-y

implicit none
real :: a,b,x,y,s
integer :: n

a=2.2
b=7.4
s=0

write (*,*) 'Problem: y=bx-e^((ax)^2)'

do n=20,50,1

   x=n/100.0
   y=b*x-exp((a*x)**2)
   s=s+log(y-y**2)/(n-1)**3
   write (*,*) x,y
   
end do

write (*,*) s

end program fun-y