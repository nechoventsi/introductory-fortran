program fun-y-2

implicit none
real :: a,b,x,y,s
integer :: n

a=2.2
b=7.4
s=0

write (*,*) 'Problem: y=bx-e^(ax)^2'

do n=-22,14,1

   x=n/10.0
   y=b*x-exp(a*x)**2
   write (*,*) 'Answer:',y
   s=s+y

end do

write (*,*) s

end program fun-y-2