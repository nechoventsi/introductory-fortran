program fun-y

implicit none
real :: a,b,x,y
character :: quit

a = 2.2
b = 7.4

write (*,*) 'Problem: y = bx-e^(ax)^2'
write (*,*) '"a" and "b" are predefined:'
write (*,*) 'a = 2.2'
write (*,*) 'b = 7.4'
write (*,*) 'Select x (real number):'
read (*,*) x

y=b*x-exp(a*x)**2

write (*,*) 'Answer:',y

write (*,*) 'Press any key to exit'
read (*,*) quit

end program fun-y