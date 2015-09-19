program roots

implicit none
real :: a,b,c,x,D,x1,x2
complex :: x1com,x2com
character(len=1) :: try

do
write (*,*) 'Find the roots of ax^2+bx+c=0'

write (*,*) 'Select a:'
read (*,*) a
write (*,*) 'Select b:'
read (*,*) b
write (*,*) 'Select c:'
read (*,*) c

if (a/=0) then

   D=(b**2)-(4*a*c)

   if (D>0) then
      write (*,*) 'Real roots are:'
      x1=(-b+sqrt(D))/(2*a)
      write (*,*) '1st root is: ',x1
      x2=(-b-sqrt(D))/(2*a)
      write (*,*) '2nd root is: ',x2
   else if (D==0) then
      write (*,*) 'Real roots are:'
      x1=-b/(2*a)
      x2=-b/(2*a)
      write (*,*) 'The root is (x1=x2): ',x1
   else if (D<0) then
      write (*,*) 'Does not have real roots!'
      write (*,*) 'Complex roots are:'
      x1com=(-b+sqrt((1,0)*D))/(2*a)
      write (*,*) '1st complex root is: ',x1com
      x2com=(-b-sqrt((1,0)*D))/(2*a)
      write (*,*) '2nd complex root is: ',x2com
   end if
   
else

   if (b/=0) then

      write (*,*) 'The equation becomes bx+c=0'
      x=-(c/b)
      write (*,*) 'x is:',x

   else

      if (c/=0) then
         write (*,*) 'Does not have a solution!'
      else
         write (*,*) 'Every x is an answer!'
      end if
   end if
end if

write (*,*) ''

write (*,*) 'Try again? (y/n)'
read (*,*) try

if (try=='y') then
   cycle
else
   exit
end if

end do

end program roots