program matrix_det

implicit none
real :: a(3,3), d1, d2, det
character*1 :: try

write (*,*) "Enter the elements of a 3x3 matrix:"

write (*,*) "a11 ="
read (*,*) a(1,1)
write (*,*) "a12 ="
read (*,*) a(1,2)
write (*,*) "a13 ="
read (*,*) a(1,3)
write (*,*) "a21 ="
read (*,*) a(2,1)
write (*,*) "a22 ="
read (*,*) a(2,2)
write (*,*) "a23 ="
read (*,*) a(2,3)
write (*,*) "a31 ="
read (*,*) a(3,1)
write (*,*) "a32 ="
read (*,*) a(3,2)
write (*,*) "a33 ="
read (*,*) a(3,3)

d1 = a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+a(1,3)*a(2,1)*a(3,2)
d2 = -a(1,3)*a(2,2)*a(3,1)-a(1,1)*a(2,3)*a(3,2)-a(1,2)*a(2,1)*a(3,3)
det = d1+d2

write (*,*) "The determinant of the matrix is: ", det

read (*,*)

end program matrix_det

! A simple 3x3 determinant solver program
! By Naiden, recoding by Ventsislav Dimitrov
! Version 0.0.2