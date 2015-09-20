program matrix-det

implicit none
real :: a11, a12, a13, a21, a22, a23, a31, a32, a33, d1, d2, det
character :: quit

write (*,*) 'Enter the elements of a 3x3 matrix:'

write (*,*) 'a11 ='
read (*,*) a11
write (*,*) 'a12 ='
read (*,*) a12
write (*,*) 'a13 ='
read (*,*) a13
write (*,*) 'a21 ='
read (*,*) a21
write (*,*) 'a22 ='
read (*,*) a22
write (*,*) 'a23 ='
read (*,*) a23
write (*,*) 'a31 ='
read (*,*) a31
write (*,*) 'a32 ='
read (*,*) a32
write (*,*) 'a33 ='
read (*,*) a33

d1=a11*a22*a33+a12*a23*a31+a13*a21*a32
d2=-a13*a22*a31-a11*a23*a32-a12*a21*a33
det=d1+d2

write (*,*) 'The determinant of the matrix is: ',det

write (*,*) 'Press any key to exit'
read (*,*) quit

end program matrix-det