program system_linear

implicit none
real :: m, s
real,allocatable :: ab(:,:), x(:)
integer :: n, i, j, k
character*50 :: fnamein, fnameout, try

write (*,*) "System of linear equations solver"
write (*,*)

12 write (*,*) "Enter file name to read:"
read (*,*) fnamein
write (*,*)
write (*,*) "How many equations do you want to solve?"
read (*,*) n
write (*,*)

allocate(ab(n,n+1),x(n))

open(11, file=fnamein)

do i=1,n,1
    read (11,*) ab(i,1:n+1)
end do

do k=1,n-1,1
    do j=k+1,n
        m = (-ab(j,k))/ab(k,k)
        do i=1,n+1,1
            ab(j,i) = ab(j,i)+ab(k,i)*m
        end do
    end do
end do

do i=n,1,-1
    s = 0
    do j=i+1,n
        s = s + ab(i,j)*x(j)
    end do
    x(i) = (ab(i,n+1)-s)/ab(i,i)
end do

close(11)

write (*,*) "Check results out? (y/n)"
read (*,*) try

if (try=="y") then
    write (*,*) "Roots (in order):"

    do i=1,n,1
        write (*,*) x(i)
    end do
end if

write (*,*)
write (*,*) "Enter file name to save results in:"
read (*,*) fnameout
open(12, file=fnameout)

do i=1,n,1
    write (12,*) x(i)
end do

close(12)
deallocate(ab,x)

write (*,*)
write (*,*) "Try again? (y/n)"
read (*,*) try

if (try=="y") then
    goto 12
else
    goto 79
end if

79

end program system_linear

! A solver program for systems of linear equations
! Version 0.0.4
! By Ventsilav Dimitrov