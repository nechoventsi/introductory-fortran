program system_linear

implicit none
real :: m, s
real,allocatable :: ab(:,:), x(:)
integer :: n, i, j, k
character*20 :: fnamein, fnameout

write (*,*) "System of linear equations solver"
write (*,*) "by Ventsilav Dimitrov"
write (*,*)

12 write (*,*) "Enter file name to read:"
read (*,*) fnamein
write (*,*)
write (*,*) "How many equations do you want to solve?"
read (*,*) n

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

write (*,*)
write (*,*) "Unknowns (in order):"

do i=1,n,1
    write (*,*) x(i)
end do

close(11)

write (*,*)
write (*,*) "Enter file name to save results in:"
read (*,*) fnameout
open(12, file=fnameout)

do i=1,n,1
    write (12,*) x(i)
end do

close(12)
deallocate(ab,x)

goto 12

end program system_linear

! Version 1.0.0