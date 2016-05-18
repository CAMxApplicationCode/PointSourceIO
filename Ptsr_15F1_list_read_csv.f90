SUBROUTINE  ptsrcsvread(Array,nraw,ngroups)
!program csvread
IMPLICIT NONE

 INTEGER i,j,ngroups,nraw
 REAL Array(2000,20)

 open(10, file = "multi_HSC10flares_modify.csv",form="formatted",status='old')
 !open(9,file="out.txt",status='new')
! open (11,form='formatted')

 ngroups=9
 nraw=10

 do i=1,nraw
	 read(10,*)(Array(i,j),j=1,ngroups)
	 write(*,*) i, Array(i,:ngroups)
 enddo
! do i=1,1000
!	do j=1,2
!		read(10,*) InArray(i,j)
!        Array(i,1)=(int(InArray(i,1)-(-2388000)/12000)+1
!		Array(i,2)=(int(InArray(i,2)-(-2388000)/12000)+1
!		write(11,*)(Array(i,j),j=1,2)
!		write(*,*) "wang1"
!	enddo
!   write(11,913) (LCCxo(n),LCCyo(n))
! enddo
! write(9,913) (grid(n,m),n=1,763)

!read(12,*) (LCCx(n),n=1,1000)
913  format(2f12.2)

!end program
END SUBROUTINE ptsrcsvread



