      program ptsr_emis_modify
c
c
c Program:	ptsr_change  
c
c Language:	Fortran90
c Programmer:	Chitsan Wang 
c Version:	0.0.5
	  parameter(MXY=300, MXZ=16, MXSPEC=200, MXPT=700000)
c 177337 is the x location
	  character*41 b
	  character*36 a
	  character*49 inputname
	  character*49 outputname
      character*10 ftype
      character*4 fname(10), spname(10,MXSPEC)
      character*4 note(60)
      integer iloc(4,MXY), LC, cn, pt(32),c,d,p,q,nrawp,ngroupsp,V,W,jd
      integer idumx(MXPT), idumy(MXPT)
      integer icell(MXPT), jcell(MXPT), kcell(MXPT)
      real    temp(MXY,MXY), bconc(MXZ,MXY),locate(2000,20),NOxbase(32)
      real    emiss(MXPT), flow(MXPT), plumht(MXPT),emissm(MXPT)
      real    xc(MXPT),yc(MXPT),sh(MXPT),X,Y,ER, H
      real    sd(MXPT),st(MXPT),sv(MXPT)
	  real    NOx_mutiple,HCHO_mutiple,ETH_mutiple
      logical lhdrspec, l3d, lbndry, lptsrc
c read and setup basic parameters    
	  a="./camx.pt.2009ab1-all.cb05.ag.rpo36."
	  b="./camx.pt.2009ab1-all.cb05.ag.rpo36.SCE8."
	  write(*,*)a
	  write(*,*)b
	  write(*,*)'input start hour 0~23'
	  read(*,*)bh
c	  write(*,*) 'input spec sequence'
c	  read (*,*) Spec  
c	  write(*,*) 'input the times of increasing emission rate'
c	  read (*,*) ER
	  CALL ptsrcsvread(locate,nrawp,ngroupsp)
c-----------------------------------------------------------
c	  do 111 j=1
	  	write(inputname,'(A36,I8)')a,20090403
	  	write(*,*)inputname
	  	write(outputname,'(A41,I8)')b,20090403
	  	write(*,*)outputname
	  	write(*,*)a
	  	write(*,*)b
c	  	write(*,*) 'input spec sequence'
c	  	read (*,*) Spec  
c	  write(*,*) 'input the times of increasing emission rate'
c	  read (*,*) ER
c	    CALL ptsrcsvread(locate,nrawp,ngroupsp)
c-----------------------------------------------------------
	  	open(10,file=inputname, form='unformatted',status='old')
	  	open(11,file=outputname, form='unformatted',status='new')
c     
		read (10) fname,note,nseg,nspec,idate,begtim,jdate,endtim
		write(11) fname,note,nseg,nspec,idate,begtim,jdate,endtim
c---
		read (10) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,nz,nzlo,nzup,hts,htl,htu
		write(11) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,nz,nzlo,nzup,hts,htl,htu
c---
		read (10) i1,j1,nx1,ny1
		write(11) i1,j1,nx1,ny1
c---      
		read (10) ((spname(m,l),m=1,10),l=1,nspec)
		write(*,903) ((spname(m,l),m=1,10),l=1,nspec)
		write(11) ((spname(m,l),m=1,10),l=1,nspec)
c---
		read (10) iseg,npmax
		write(11) iseg,npmax
c---
		read (10) ((xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip)),ip=1,npmax)
		write(11) (xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip),ip=1,npmax)
c-----------------------------------------------------------------------------------
		NOx_mutiple = 7.3
		HCHO_mutiple = 0.57
		ETH_mutiple = 2.65
		NOx_add = 2406.67
		HCHO_add = 1476.67
		ETH_add = 6392.86
		pt=0
c------Array zero
		cn=0   
c------count zero
c		 write(*,*)pt
		do ip = 1,npmax
			do p=1,nrawp
c			write(*,*) "wang5"
			X=locate(p,1)
			Y=locate(p,2)
			H=locate(p,3)
			if (xc(ip) .EQ. X .AND. yc(ip) .EQ. Y .AND. sh(ip) .EQ. H) THEN 
				PRINT *,'Found at',ip,xc(ip),yc(ip),sh(ip),sd(ip)
				cn = cn+1
				pt(cn) = ip
			end if
			enddo
		enddo
		write(*,*)pt,cn
 100	read (10,end=800) ibgdat,begtim,iendat,endtim
		if (endtim .LE. bh) then
			write(*,904) ibgdat,begtim,iendat,endtim
			write(11) ibgdat,begtim,iendat,endtim
			read (10) iseg,numpts
			write(11) iseg,numpts
			read (10) (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
			write(11) (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
			do l=1,nspec
			read (10) iseg, (spname(m,l),m=1,10),(emiss(ip),ip=1,numpts)
			write(11) iseg, (spname(m,l),m=1,10), (emiss(ip),
     &                          ip=1,numpts)  	 
			enddo
		else
			write(*,904) ibgdat,begtim,iendat,endtim
			write(11) ibgdat,begtim,iendat,endtim
			read (10) iseg,numpts
			write(11) iseg,numpts
			read (10) (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
			write(11) (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
			do l=1,nspec
c			write(*,*) "wang3"
				read (10) iseg, (spname(m,l),m=1,10),(emiss(ip),ip=1,numpts)
				write(*,*) l, begtim
				SELECT CASE (l)
					CASE (1) 
						write(*,903) iseg, (spname(m,l),m=1,10)
c						write(*,*) "wang2"
						do i=1, cn
							write(*,*) i
							LC=pt(i)
							write(*,*)emiss(LC)
							emiss(LC)=emiss(LC)*0+NOx_add
							NOxbase(i)=emiss(LC)
							write(*,*)emiss(LC)
						enddo
					CASE (5) 
						write(*,903) iseg, (spname(m,l),m=1,10)
						do i=1, cn
							LC=pt(i)
							write(*,*)emiss(LC)
							emiss(LC)=emiss(LC)*0+HCHO_add
							write(*,*)emiss(LC),i
						enddo
					CASE (11) 
						write(*,903) iseg, (spname(m,l),m=1,10)
						do i=1, cn
							LC=pt(i)
							write(*,*)emiss(LC)
							emiss(LC)=emiss(LC)*0+ETH_add
							write(*,*)emiss(LC),i
						enddo
				end select
				write(11) iseg, (spname(m,l),m=1,10), (emiss(ip),ip=1,numpts)  	 
			enddo
		endif
	  goto 100
c 222  continue
	  close(10)
      close(11)
 800  write(*,*) "End of File"
c 111  continue
c
c---  I/O error messages ---
c
 900  format(10a1,60a1,/,i2,1x,i2,1x,i6,f6.0,i6,f6.0)
 901  format(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)
 902  format(4i5) 
 903  format(10a1)
 904  format(5x,2(i10,f10.2))
 905  format(i4,10a1)
 906  format(5e14.7)
 908  format(3i10/(5i14))
 909  format(i10,10a1,i10/(5e14.7))
 910  format(2i10)
 911  format(2(f16.5,1x),4e14.7)
 912  format(3i12,2e14.7)
 913  format(5e14.7)
 914  format(1a40,1I5)
 915  format(1a45,1I5)
c
 999  stop
      end
