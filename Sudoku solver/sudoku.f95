program sudoku
implicit none
integer x(9,9),x2(9,9,10),i,j,g,h,r(9),c(9),e,f,k,l,m,a3,a4,u,u2,y(9,2),z,z1,n,o,a,t,s,q,x3(9,2),y2
integer x4(81,3),v1,v2,v3,v4,h1,h2,h3,h4,h5,h6,h7,h8,h9,ch(9,4),t1,a5,a2,z2,ll,t2,t3,t4,ch3(81),h10,h11,q1,h12,v5,v6,h13,v8,q2
integer v9,v10,elim(81,2),q3,t9,t10,x5(9,9,10),h15

open(unit=12,file="sud.txt",status='old')
do i=1,9
read(12,*)x(i,1),x(i,2),x(i,3),x(i,4),x(i,5),x(i,6),x(i,7),x(i,8),x(i,9)
enddo
do i=1,81
do j=1,3
x4(i,j)=0
enddo
enddo
s=0



do i=1,81
do j=1,2
elim(i,j)=0
enddo
enddo


 
s=s+1



ll=0

do i=1,9
	do j=1,9
		x2(i,j,1)=x(i,j)
		x5(i,j,1)=x(i,j)
		do k=1,9
		x2(i,j,k+1)=k
		x5(i,j,k+1)=k
		enddo
	enddo
enddo
do q1=1,2042

!elimination process

do 

6 do i=1,9
	do j=1,9
	
if(x(i,j)==0) then
		do l=1,9
			do k=1,9
	
		if(i .ne. l) then
			if(x2(i,j,k+1)==x(l,j)) then
			x2(i,j,k+1)=0
		if(q1==1) then
			x5(i,j,k+1)=0
		endif                  
		endif	
		endif
	             if(i .ne. l) then
			if(x2(i,j,k+1)==x(i,l)) then
			x2(i,j,k+1)=0	
			if(q1==1) then
			x5(i,j,k+1)=0
			endif
			
                        endif
			endif

			enddo
		enddo

	e=i
	f=j
	call rowfin(e,f)
		do l=e,e+2
			do k=f,f+2
				do m=2,10
				if(x(l,k)==x2(i,j,m)) then
				x2(i,j,m)=0
if(q1==1) then

			x5(i,j,m)=0
			
endif

				endif
				enddo
			enddo
		enddo
	
	a3=0
		do l=2,10
		if(x2(i,j,l).ne.0) then
		a3=a3+1
		a4=x2(i,j,l)
		a5=l
		endif
		enddo
	if(a3==1) then
	x(i,j)=a4


	
	x2(i,j,a5)=0

if(q1==1) then
			x5(i,j,a5)=0
			endif
	endif


endif
	
	enddo
enddo



!elimination completed
!crooks method starts
do i=1,9
	do j=1,9
	if(x(i,j)==0) then
	e=i
	f=j
	call rowfin(e,f)
		do l=e,e+2  
			do m=f,f+2  
			a=0
			a2=0
			if(x(l,m)==0) then 
				do o=2,10 
				do n=2,10  
				if(x2(i,j,n) .ne. 0) then				
				if(x2(i,j,n) .eq. x2(l,m,o)) then
				a=a+1
				else
				a2=a2+1
				endif
				endif
				enddo 
				enddo 
			if(a2==0) then 
					do n=1,9 
					if(y(n,1)==0) then
					y(n,1)=e
					y(n,2)=f
					endif
					enddo  
			endif 
			endif 
enddo  
			enddo   




do l=1,9
if(y(l,1).ne. 0) then
z=z+1
elseif(x2(i,j,l+1) .ne. 0) then
z1=z1+1
endif
enddo


if(z==z1) then 
do l=e,e+2
	do m=f,f+2
		do n=2,10
			do o=2,10
			if(x2(l,m,o)==x2(i,j,n)) then
			x2(l,m,o)=0
	if(q1==1) then
		x5(l,m,o)=0
endif
			endif
			enddo

		enddo
	enddo
enddo

endif



	endif
	enddo!older ones
enddo
! crooks elimination ends



!assinging values if possible 
u=0
do i=1,9
	do j=1,9
		do k=2,10
		if(x2(i,j,k).ne.0) then
		u=u+1
		u2=x2(i,j,k)
		endif
		enddo
	if(u==1) then
	x(i,j)=u2
	ll=ll+1
	endif
	enddo
enddo
!assinging values completed 


!another elimination starts 
do i=1,9
	do j=1,9
		do k=1,9
			do l=1,9
			if(x2(i,j,k+1)==x3(l,1)) then
			x3(l,2)=x3(l,2)+1
			y2=j
			endif
			enddo
		enddo

				do k=1,9
				if(x3(k,2)==1) then
				x(i,y2)=x3(k,1)
				ll=ll+1
				endif
				enddo


	enddo
enddo



do i=1,9
	do j=1,9
		do k=1,9
			do l=1,9
			if(x2(j,i,k+1)==x3(l,1)) then
			x3(l,2)=x3(l,2)+1
			y2=j
			endif
			enddo
		enddo

				do k=1,9
				if(x3(k,2)==1) then
				x(j,y2)=x3(k,1)
				ll=ll+1
				endif
				enddo

enddo
enddo
!another elimination complete 
if(ll==0) exit
enddo
if(h15==1) then
h15=0
goto 7
endif
!checking weather the solution is correct 

h3=0
do k=1,9
do l=1,9

if(x(k,l).ne.0) then
h3=h3+1
endif
enddo
enddo

if(h3==81) then

do k=1,9
!populating started 
ch(k,1)=k
ch(k,2)=0
ch(k,3)=0
ch(k,4)=0
enddo
!populating completed
t2=0
do i=1,9
do j=1,9
do k=1,9
if(ch(k,1)==x(i,j)) then
ch(k,2)=ch(k,2)+1
endif
enddo

enddo

do k=1,9
if(ch(k,2)>1)then
t2=t2+1
endif
enddo
do k=1,9
ch(k,2)=0
enddo

enddo


t3=0
do i=1,9
do j=1,9
do k=1,9
if(ch(k,1)==x(j,i)) then 
ch(k,3)=ch(k,3)+1
endif
enddo
enddo


do k=1,9
if(ch(k,3)>1) then
t3=1+t3
endif
enddo

do k=1,9
ch(k,3)=0
enddo

enddo
t4=0
!matrix checking
do l=1,7,3
	do m=1,7,3
	
		do n=l,l+2
		
			do o=m,m+2
				do q=1,9

				if(ch(q,1)==x(n,o)) then
				ch(q,4)=ch(q,4)+1
				endif

				enddo


			enddo
		enddo
	

		do n=1,9
		if(ch(n,4)>1) then
		t4=t4+1 

		endif
		enddo
		
	

		do n=1,9
		ch(n,4)=0
		enddo
enddo
enddo



if(t2==0) then
if(t3==0) then
if(t4==0) then

t1=2
else
t1=1
endif
else
t1=1
endif
else
t1=1
endif

else
t1=0
endif

h11=0
h12=0
!checking completed 


!backtracking starts 
if(t1==0) then
 1 do i=1,81
ch3(i)=0
enddo
do i=1,9
	do j=1,9
	if(x(i,j)==0) then
		do k=2,10
		if(x5(i,j,k).ne. 0)  then
		ch3((i-1)*9+j)=ch3((i-1)*9+j)+1
		
		endif
		enddo
	endif	
	enddo
enddo

h4=0

	do j=2,9  !2
do i=1,81 !1
	if(ch3(i)== j) then
	h4=h4+1
	

	if(h4==1) then
	
	h5=mod(i,9)
	
	if(h5==0) then 
	h5=9
	endif
	h6=(i-h5)/9+1

	do k=2,10 !4
		
if(x5(h6,h5,k) .ne. 0) then 
			do l=1,81 !3
			if(x4(l,1)==0) then
			h12=h12+1
			if(h12==1) then		
			x4(l,1)=h6
			x4(l,2)=h5
			x4(l,3)=x5(h6,h5,k)
			x(h6,h5)=x4(l,3)
			


			h11=1
			endif
			endif
			enddo !3
		endif
			
		enddo !4
		
endif
endif
		
	enddo !2
enddo !1
h5=0
h6=0
if(h11==0) then
t1=1

endif
endif

if(h11==0) then
endif


if(t1==1) then
7 h13=0

do i=1,81

if(x4(i,1)==0) then
h13=h13+1
if(h13==1) then 

v1=x4(i-1,1)
v2=x4(i-1,2)
v3=x4(i-1,3)
q2=i-1

endif
endif
if(h13==1) exit
enddo

do i=2,10
if(x5(v1,v2,i)==v3) then
v5=i

endif
enddo

do i=2,10
if(x5(v1,v2,i) .ne. 0) then
v6=i

endif
enddo

if(v5.ne. v6) then
v9=0



do i=v5+1,10

if(x5(v1,v2,i) .ne. 0) then
v9=v9+1
if(v9==1) then
x(v1,v2)=x5(v1,v2,i)
x4(q2,3)=x5(v1,v2,i)
endif
endif
enddo

elseif(v5==v6) then
x(v1,v2)=0

x4(q2,1)=0
x4(q2,2)=0
x4(q2,3)=0
h15=1
goto 6


endif


endif

enddo
!backtracking completed 
do i=1,15
print*,x4(i,1),x4(i,2),x4(i,3)
enddo

do i=1,9
print*,x(i,1),x(i,2),x(i,3),x(i,4),x(i,5),x(i,6),x(i,7),x(i,8),x(i,9)
enddo


stop
end program

subroutine rowfin(e,f)
implicit none
integer e,f,g,h

g=mod(e,3)
h=mod(f,3)

if(g==0) then
e=e-2
elseif(g==1) then
e=e
elseif(g==2) then
e=e-1
endif

if(h==0) then
f=f-2
elseif(h==1) then
f=f
elseif(h==2) then
f=f-1
endif
end subroutine


