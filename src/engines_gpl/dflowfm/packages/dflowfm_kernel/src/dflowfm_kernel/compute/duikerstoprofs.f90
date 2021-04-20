 subroutine duikerstoprofs()
 use m_netw
 use m_flowgeom
 use m_flow
 use m_missing
 use m_polygon
 use unstruc_model
 use m_sferic
 use geometry_module, only: dbdistance
 use m_partitioninfo
 implicit none

 integer          :: minp, Ls, Lf, n, k1, k2
 double precision :: x1,y1,z1,x2,y2,z2,xc,yc,XLS,YLS,dum,w1,w2,h1,h2
 logical          :: jawel

 inquire(file=trim(md_pipefile) , exist=jawel)
 if (.not. jawel) return

 call oldfil(minp, md_pipefile)
 call reapol(minp,0)
 allocate(jaduiktmp(1:Lnx1D)) ; jaduiktmp = 0

 do n  = 1,npl-1

    x1 = xpl(n)   ; y1 = ypl(n)   ; z1 = zpl(n)   ; w1 = dzL(n)   ; h1 = dzR(n)
    x2 = xpl(n+1) ; y2 = ypl(n+1) ; z2 = zpl(n+1) ; w2 = dzL(n+1) ; h2 = dzR(n+1)
    if (x1 == DMISS .or. x2 == DMISS) cycle
    if (w1 <= 0d0 .or. w2 <= 0d0) then
       call qnerror(' pipes: width <= 0d0, fourth column', 'in', md_pipefile)
       call qnerror(' pipes: width <= 0d0, fourth column', 'in', md_pipefile)
    endif

    if (x1 .ne. dmiss .and. x2 .ne. dmiss) then
        xc = 0.5d0*(x1+x2)
        yc = 0.5d0*(y1+y2)
        CALL CLOSETO1Dnetlink(Xc,Yc,LS,XLS,YLS,dum, 0)
        if (Ls > 0) then
            Lf = lne2ln(Ls)
            if (kcu(Lf) == 1 .or. kcu(Lf) == 5) then
               k1 = ln(1,Lf) ; k2 = ln(2,Lf)
               if (jampi == 1) then
                  if (idomain(k1) /= my_rank .and. idomain(k2)  /= my_rank) cycle
               endif
               IF ( dbdistance(X1,Y1,Xzw(K1),Yzw(K1), jsferic, jasfer3D, dmiss) < dbdistance(X1,Y1,Xzw(K2),Yzw(K2), jsferic, jasfer3D, dmiss) ) THEN
                  bob(1,Lf)  = z1 ; bl(k1) = min(z1, bl(k1) )
                  bob(2,Lf)  = z2 ; bl(k2) = min(z2, bl(k2) )
                else
                  bob(1,Lf)  = z2 ; bl(k1) = min(z2, bl(k1) )
                  bob(2,Lf)  = z1 ; bl(k2) = min(z1, bl(k2) )
               endif
               prof1D(1,Lf)  = w1 ; wu(Lf) = w1
               prof1D(2,Lf)  = h1
               prof1D(3,Lf)  =  2                                      ! for now, simple rectan
               jaduiktmp(Lf) =  1
            endif
         endif
     endif

 enddo
 end subroutine duikerstoprofs
