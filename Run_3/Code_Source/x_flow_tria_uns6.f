
       subroutine flow_tria_uns6(re,ae,b1,b2,b3,c1,c2,c3,
     .    aaf,ndf,nen,vx,vy,p,vx1,vy1,vx2,vy2,vx3,vy3,vx4,vy4,
     .    rdl,eps,dt,
     .    vxl,vyl,vxo,vyo,volx, voly, bv)

      implicit double precision (a-h, o-z)
      
      dimension aaf(ndf*nen,ndf*nen),rdl(ndf*nen),
     .                       vx(nen), vy(nen),p(nen),
     .                       vx1(nen),vy1(nen),
     .                       vx2(nen),vy2(nen),
     .                       vx3(nen),vy3(nen),
     .                       vx4(nen),vy4(nen),
     .                       vxl(nen),vyl(nen),
     .                       vxo(nen),vyo(nen),
     .                       volx(nen), voly(nen)


      ans5=-144*vx(2)**2*b2*dt-144*vx(2)*vx(1)*b1*dt-288*vx(2)*vx(1)*
     . b2*dt-256*vx(2)*vy(6)*c2*dt-320*vx(2)*vy(5)*c2*dt-512*vx(2)*vy
     . (4)*c2*dt+176*vx(2)*vy(3)*c2*dt-144*vx(2)*vy(2)*c2*dt-288*vx(2
     . )*vy(1)*c2*dt-217*vx(2)+1248*vx(1)**2*b1*dt+768*vx(1)*vy(6)*c1
     . *dt+192*vx(1)*vy(5)*c1*dt+768*vx(1)*vy(4)*c1*dt-144*vx(1)*vy(3
     . )*c1*dt-144*vx(1)*vy(2)*c1*dt+1248*vx(1)*vy(1)*c1*dt+1302*vx(1
     . )+1568*vx1(5)+392*vx1(3)+392*vx1(2)-2352*vx1(1)-1064*vx2(5)-
     . 266*vx2(3)-266*vx2(2)+1596*vx2(1)+448*vx3(5)+112*vx3(3)+112*
     . vx3(2)-672*vx3(1)-84*vx4(5)-21*vx4(3)-21*vx4(2)+126*vx4(1)
      ans4=-256*vx(5)*vy(6)*c3*dt-768*vx(5)*vy(5)*c2*dt-768*vx(5)*vy(
     . 5)*c3*dt-256*vx(5)*vy(4)*c2*dt-512*vx(5)*vy(4)*c3*dt-256*vx(5)
     . *vy(3)*c2*dt+64*vx(5)*vy(3)*c3*dt+64*vx(5)*vy(2)*c2*dt-256*vx(
     . 5)*vy(2)*c3*dt+384*vx(5)*vy(1)*c2*dt+384*vx(5)*vy(1)*c3*dt-868
     . *vx(5)-512*vx(4)**2*b1*dt+768*vx(4)**2*b2*dt+64*vx(4)*vx(3)*b1
     . *dt-256*vx(4)*vx(3)*b2*dt-256*vx(4)*vx(3)*b3*dt-256*vx(4)*vx(2
     . )*b1*dt-768*vx(4)*vx(2)*b2*dt+1152*vx(4)*vx(1)*b1*dt+1920*vx(4
     . )*vx(1)*b2*dt-256*vx(4)*vy(6)*c1*dt+768*vx(4)*vy(6)*c2*dt-768*
     . vx(4)*vy(5)*c1*dt-256*vx(4)*vy(5)*c2*dt-512*vx(4)*vy(4)*c1*dt+
     . 768*vx(4)*vy(4)*c2*dt+64*vx(4)*vy(3)*c1*dt-256*vx(4)*vy(3)*c2*
     . dt-256*vx(4)*vy(2)*c1*dt-256*vx(4)*vy(2)*c2*dt+384*vx(4)*vy(1)
     . *c1*dt+1920*vx(4)*vy(1)*c2*dt-144*vx(3)**2*b3*dt+176*vx(3)*vx(
     . 2)*b2*dt+176*vx(3)*vx(2)*b3*dt-144*vx(3)*vx(1)*b1*dt-288*vx(3)
     . *vx(1)*b3*dt-512*vx(3)*vy(6)*c3*dt-320*vx(3)*vy(5)*c3*dt-256*
     . vx(3)*vy(4)*c3*dt-144*vx(3)*vy(3)*c3*dt+176*vx(3)*vy(2)*c3*dt-
     . 288*vx(3)*vy(1)*c3*dt-217*vx(3)+ans5
      ans3=-6720*p(1)*b1*dt+448*volx(5)*dt+112*volx(3)*dt+112*volx(2)
     . *dt-672*volx(1)*dt-512*vx(6)**2*b1*dt+768*vx(6)**2*b3*dt-768*
     . vx(6)*vx(5)*b1*dt-512*vx(6)*vx(5)*b2*dt-512*vx(6)*vx(5)*b3*dt-
     . 512*vx(6)*vx(4)*b1*dt+768*vx(6)*vx(4)*b2*dt+768*vx(6)*vx(4)*b3
     . *dt-256*vx(6)*vx(3)*b1*dt-768*vx(6)*vx(3)*b3*dt+64*vx(6)*vx(2)
     . *b1*dt-256*vx(6)*vx(2)*b2*dt-256*vx(6)*vx(2)*b3*dt+1152*vx(6)*
     . vx(1)*b1*dt+1920*vx(6)*vx(1)*b3*dt-512*vx(6)*vy(6)*c1*dt+768*
     . vx(6)*vy(6)*c3*dt-768*vx(6)*vy(5)*c1*dt-256*vx(6)*vy(5)*c3*dt-
     . 256*vx(6)*vy(4)*c1*dt+768*vx(6)*vy(4)*c3*dt-256*vx(6)*vy(3)*c1
     . *dt-256*vx(6)*vy(3)*c3*dt+64*vx(6)*vy(2)*c1*dt-256*vx(6)*vy(2)
     . *c3*dt+384*vx(6)*vy(1)*c1*dt+1920*vx(6)*vy(1)*c3*dt-768*vx(5)
     . **2*b2*dt-768*vx(5)**2*b3*dt-768*vx(5)*vx(4)*b1*dt-512*vx(5)*
     . vx(4)*b2*dt-512*vx(5)*vx(4)*b3*dt-256*vx(5)*vx(3)*b2*dt-256*vx
     . (5)*vx(3)*b3*dt-256*vx(5)*vx(2)*b2*dt-256*vx(5)*vx(2)*b3*dt+
     . 192*vx(5)*vx(1)*b1*dt+384*vx(5)*vx(1)*b2*dt+384*vx(5)*vx(1)*b3
     . *dt-512*vx(5)*vy(6)*c2*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(6)*b1*b3*bv+4*vx(6)*b1*b3+4*vx(6)*c1*c3+4
     . *vx(4)*b1*b2*bv+4*vx(4)*b1*b2+4*vx(4)*c1*c2-vx(3)*b1*b3*bv-vx(
     . 3)*b1*b3-vx(3)*c1*c3-vx(2)*b1*b2*bv-vx(2)*b1*b2-vx(2)*c1*c2+3*
     . vx(1)*b1**2*bv+3*vx(1)*b1**2+3*vx(1)*c1**2+4*vy(6)*b1*bv*c3+4*
     . vy(4)*b1*bv*c2-vy(3)*b1*bv*c3-vy(2)*b1*bv*c2+3*vy(1)*b1*bv*c1)
      ans1=ans2+ans6
      rdl(1)=ans1/(20160*dt)
      ans5=-256*vy(4)*vy(3)*c2*dt-256*vy(4)*vy(3)*c3*dt-256*vy(4)*vy(
     . 2)*c1*dt-768*vy(4)*vy(2)*c2*dt+1152*vy(4)*vy(1)*c1*dt+1920*vy(
     . 4)*vy(1)*c2*dt-144*vy(3)**2*c3*dt+176*vy(3)*vy(2)*c2*dt+176*vy
     . (3)*vy(2)*c3*dt-144*vy(3)*vy(1)*c1*dt-288*vy(3)*vy(1)*c3*dt-
     . 217*vy(3)-144*vy(2)**2*c2*dt-144*vy(2)*vy(1)*c1*dt-288*vy(2)*
     . vy(1)*c2*dt-217*vy(2)+1248*vy(1)**2*c1*dt+1302*vy(1)+1568*vy1(
     . 5)+392*vy1(3)+392*vy1(2)-2352*vy1(1)-1064*vy2(5)-266*vy2(3)-
     . 266*vy2(2)+1596*vy2(1)+448*vy3(5)+112*vy3(3)+112*vy3(2)-672*
     . vy3(1)-84*vy4(5)-21*vy4(3)-21*vy4(2)+126*vy4(1)
      ans4=-256*vx(2)*vy(4)*b1*dt-256*vx(2)*vy(4)*b2*dt+176*vx(2)*vy(
     . 3)*b3*dt-144*vx(2)*vy(2)*b2*dt-144*vx(2)*vy(1)*b1*dt+384*vx(1)
     . *vy(6)*b1*dt+1920*vx(1)*vy(6)*b3*dt+384*vx(1)*vy(5)*b2*dt+384*
     . vx(1)*vy(5)*b3*dt+384*vx(1)*vy(4)*b1*dt+1920*vx(1)*vy(4)*b2*dt
     . -288*vx(1)*vy(3)*b3*dt-288*vx(1)*vy(2)*b2*dt+1248*vx(1)*vy(1)*
     . b1*dt-512*vy(6)**2*c1*dt+768*vy(6)**2*c3*dt-768*vy(6)*vy(5)*c1
     . *dt-512*vy(6)*vy(5)*c2*dt-512*vy(6)*vy(5)*c3*dt-512*vy(6)*vy(4
     . )*c1*dt+768*vy(6)*vy(4)*c2*dt+768*vy(6)*vy(4)*c3*dt-256*vy(6)*
     . vy(3)*c1*dt-768*vy(6)*vy(3)*c3*dt+64*vy(6)*vy(2)*c1*dt-256*vy(
     . 6)*vy(2)*c2*dt-256*vy(6)*vy(2)*c3*dt+1152*vy(6)*vy(1)*c1*dt+
     . 1920*vy(6)*vy(1)*c3*dt-768*vy(5)**2*c2*dt-768*vy(5)**2*c3*dt-
     . 768*vy(5)*vy(4)*c1*dt-512*vy(5)*vy(4)*c2*dt-512*vy(5)*vy(4)*c3
     . *dt-256*vy(5)*vy(3)*c2*dt-256*vy(5)*vy(3)*c3*dt-256*vy(5)*vy(2
     . )*c2*dt-256*vy(5)*vy(2)*c3*dt+192*vy(5)*vy(1)*c1*dt+384*vy(5)*
     . vy(1)*c2*dt+384*vy(5)*vy(1)*c3*dt-868*vy(5)-512*vy(4)**2*c1*dt
     . +768*vy(4)**2*c2*dt+64*vy(4)*vy(3)*c1*dt+ans5
      ans3=-6720*p(1)*c1*dt+448*voly(5)*dt+112*voly(3)*dt+112*voly(2)
     . *dt-672*voly(1)*dt-512*vx(6)*vy(6)*b1*dt+768*vx(6)*vy(6)*b3*dt
     . -512*vx(6)*vy(5)*b2*dt-256*vx(6)*vy(5)*b3*dt-256*vx(6)*vy(4)*
     . b1*dt+768*vx(6)*vy(4)*b2*dt-512*vx(6)*vy(3)*b3*dt-256*vx(6)*vy
     . (2)*b2*dt+768*vx(6)*vy(1)*b1*dt-768*vx(5)*vy(6)*b1*dt-256*vx(5
     . )*vy(6)*b3*dt-768*vx(5)*vy(5)*b2*dt-768*vx(5)*vy(5)*b3*dt-768*
     . vx(5)*vy(4)*b1*dt-256*vx(5)*vy(4)*b2*dt-320*vx(5)*vy(3)*b3*dt-
     . 320*vx(5)*vy(2)*b2*dt+192*vx(5)*vy(1)*b1*dt-256*vx(4)*vy(6)*b1
     . *dt+768*vx(4)*vy(6)*b3*dt-256*vx(4)*vy(5)*b2*dt-512*vx(4)*vy(5
     . )*b3*dt-512*vx(4)*vy(4)*b1*dt+768*vx(4)*vy(4)*b2*dt-256*vx(4)*
     . vy(3)*b3*dt-512*vx(4)*vy(2)*b2*dt+768*vx(4)*vy(1)*b1*dt-256*vx
     . (3)*vy(6)*b1*dt-256*vx(3)*vy(6)*b3*dt-256*vx(3)*vy(5)*b2*dt+64
     . *vx(3)*vy(5)*b3*dt+64*vx(3)*vy(4)*b1*dt-256*vx(3)*vy(4)*b2*dt-
     . 144*vx(3)*vy(3)*b3*dt+176*vx(3)*vy(2)*b2*dt-144*vx(3)*vy(1)*b1
     . *dt+64*vx(2)*vy(6)*b1*dt-256*vx(2)*vy(6)*b3*dt+64*vx(2)*vy(5)*
     . b2*dt-256*vx(2)*vy(5)*b3*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(6)*b3*bv*c1+4*vx(4)*b2*bv*c1-vx(3)*b3*bv*
     . c1-vx(2)*b2*bv*c1+3*vx(1)*b1*bv*c1+4*vy(6)*b1*b3+4*vy(6)*bv*c1
     . *c3+4*vy(6)*c1*c3+4*vy(4)*b1*b2+4*vy(4)*bv*c1*c2+4*vy(4)*c1*c2
     . -vy(3)*b1*b3-vy(3)*bv*c1*c3-vy(3)*c1*c3-vy(2)*b1*b2-vy(2)*bv*
     . c1*c2-vy(2)*c1*c2+3*vy(1)*b1**2+3*vy(1)*bv*c1**2+3*vy(1)*c1**2
     . )
      ans1=ans2+ans6
      rdl(2)=ans1/(20160*dt)
      rdl(3)=(re*ae*(vx(6)*b1+2*vx(6)*b3+vx(5)*b2+vx(5)*b3+vx(4)*b1+2
     . *vx(4)*b2+vx(1)*b1+vy(6)*c1+2*vy(6)*c3+vy(5)*c2+vy(5)*c3+vy(4)
     . *c1+2*vy(4)*c2+vy(1)*c1))/3
      ans5=-288*vx(2)*vx(1)*b1*dt-144*vx(2)*vx(1)*b2*dt+192*vx(2)*vy(
     . 6)*c2*dt+768*vx(2)*vy(5)*c2*dt+768*vx(2)*vy(4)*c2*dt-144*vx(2)
     . *vy(3)*c2*dt+1248*vx(2)*vy(2)*c2*dt-144*vx(2)*vy(1)*c2*dt+1302
     . *vx(2)-144*vx(1)**2*b1*dt-320*vx(1)*vy(6)*c1*dt-256*vx(1)*vy(5
     . )*c1*dt-512*vx(1)*vy(4)*c1*dt+176*vx(1)*vy(3)*c1*dt-288*vx(1)*
     . vy(2)*c1*dt-144*vx(1)*vy(1)*c1*dt-217*vx(1)+1568*vx1(6)+392*
     . vx1(3)-2352*vx1(2)+392*vx1(1)-1064*vx2(6)-266*vx2(3)+1596*vx2(
     . 2)-266*vx2(1)+448*vx3(6)+112*vx3(3)-672*vx3(2)+112*vx3(1)-84*
     . vx4(6)-21*vx4(3)+126*vx4(2)-21*vx4(1)
      ans4=-256*vx(5)*vy(6)*c3*dt-512*vx(5)*vy(5)*c2*dt+768*vx(5)*vy(
     . 5)*c3*dt-256*vx(5)*vy(4)*c2*dt+768*vx(5)*vy(4)*c3*dt-256*vx(5)
     . *vy(3)*c2*dt-256*vx(5)*vy(3)*c3*dt+384*vx(5)*vy(2)*c2*dt+1920*
     . vx(5)*vy(2)*c3*dt+64*vx(5)*vy(1)*c2*dt-256*vx(5)*vy(1)*c3*dt+
     . 768*vx(4)**2*b1*dt-512*vx(4)**2*b2*dt-256*vx(4)*vx(3)*b1*dt+64
     . *vx(4)*vx(3)*b2*dt-256*vx(4)*vx(3)*b3*dt+1920*vx(4)*vx(2)*b1*
     . dt+1152*vx(4)*vx(2)*b2*dt-768*vx(4)*vx(1)*b1*dt-256*vx(4)*vx(1
     . )*b2*dt-256*vx(4)*vy(6)*c1*dt-768*vx(4)*vy(6)*c2*dt+768*vx(4)*
     . vy(5)*c1*dt-256*vx(4)*vy(5)*c2*dt+768*vx(4)*vy(4)*c1*dt-512*vx
     . (4)*vy(4)*c2*dt-256*vx(4)*vy(3)*c1*dt+64*vx(4)*vy(3)*c2*dt+
     . 1920*vx(4)*vy(2)*c1*dt+384*vx(4)*vy(2)*c2*dt-256*vx(4)*vy(1)*
     . c1*dt-256*vx(4)*vy(1)*c2*dt-144*vx(3)**2*b3*dt-144*vx(3)*vx(2)
     . *b2*dt-288*vx(3)*vx(2)*b3*dt+176*vx(3)*vx(1)*b1*dt+176*vx(3)*
     . vx(1)*b3*dt-320*vx(3)*vy(6)*c3*dt-512*vx(3)*vy(5)*c3*dt-256*vx
     . (3)*vy(4)*c3*dt-144*vx(3)*vy(3)*c3*dt-288*vx(3)*vy(2)*c3*dt+
     . 176*vx(3)*vy(1)*c3*dt-217*vx(3)+1248*vx(2)**2*b2*dt+ans5
      ans3=-6720*p(2)*b2*dt+448*volx(6)*dt+112*volx(3)*dt-672*volx(2)
     . *dt+112*volx(1)*dt-768*vx(6)**2*b1*dt-768*vx(6)**2*b3*dt-512*
     . vx(6)*vx(5)*b1*dt-768*vx(6)*vx(5)*b2*dt-512*vx(6)*vx(5)*b3*dt-
     . 512*vx(6)*vx(4)*b1*dt-768*vx(6)*vx(4)*b2*dt-512*vx(6)*vx(4)*b3
     . *dt-256*vx(6)*vx(3)*b1*dt-256*vx(6)*vx(3)*b3*dt+384*vx(6)*vx(2
     . )*b1*dt+192*vx(6)*vx(2)*b2*dt+384*vx(6)*vx(2)*b3*dt-256*vx(6)*
     . vx(1)*b1*dt-256*vx(6)*vx(1)*b3*dt-768*vx(6)*vy(6)*c1*dt-768*vx
     . (6)*vy(6)*c3*dt-512*vx(6)*vy(5)*c1*dt-256*vx(6)*vy(5)*c3*dt-
     . 256*vx(6)*vy(4)*c1*dt-512*vx(6)*vy(4)*c3*dt-256*vx(6)*vy(3)*c1
     . *dt+64*vx(6)*vy(3)*c3*dt+384*vx(6)*vy(2)*c1*dt+384*vx(6)*vy(2)
     . *c3*dt+64*vx(6)*vy(1)*c1*dt-256*vx(6)*vy(1)*c3*dt-868*vx(6)-
     . 512*vx(5)**2*b2*dt+768*vx(5)**2*b3*dt+768*vx(5)*vx(4)*b1*dt-
     . 512*vx(5)*vx(4)*b2*dt+768*vx(5)*vx(4)*b3*dt-256*vx(5)*vx(3)*b2
     . *dt-768*vx(5)*vx(3)*b3*dt+1152*vx(5)*vx(2)*b2*dt+1920*vx(5)*vx
     . (2)*b3*dt-256*vx(5)*vx(1)*b1*dt+64*vx(5)*vx(1)*b2*dt-256*vx(5)
     . *vx(1)*b3*dt-768*vx(5)*vy(6)*c2*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(5)*b2*b3*bv+4*vx(5)*b2*b3+4*vx(5)*c2*c3+4
     . *vx(4)*b1*b2*bv+4*vx(4)*b1*b2+4*vx(4)*c1*c2-vx(3)*b2*b3*bv-vx(
     . 3)*b2*b3-vx(3)*c2*c3+3*vx(2)*b2**2*bv+3*vx(2)*b2**2+3*vx(2)*c2
     . **2-vx(1)*b1*b2*bv-vx(1)*b1*b2-vx(1)*c1*c2+4*vy(5)*b2*bv*c3+4*
     . vy(4)*b2*bv*c1-vy(3)*b2*bv*c3+3*vy(2)*b2*bv*c2-vy(1)*b2*bv*c1)
      ans1=ans2+ans6
      rdl(4)=ans1/(20160*dt)
      ans5=-256*vy(4)*vy(3)*c3*dt+1920*vy(4)*vy(2)*c1*dt+1152*vy(4)*
     . vy(2)*c2*dt-768*vy(4)*vy(1)*c1*dt-256*vy(4)*vy(1)*c2*dt-144*vy
     . (3)**2*c3*dt-144*vy(3)*vy(2)*c2*dt-288*vy(3)*vy(2)*c3*dt+176*
     . vy(3)*vy(1)*c1*dt+176*vy(3)*vy(1)*c3*dt-217*vy(3)+1248*vy(2)**
     . 2*c2*dt-288*vy(2)*vy(1)*c1*dt-144*vy(2)*vy(1)*c2*dt+1302*vy(2)
     . -144*vy(1)**2*c1*dt-217*vy(1)+1568*vy1(6)+392*vy1(3)-2352*vy1(
     . 2)+392*vy1(1)-1064*vy2(6)-266*vy2(3)+1596*vy2(2)-266*vy2(1)+
     . 448*vy3(6)+112*vy3(3)-672*vy3(2)+112*vy3(1)-84*vy4(6)-21*vy4(3
     . )+126*vy4(2)-21*vy4(1)
      ans4=384*vx(2)*vy(4)*b2*dt-288*vx(2)*vy(3)*b3*dt+1248*vx(2)*vy(
     . 2)*b2*dt-288*vx(2)*vy(1)*b1*dt+64*vx(1)*vy(6)*b1*dt-256*vx(1)*
     . vy(6)*b3*dt+64*vx(1)*vy(5)*b2*dt-256*vx(1)*vy(5)*b3*dt-256*vx(
     . 1)*vy(4)*b1*dt-256*vx(1)*vy(4)*b2*dt+176*vx(1)*vy(3)*b3*dt-144
     . *vx(1)*vy(2)*b2*dt-144*vx(1)*vy(1)*b1*dt-768*vy(6)**2*c1*dt-
     . 768*vy(6)**2*c3*dt-512*vy(6)*vy(5)*c1*dt-768*vy(6)*vy(5)*c2*dt
     . -512*vy(6)*vy(5)*c3*dt-512*vy(6)*vy(4)*c1*dt-768*vy(6)*vy(4)*
     . c2*dt-512*vy(6)*vy(4)*c3*dt-256*vy(6)*vy(3)*c1*dt-256*vy(6)*vy
     . (3)*c3*dt+384*vy(6)*vy(2)*c1*dt+192*vy(6)*vy(2)*c2*dt+384*vy(6
     . )*vy(2)*c3*dt-256*vy(6)*vy(1)*c1*dt-256*vy(6)*vy(1)*c3*dt-868*
     . vy(6)-512*vy(5)**2*c2*dt+768*vy(5)**2*c3*dt+768*vy(5)*vy(4)*c1
     . *dt-512*vy(5)*vy(4)*c2*dt+768*vy(5)*vy(4)*c3*dt-256*vy(5)*vy(3
     . )*c2*dt-768*vy(5)*vy(3)*c3*dt+1152*vy(5)*vy(2)*c2*dt+1920*vy(5
     . )*vy(2)*c3*dt-256*vy(5)*vy(1)*c1*dt+64*vy(5)*vy(1)*c2*dt-256*
     . vy(5)*vy(1)*c3*dt+768*vy(4)**2*c1*dt-512*vy(4)**2*c2*dt-256*vy
     . (4)*vy(3)*c1*dt+64*vy(4)*vy(3)*c2*dt+ans5
      ans3=-6720*p(2)*c2*dt+448*voly(6)*dt+112*voly(3)*dt-672*voly(2)
     . *dt+112*voly(1)*dt-768*vx(6)*vy(6)*b1*dt-768*vx(6)*vy(6)*b3*dt
     . -768*vx(6)*vy(5)*b2*dt-256*vx(6)*vy(5)*b3*dt-256*vx(6)*vy(4)*
     . b1*dt-768*vx(6)*vy(4)*b2*dt-320*vx(6)*vy(3)*b3*dt+192*vx(6)*vy
     . (2)*b2*dt-320*vx(6)*vy(1)*b1*dt-512*vx(5)*vy(6)*b1*dt-256*vx(5
     . )*vy(6)*b3*dt-512*vx(5)*vy(5)*b2*dt+768*vx(5)*vy(5)*b3*dt+768*
     . vx(5)*vy(4)*b1*dt-256*vx(5)*vy(4)*b2*dt-512*vx(5)*vy(3)*b3*dt+
     . 768*vx(5)*vy(2)*b2*dt-256*vx(5)*vy(1)*b1*dt-256*vx(4)*vy(6)*b1
     . *dt-512*vx(4)*vy(6)*b3*dt-256*vx(4)*vy(5)*b2*dt+768*vx(4)*vy(5
     . )*b3*dt+768*vx(4)*vy(4)*b1*dt-512*vx(4)*vy(4)*b2*dt-256*vx(4)*
     . vy(3)*b3*dt+768*vx(4)*vy(2)*b2*dt-512*vx(4)*vy(1)*b1*dt-256*vx
     . (3)*vy(6)*b1*dt+64*vx(3)*vy(6)*b3*dt-256*vx(3)*vy(5)*b2*dt-256
     . *vx(3)*vy(5)*b3*dt-256*vx(3)*vy(4)*b1*dt+64*vx(3)*vy(4)*b2*dt-
     . 144*vx(3)*vy(3)*b3*dt-144*vx(3)*vy(2)*b2*dt+176*vx(3)*vy(1)*b1
     . *dt+384*vx(2)*vy(6)*b1*dt+384*vx(2)*vy(6)*b3*dt+384*vx(2)*vy(5
     . )*b2*dt+1920*vx(2)*vy(5)*b3*dt+1920*vx(2)*vy(4)*b1*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(5)*b3*bv*c2+4*vx(4)*b1*bv*c2-vx(3)*b3*bv*
     . c2+3*vx(2)*b2*bv*c2-vx(1)*b1*bv*c2+4*vy(5)*b2*b3+4*vy(5)*bv*c2
     . *c3+4*vy(5)*c2*c3+4*vy(4)*b1*b2+4*vy(4)*bv*c1*c2+4*vy(4)*c1*c2
     . -vy(3)*b2*b3-vy(3)*bv*c2*c3-vy(3)*c2*c3+3*vy(2)*b2**2+3*vy(2)*
     . bv*c2**2+3*vy(2)*c2**2-vy(1)*b1*b2-vy(1)*bv*c1*c2-vy(1)*c1*c2)
      ans1=ans2+ans6
      rdl(5)=ans1/(20160*dt)
      rdl(6)=(re*ae*(vx(6)*b1+vx(6)*b3+vx(5)*b2+2*vx(5)*b3+2*vx(4)*b1
     . +vx(4)*b2+vx(2)*b2+vy(6)*c1+vy(6)*c3+vy(5)*c2+2*vy(5)*c3+2*vy(
     . 4)*c1+vy(4)*c2+vy(2)*c2))/3
      ans5=176*vx(2)*vx(1)*b1*dt+176*vx(2)*vx(1)*b2*dt-256*vx(2)*vy(6
     . )*c2*dt-512*vx(2)*vy(5)*c2*dt-320*vx(2)*vy(4)*c2*dt-288*vx(2)*
     . vy(3)*c2*dt-144*vx(2)*vy(2)*c2*dt+176*vx(2)*vy(1)*c2*dt-217*vx
     . (2)-144*vx(1)**2*b1*dt-512*vx(1)*vy(6)*c1*dt-256*vx(1)*vy(5)*
     . c1*dt-320*vx(1)*vy(4)*c1*dt-288*vx(1)*vy(3)*c1*dt+176*vx(1)*vy
     . (2)*c1*dt-144*vx(1)*vy(1)*c1*dt-217*vx(1)+1568*vx1(4)-2352*vx1
     . (3)+392*vx1(2)+392*vx1(1)-1064*vx2(4)+1596*vx2(3)-266*vx2(2)-
     . 266*vx2(1)+448*vx3(4)-672*vx3(3)+112*vx3(2)+112*vx3(1)-84*vx4(
     . 4)+126*vx4(3)-21*vx4(2)-21*vx4(1)
      ans4=768*vx(5)*vy(5)*c2*dt-512*vx(5)*vy(5)*c3*dt-256*vx(5)*vy(4
     . )*c2*dt-768*vx(5)*vy(4)*c3*dt+1920*vx(5)*vy(3)*c2*dt+384*vx(5)
     . *vy(3)*c3*dt-256*vx(5)*vy(2)*c2*dt-256*vx(5)*vy(2)*c3*dt-256*
     . vx(5)*vy(1)*c2*dt+64*vx(5)*vy(1)*c3*dt-768*vx(4)**2*b1*dt-768*
     . vx(4)**2*b2*dt+384*vx(4)*vx(3)*b1*dt+384*vx(4)*vx(3)*b2*dt+192
     . *vx(4)*vx(3)*b3*dt-256*vx(4)*vx(2)*b1*dt-256*vx(4)*vx(2)*b2*dt
     . -256*vx(4)*vx(1)*b1*dt-256*vx(4)*vx(1)*b2*dt-256*vx(4)*vy(6)*
     . c1*dt-512*vx(4)*vy(6)*c2*dt-512*vx(4)*vy(5)*c1*dt-256*vx(4)*vy
     . (5)*c2*dt-768*vx(4)*vy(4)*c1*dt-768*vx(4)*vy(4)*c2*dt+384*vx(4
     . )*vy(3)*c1*dt+384*vx(4)*vy(3)*c2*dt-256*vx(4)*vy(2)*c1*dt+64*
     . vx(4)*vy(2)*c2*dt+64*vx(4)*vy(1)*c1*dt-256*vx(4)*vy(1)*c2*dt-
     . 868*vx(4)+1248*vx(3)**2*b3*dt-288*vx(3)*vx(2)*b2*dt-144*vx(3)*
     . vx(2)*b3*dt-288*vx(3)*vx(1)*b1*dt-144*vx(3)*vx(1)*b3*dt+768*vx
     . (3)*vy(6)*c3*dt+768*vx(3)*vy(5)*c3*dt+192*vx(3)*vy(4)*c3*dt+
     . 1248*vx(3)*vy(3)*c3*dt-144*vx(3)*vy(2)*c3*dt-144*vx(3)*vy(1)*
     . c3*dt+1302*vx(3)-144*vx(2)**2*b2*dt+ans5
      ans3=-6720*p(3)*b3*dt+448*volx(4)*dt-672*volx(3)*dt+112*volx(2)
     . *dt+112*volx(1)*dt+768*vx(6)**2*b1*dt-512*vx(6)**2*b3*dt+768*
     . vx(6)*vx(5)*b1*dt+768*vx(6)*vx(5)*b2*dt-512*vx(6)*vx(5)*b3*dt-
     . 512*vx(6)*vx(4)*b1*dt-512*vx(6)*vx(4)*b2*dt-768*vx(6)*vx(4)*b3
     . *dt+1920*vx(6)*vx(3)*b1*dt+1152*vx(6)*vx(3)*b3*dt-256*vx(6)*vx
     . (2)*b1*dt-256*vx(6)*vx(2)*b2*dt+64*vx(6)*vx(2)*b3*dt-768*vx(6)
     . *vx(1)*b1*dt-256*vx(6)*vx(1)*b3*dt+768*vx(6)*vy(6)*c1*dt-512*
     . vx(6)*vy(6)*c3*dt+768*vx(6)*vy(5)*c1*dt-256*vx(6)*vy(5)*c3*dt-
     . 256*vx(6)*vy(4)*c1*dt-768*vx(6)*vy(4)*c3*dt+1920*vx(6)*vy(3)*
     . c1*dt+384*vx(6)*vy(3)*c3*dt-256*vx(6)*vy(2)*c1*dt+64*vx(6)*vy(
     . 2)*c3*dt-256*vx(6)*vy(1)*c1*dt-256*vx(6)*vy(1)*c3*dt+768*vx(5)
     . **2*b2*dt-512*vx(5)**2*b3*dt-512*vx(5)*vx(4)*b1*dt-512*vx(5)*
     . vx(4)*b2*dt-768*vx(5)*vx(4)*b3*dt+1920*vx(5)*vx(3)*b2*dt+1152*
     . vx(5)*vx(3)*b3*dt-768*vx(5)*vx(2)*b2*dt-256*vx(5)*vx(2)*b3*dt-
     . 256*vx(5)*vx(1)*b1*dt-256*vx(5)*vx(1)*b2*dt+64*vx(5)*vx(1)*b3*
     . dt+768*vx(5)*vy(6)*c2*dt-256*vx(5)*vy(6)*c3*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(6)*b1*b3*bv+4*vx(6)*b1*b3+4*vx(6)*c1*c3+4
     . *vx(5)*b2*b3*bv+4*vx(5)*b2*b3+4*vx(5)*c2*c3+3*vx(3)*b3**2*bv+3
     . *vx(3)*b3**2+3*vx(3)*c3**2-vx(2)*b2*b3*bv-vx(2)*b2*b3-vx(2)*c2
     . *c3-vx(1)*b1*b3*bv-vx(1)*b1*b3-vx(1)*c1*c3+4*vy(6)*b3*bv*c1+4*
     . vy(5)*b3*bv*c2+3*vy(3)*b3*bv*c3-vy(2)*b3*bv*c2-vy(1)*b3*bv*c1)
      ans1=ans2+ans6
      rdl(7)=ans1/(20160*dt)
      ans5=-256*vy(4)*vy(2)*c1*dt-256*vy(4)*vy(2)*c2*dt-256*vy(4)*vy(
     . 1)*c1*dt-256*vy(4)*vy(1)*c2*dt-868*vy(4)+1248*vy(3)**2*c3*dt-
     . 288*vy(3)*vy(2)*c2*dt-144*vy(3)*vy(2)*c3*dt-288*vy(3)*vy(1)*c1
     . *dt-144*vy(3)*vy(1)*c3*dt+1302*vy(3)-144*vy(2)**2*c2*dt+176*vy
     . (2)*vy(1)*c1*dt+176*vy(2)*vy(1)*c2*dt-217*vy(2)-144*vy(1)**2*
     . c1*dt-217*vy(1)+1568*vy1(4)-2352*vy1(3)+392*vy1(2)+392*vy1(1)-
     . 1064*vy2(4)+1596*vy2(3)-266*vy2(2)-266*vy2(1)+448*vy3(4)-672*
     . vy3(3)+112*vy3(2)+112*vy3(1)-84*vy4(4)+126*vy4(3)-21*vy4(2)-21
     . *vy4(1)
      ans4=64*vx(2)*vy(4)*b2*dt-144*vx(2)*vy(3)*b3*dt-144*vx(2)*vy(2)
     . *b2*dt+176*vx(2)*vy(1)*b1*dt-256*vx(1)*vy(6)*b1*dt-256*vx(1)*
     . vy(6)*b3*dt-256*vx(1)*vy(5)*b2*dt+64*vx(1)*vy(5)*b3*dt+64*vx(1
     . )*vy(4)*b1*dt-256*vx(1)*vy(4)*b2*dt-144*vx(1)*vy(3)*b3*dt+176*
     . vx(1)*vy(2)*b2*dt-144*vx(1)*vy(1)*b1*dt+768*vy(6)**2*c1*dt-512
     . *vy(6)**2*c3*dt+768*vy(6)*vy(5)*c1*dt+768*vy(6)*vy(5)*c2*dt-
     . 512*vy(6)*vy(5)*c3*dt-512*vy(6)*vy(4)*c1*dt-512*vy(6)*vy(4)*c2
     . *dt-768*vy(6)*vy(4)*c3*dt+1920*vy(6)*vy(3)*c1*dt+1152*vy(6)*vy
     . (3)*c3*dt-256*vy(6)*vy(2)*c1*dt-256*vy(6)*vy(2)*c2*dt+64*vy(6)
     . *vy(2)*c3*dt-768*vy(6)*vy(1)*c1*dt-256*vy(6)*vy(1)*c3*dt+768*
     . vy(5)**2*c2*dt-512*vy(5)**2*c3*dt-512*vy(5)*vy(4)*c1*dt-512*vy
     . (5)*vy(4)*c2*dt-768*vy(5)*vy(4)*c3*dt+1920*vy(5)*vy(3)*c2*dt+
     . 1152*vy(5)*vy(3)*c3*dt-768*vy(5)*vy(2)*c2*dt-256*vy(5)*vy(2)*
     . c3*dt-256*vy(5)*vy(1)*c1*dt-256*vy(5)*vy(1)*c2*dt+64*vy(5)*vy(
     . 1)*c3*dt-768*vy(4)**2*c1*dt-768*vy(4)**2*c2*dt+384*vy(4)*vy(3)
     . *c1*dt+384*vy(4)*vy(3)*c2*dt+192*vy(4)*vy(3)*c3*dt+ans5
      ans3=-6720*p(3)*c3*dt+448*voly(4)*dt-672*voly(3)*dt+112*voly(2)
     . *dt+112*voly(1)*dt+768*vx(6)*vy(6)*b1*dt-512*vx(6)*vy(6)*b3*dt
     . +768*vx(6)*vy(5)*b2*dt-256*vx(6)*vy(5)*b3*dt-256*vx(6)*vy(4)*
     . b1*dt-512*vx(6)*vy(4)*b2*dt+768*vx(6)*vy(3)*b3*dt-256*vx(6)*vy
     . (2)*b2*dt-512*vx(6)*vy(1)*b1*dt+768*vx(5)*vy(6)*b1*dt-256*vx(5
     . )*vy(6)*b3*dt+768*vx(5)*vy(5)*b2*dt-512*vx(5)*vy(5)*b3*dt-512*
     . vx(5)*vy(4)*b1*dt-256*vx(5)*vy(4)*b2*dt+768*vx(5)*vy(3)*b3*dt-
     . 512*vx(5)*vy(2)*b2*dt-256*vx(5)*vy(1)*b1*dt-256*vx(4)*vy(6)*b1
     . *dt-768*vx(4)*vy(6)*b3*dt-256*vx(4)*vy(5)*b2*dt-768*vx(4)*vy(5
     . )*b3*dt-768*vx(4)*vy(4)*b1*dt-768*vx(4)*vy(4)*b2*dt+192*vx(4)*
     . vy(3)*b3*dt-320*vx(4)*vy(2)*b2*dt-320*vx(4)*vy(1)*b1*dt+1920*
     . vx(3)*vy(6)*b1*dt+384*vx(3)*vy(6)*b3*dt+1920*vx(3)*vy(5)*b2*dt
     . +384*vx(3)*vy(5)*b3*dt+384*vx(3)*vy(4)*b1*dt+384*vx(3)*vy(4)*
     . b2*dt+1248*vx(3)*vy(3)*b3*dt-288*vx(3)*vy(2)*b2*dt-288*vx(3)*
     . vy(1)*b1*dt-256*vx(2)*vy(6)*b1*dt+64*vx(2)*vy(6)*b3*dt-256*vx(
     . 2)*vy(5)*b2*dt-256*vx(2)*vy(5)*b3*dt-256*vx(2)*vy(4)*b1*dt+
     . ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(4*vx(6)*b1*bv*c3+4*vx(5)*b2*bv*c3+3*vx(3)*b3*
     . bv*c3-vx(2)*b2*bv*c3-vx(1)*b1*bv*c3+4*vy(6)*b1*b3+4*vy(6)*bv*
     . c1*c3+4*vy(6)*c1*c3+4*vy(5)*b2*b3+4*vy(5)*bv*c2*c3+4*vy(5)*c2*
     . c3+3*vy(3)*b3**2+3*vy(3)*bv*c3**2+3*vy(3)*c3**2-vy(2)*b2*b3-vy
     . (2)*bv*c2*c3-vy(2)*c2*c3-vy(1)*b1*b3-vy(1)*bv*c1*c3-vy(1)*c1*
     . c3)
      ans1=ans2+ans6
      rdl(8)=ans1/(20160*dt)
      rdl(9)=(re*ae*(2*vx(6)*b1+vx(6)*b3+2*vx(5)*b2+vx(5)*b3+vx(4)*b1
     . +vx(4)*b2+vx(3)*b3+2*vy(6)*c1+vy(6)*c3+2*vy(5)*c2+vy(5)*c3+vy(
     . 4)*c1+vy(4)*c2+vy(3)*c3))/3
      ans5=-64*vx(3)*vy(2)*c3*dt-64*vx(3)*vy(1)*c3*dt-217*vx(3)+192*
     . vx(2)**2*b2*dt-128*vx(2)*vx(1)*b1*dt-128*vx(2)*vx(1)*b2*dt+64*
     . vx(2)*vy(6)*c2*dt+320*vx(2)*vy(5)*c2*dt+640*vx(2)*vy(4)*c2*dt-
     . 80*vx(2)*vy(3)*c2*dt+192*vx(2)*vy(2)*c2*dt-128*vx(2)*vy(1)*c2*
     . dt+192*vx(1)**2*b1*dt+320*vx(1)*vy(6)*c1*dt+64*vx(1)*vy(5)*c1*
     . dt+640*vx(1)*vy(4)*c1*dt-80*vx(1)*vy(3)*c1*dt-128*vx(1)*vy(2)*
     . c1*dt+192*vx(1)*vy(1)*c1*dt-1568*vx1(6)-1568*vx1(5)-3136*vx1(4
     . )+392*vx1(3)+1064*vx2(6)+1064*vx2(5)+2128*vx2(4)-266*vx2(3)-
     . 448*vx3(6)-448*vx3(5)-896*vx3(4)+112*vx3(3)+84*vx4(6)+84*vx4(5
     . )+168*vx4(4)-21*vx4(3)
      ans4=-64*vx(5)*vx(1)*b2*dt-128*vx(5)*vx(1)*b3*dt+512*vx(5)*vy(6
     . )*c2*dt+512*vx(5)*vy(6)*c3*dt+512*vx(5)*vy(5)*c2*dt+768*vx(5)*
     . vy(5)*c3*dt+512*vx(5)*vy(4)*c2*dt+1536*vx(5)*vy(4)*c3*dt-64*vx
     . (5)*vy(3)*c2*dt-192*vx(5)*vy(3)*c3*dt-64*vx(5)*vy(2)*c2*dt+192
     . *vx(5)*vy(2)*c3*dt-64*vx(5)*vy(1)*c2*dt-128*vx(5)*vy(1)*c3*dt+
     . 868*vx(5)+1536*vx(4)**2*b1*dt+1536*vx(4)**2*b2*dt-192*vx(4)*vx
     . (3)*b1*dt-192*vx(4)*vx(3)*b2*dt-384*vx(4)*vx(3)*b3*dt+192*vx(4
     . )*vx(2)*b1*dt+512*vx(4)*vx(2)*b2*dt+512*vx(4)*vx(1)*b1*dt+192*
     . vx(4)*vx(1)*b2*dt+512*vx(4)*vy(6)*c1*dt+768*vx(4)*vy(6)*c2*dt+
     . 768*vx(4)*vy(5)*c1*dt+512*vx(4)*vy(5)*c2*dt+1536*vx(4)*vy(4)*
     . c1*dt+1536*vx(4)*vy(4)*c2*dt-192*vx(4)*vy(3)*c1*dt-192*vx(4)*
     . vy(3)*c2*dt+192*vx(4)*vy(2)*c1*dt-128*vx(4)*vy(2)*c2*dt-128*vx
     . (4)*vy(1)*c1*dt+192*vx(4)*vy(1)*c2*dt+1736*vx(4)+48*vx(3)**2*
     . b3*dt-80*vx(3)*vx(2)*b2*dt-64*vx(3)*vx(2)*b3*dt-80*vx(3)*vx(1)
     . *b1*dt-64*vx(3)*vx(1)*b3*dt+64*vx(3)*vy(6)*c3*dt+64*vx(3)*vy(5
     . )*c3*dt-384*vx(3)*vy(4)*c3*dt+48*vx(3)*vy(3)*c3*dt+ans5
      ans3=-1680*p(3)*b1*dt-1680*p(3)*b2*dt-3360*p(2)*b1*dt-1680*p(2)
     . *b2*dt-1680*p(1)*b1*dt-3360*p(1)*b2*dt-448*volx(6)*dt-448*volx
     . (5)*dt-896*volx(4)*dt+112*volx(3)*dt+512*vx(6)**2*b1*dt+768*vx
     . (6)**2*b3*dt+512*vx(6)*vx(5)*b1*dt+512*vx(6)*vx(5)*b2*dt+1024*
     . vx(6)*vx(5)*b3*dt+1024*vx(6)*vx(4)*b1*dt+768*vx(6)*vx(4)*b2*dt
     . +1536*vx(6)*vx(4)*b3*dt-64*vx(6)*vx(3)*b1*dt-128*vx(6)*vx(3)*
     . b3*dt-64*vx(6)*vx(2)*b1*dt+64*vx(6)*vx(2)*b2*dt-128*vx(6)*vx(2
     . )*b3*dt+256*vx(6)*vx(1)*b1*dt+192*vx(6)*vx(1)*b3*dt+512*vx(6)*
     . vy(6)*c1*dt+768*vx(6)*vy(6)*c3*dt+512*vx(6)*vy(5)*c1*dt+512*vx
     . (6)*vy(5)*c3*dt+512*vx(6)*vy(4)*c1*dt+1536*vx(6)*vy(4)*c3*dt-
     . 64*vx(6)*vy(3)*c1*dt-192*vx(6)*vy(3)*c3*dt-64*vx(6)*vy(2)*c1*
     . dt-128*vx(6)*vy(2)*c3*dt-64*vx(6)*vy(1)*c1*dt+192*vx(6)*vy(1)*
     . c3*dt+868*vx(6)+512*vx(5)**2*b2*dt+768*vx(5)**2*b3*dt+768*vx(5
     . )*vx(4)*b1*dt+1024*vx(5)*vx(4)*b2*dt+1536*vx(5)*vx(4)*b3*dt-64
     . *vx(5)*vx(3)*b2*dt-128*vx(5)*vx(3)*b3*dt+256*vx(5)*vx(2)*b2*dt
     . +192*vx(5)*vx(2)*b3*dt+64*vx(5)*vx(1)*b1*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(vx(6)*b1**2*bv+vx(6)*b1**2+vx(6)*b1*b2*bv+vx(6
     . )*b1*b2+vx(6)*b1*b3*bv+vx(6)*b1*b3+2*vx(6)*b2*b3*bv+2*vx(6)*b2
     . *b3+vx(6)*c1**2+vx(6)*c1*c2+vx(6)*c1*c3+2*vx(6)*c2*c3+vx(5)*b1
     . *b2*bv+vx(5)*b1*b2+2*vx(5)*b1*b3*bv+2*vx(5)*b1*b3+vx(5)*b2**2*
     . bv+vx(5)*b2**2+vx(5)*b2*b3*bv+vx(5)*b2*b3+vx(5)*c1*c2+2*vx(5)*
     . c1*c3+vx(5)*c2**2+vx(5)*c2*c3+2*vx(4)*b1**2*bv+2*vx(4)*b1**2+2
     . *vx(4)*b1*b2*bv+2*vx(4)*b1*b2+2*vx(4)*b2**2*bv+2*vx(4)*b2**2+2
     . *vx(4)*c1**2+2*vx(4)*c1*c2+2*vx(4)*c2**2+vx(2)*b1*b2*bv+vx(2)*
     . b1*b2+vx(2)*c1*c2+vx(1)*b1*b2*bv+vx(1)*b1*b2+vx(1)*c1*c2+vy(6)
     . *b1*bv*c1+vy(6)*b1*bv*c3+vy(6)*b2*bv*c1+2*vy(6)*b2*bv*c3+vy(5)
     . *b1*bv*c2+2*vy(5)*b1*bv*c3+vy(5)*b2*bv*c2+vy(5)*b2*bv*c3+2*vy(
     . 4)*b1*bv*c1+vy(4)*b1*bv*c2+vy(4)*b2*bv*c1+2*vy(4)*b2*bv*c2+vy(
     . 2)*b1*bv*c2+vy(1)*b2*bv*c1)
      ans1=ans2+ans6
      rdl(10)=ans1/(5040*dt)
      ans5=-192*vy(4)*vy(3)*c1*dt-192*vy(4)*vy(3)*c2*dt-384*vy(4)*vy(
     . 3)*c3*dt+192*vy(4)*vy(2)*c1*dt+512*vy(4)*vy(2)*c2*dt+512*vy(4)
     . *vy(1)*c1*dt+192*vy(4)*vy(1)*c2*dt+1736*vy(4)+48*vy(3)**2*c3*
     . dt-80*vy(3)*vy(2)*c2*dt-64*vy(3)*vy(2)*c3*dt-80*vy(3)*vy(1)*c1
     . *dt-64*vy(3)*vy(1)*c3*dt-217*vy(3)+192*vy(2)**2*c2*dt-128*vy(2
     . )*vy(1)*c1*dt-128*vy(2)*vy(1)*c2*dt+192*vy(1)**2*c1*dt-1568*
     . vy1(6)-1568*vy1(5)-3136*vy1(4)+392*vy1(3)+1064*vy2(6)+1064*vy2
     . (5)+2128*vy2(4)-266*vy2(3)-448*vy3(6)-448*vy3(5)-896*vy3(4)+
     . 112*vy3(3)+84*vy4(6)+84*vy4(5)+168*vy4(4)-21*vy4(3)
      ans4=-64*vx(2)*vy(5)*b2*dt+192*vx(2)*vy(5)*b3*dt+192*vx(2)*vy(4
     . )*b1*dt-128*vx(2)*vy(4)*b2*dt-64*vx(2)*vy(3)*b3*dt+192*vx(2)*
     . vy(2)*b2*dt-128*vx(2)*vy(1)*b1*dt-64*vx(1)*vy(6)*b1*dt+192*vx(
     . 1)*vy(6)*b3*dt-64*vx(1)*vy(5)*b2*dt-128*vx(1)*vy(5)*b3*dt-128*
     . vx(1)*vy(4)*b1*dt+192*vx(1)*vy(4)*b2*dt-64*vx(1)*vy(3)*b3*dt-
     . 128*vx(1)*vy(2)*b2*dt+192*vx(1)*vy(1)*b1*dt+512*vy(6)**2*c1*dt
     . +768*vy(6)**2*c3*dt+512*vy(6)*vy(5)*c1*dt+512*vy(6)*vy(5)*c2*
     . dt+1024*vy(6)*vy(5)*c3*dt+1024*vy(6)*vy(4)*c1*dt+768*vy(6)*vy(
     . 4)*c2*dt+1536*vy(6)*vy(4)*c3*dt-64*vy(6)*vy(3)*c1*dt-128*vy(6)
     . *vy(3)*c3*dt-64*vy(6)*vy(2)*c1*dt+64*vy(6)*vy(2)*c2*dt-128*vy(
     . 6)*vy(2)*c3*dt+256*vy(6)*vy(1)*c1*dt+192*vy(6)*vy(1)*c3*dt+868
     . *vy(6)+512*vy(5)**2*c2*dt+768*vy(5)**2*c3*dt+768*vy(5)*vy(4)*
     . c1*dt+1024*vy(5)*vy(4)*c2*dt+1536*vy(5)*vy(4)*c3*dt-64*vy(5)*
     . vy(3)*c2*dt-128*vy(5)*vy(3)*c3*dt+256*vy(5)*vy(2)*c2*dt+192*vy
     . (5)*vy(2)*c3*dt+64*vy(5)*vy(1)*c1*dt-64*vy(5)*vy(1)*c2*dt-128*
     . vy(5)*vy(1)*c3*dt+868*vy(5)+1536*vy(4)**2*c1*dt+1536*vy(4)**2*
     . c2*dt+ans5
      ans3=-1680*p(3)*c1*dt-1680*p(3)*c2*dt-3360*p(2)*c1*dt-1680*p(2)
     . *c2*dt-1680*p(1)*c1*dt-3360*p(1)*c2*dt-448*voly(6)*dt-448*voly
     . (5)*dt-896*voly(4)*dt+112*voly(3)*dt+512*vx(6)*vy(6)*b1*dt+768
     . *vx(6)*vy(6)*b3*dt+512*vx(6)*vy(5)*b2*dt+512*vx(6)*vy(5)*b3*dt
     . +512*vx(6)*vy(4)*b1*dt+768*vx(6)*vy(4)*b2*dt+64*vx(6)*vy(3)*b3
     . *dt+64*vx(6)*vy(2)*b2*dt+320*vx(6)*vy(1)*b1*dt+512*vx(5)*vy(6)
     . *b1*dt+512*vx(5)*vy(6)*b3*dt+512*vx(5)*vy(5)*b2*dt+768*vx(5)*
     . vy(5)*b3*dt+768*vx(5)*vy(4)*b1*dt+512*vx(5)*vy(4)*b2*dt+64*vx(
     . 5)*vy(3)*b3*dt+320*vx(5)*vy(2)*b2*dt+64*vx(5)*vy(1)*b1*dt+512*
     . vx(4)*vy(6)*b1*dt+1536*vx(4)*vy(6)*b3*dt+512*vx(4)*vy(5)*b2*dt
     . +1536*vx(4)*vy(5)*b3*dt+1536*vx(4)*vy(4)*b1*dt+1536*vx(4)*vy(4
     . )*b2*dt-384*vx(4)*vy(3)*b3*dt+640*vx(4)*vy(2)*b2*dt+640*vx(4)*
     . vy(1)*b1*dt-64*vx(3)*vy(6)*b1*dt-192*vx(3)*vy(6)*b3*dt-64*vx(3
     . )*vy(5)*b2*dt-192*vx(3)*vy(5)*b3*dt-192*vx(3)*vy(4)*b1*dt-192*
     . vx(3)*vy(4)*b2*dt+48*vx(3)*vy(3)*b3*dt-80*vx(3)*vy(2)*b2*dt-80
     . *vx(3)*vy(1)*b1*dt-64*vx(2)*vy(6)*b1*dt-128*vx(2)*vy(6)*b3*dt+
     . ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(vx(6)*b1*bv*c1+vx(6)*b1*bv*c2+vx(6)*b3*bv*c1+2
     . *vx(6)*b3*bv*c2+vx(5)*b2*bv*c1+vx(5)*b2*bv*c2+2*vx(5)*b3*bv*c1
     . +vx(5)*b3*bv*c2+2*vx(4)*b1*bv*c1+vx(4)*b1*bv*c2+vx(4)*b2*bv*c1
     . +2*vx(4)*b2*bv*c2+vx(2)*b2*bv*c1+vx(1)*b1*bv*c2+vy(6)*b1**2+vy
     . (6)*b1*b2+vy(6)*b1*b3+2*vy(6)*b2*b3+vy(6)*bv*c1**2+vy(6)*bv*c1
     . *c2+vy(6)*bv*c1*c3+2*vy(6)*bv*c2*c3+vy(6)*c1**2+vy(6)*c1*c2+vy
     . (6)*c1*c3+2*vy(6)*c2*c3+vy(5)*b1*b2+2*vy(5)*b1*b3+vy(5)*b2**2+
     . vy(5)*b2*b3+vy(5)*bv*c1*c2+2*vy(5)*bv*c1*c3+vy(5)*bv*c2**2+vy(
     . 5)*bv*c2*c3+vy(5)*c1*c2+2*vy(5)*c1*c3+vy(5)*c2**2+vy(5)*c2*c3+
     . 2*vy(4)*b1**2+2*vy(4)*b1*b2+2*vy(4)*b2**2+2*vy(4)*bv*c1**2+2*
     . vy(4)*bv*c1*c2+2*vy(4)*bv*c2**2+2*vy(4)*c1**2+2*vy(4)*c1*c2+2*
     . vy(4)*c2**2+vy(2)*b1*b2+vy(2)*bv*c1*c2+vy(2)*c1*c2+vy(1)*b1*b2
     . +vy(1)*bv*c1*c2+vy(1)*c1*c2)
      ans1=ans2+ans6
      rdl(11)=ans1/(5040*dt)
      ans5=-128*vx(3)*vy(2)*c3*dt-80*vx(3)*vy(1)*c3*dt+192*vx(2)**2*
     . b2*dt-64*vx(2)*vx(1)*b1*dt-80*vx(2)*vx(1)*b2*dt+64*vx(2)*vy(6)
     . *c2*dt+640*vx(2)*vy(5)*c2*dt+320*vx(2)*vy(4)*c2*dt-128*vx(2)*
     . vy(3)*c2*dt+192*vx(2)*vy(2)*c2*dt-80*vx(2)*vy(1)*c2*dt+48*vx(1
     . )**2*b1*dt+64*vx(1)*vy(6)*c1*dt-384*vx(1)*vy(5)*c1*dt+64*vx(1)
     . *vy(4)*c1*dt-64*vx(1)*vy(3)*c1*dt-64*vx(1)*vy(2)*c1*dt+48*vx(1
     . )*vy(1)*c1*dt-217*vx(1)-1568*vx1(6)-3136*vx1(5)-1568*vx1(4)+
     . 392*vx1(1)+1064*vx2(6)+2128*vx2(5)+1064*vx2(4)-266*vx2(1)-448*
     . vx3(6)-896*vx3(5)-448*vx3(4)+112*vx3(1)+84*vx4(6)+168*vx4(5)+
     . 84*vx4(4)-21*vx4(1)
      ans4=-192*vx(5)*vx(1)*b2*dt-192*vx(5)*vx(1)*b3*dt+768*vx(5)*vy(
     . 6)*c2*dt+512*vx(5)*vy(6)*c3*dt+1536*vx(5)*vy(5)*c2*dt+1536*vx(
     . 5)*vy(5)*c3*dt+512*vx(5)*vy(4)*c2*dt+768*vx(5)*vy(4)*c3*dt+192
     . *vx(5)*vy(3)*c2*dt-128*vx(5)*vy(3)*c3*dt-128*vx(5)*vy(2)*c2*dt
     . +192*vx(5)*vy(2)*c3*dt-192*vx(5)*vy(1)*c2*dt-192*vx(5)*vy(1)*
     . c3*dt+1736*vx(5)+768*vx(4)**2*b1*dt+512*vx(4)**2*b2*dt-128*vx(
     . 4)*vx(3)*b1*dt-64*vx(4)*vx(3)*b2*dt+64*vx(4)*vx(3)*b3*dt+192*
     . vx(4)*vx(2)*b1*dt+256*vx(4)*vx(2)*b2*dt-128*vx(4)*vx(1)*b1*dt-
     . 64*vx(4)*vx(1)*b2*dt+512*vx(4)*vy(6)*c1*dt+512*vx(4)*vy(6)*c2*
     . dt+1536*vx(4)*vy(5)*c1*dt+512*vx(4)*vy(5)*c2*dt+768*vx(4)*vy(4
     . )*c1*dt+512*vx(4)*vy(4)*c2*dt-128*vx(4)*vy(3)*c1*dt-64*vx(4)*
     . vy(3)*c2*dt+192*vx(4)*vy(2)*c1*dt-64*vx(4)*vy(2)*c2*dt-192*vx(
     . 4)*vy(1)*c1*dt-64*vx(4)*vy(1)*c2*dt+868*vx(4)+192*vx(3)**2*b3*
     . dt-128*vx(3)*vx(2)*b2*dt-128*vx(3)*vx(2)*b3*dt-64*vx(3)*vx(1)*
     . b1*dt-80*vx(3)*vx(1)*b3*dt+320*vx(3)*vy(6)*c3*dt+640*vx(3)*vy(
     . 5)*c3*dt+64*vx(3)*vy(4)*c3*dt+192*vx(3)*vy(3)*c3*dt+ans5
      ans3=-3360*p(3)*b2*dt-1680*p(3)*b3*dt-1680*p(2)*b2*dt-3360*p(2)
     . *b3*dt-1680*p(1)*b2*dt-1680*p(1)*b3*dt-448*volx(6)*dt-896*volx
     . (5)*dt-448*volx(4)*dt+112*volx(1)*dt+768*vx(6)**2*b1*dt+512*vx
     . (6)**2*b3*dt+1536*vx(6)*vx(5)*b1*dt+768*vx(6)*vx(5)*b2*dt+1024
     . *vx(6)*vx(5)*b3*dt+1024*vx(6)*vx(4)*b1*dt+512*vx(6)*vx(4)*b2*
     . dt+512*vx(6)*vx(4)*b3*dt+192*vx(6)*vx(3)*b1*dt+256*vx(6)*vx(3)
     . *b3*dt-128*vx(6)*vx(2)*b1*dt+64*vx(6)*vx(2)*b2*dt-64*vx(6)*vx(
     . 2)*b3*dt-128*vx(6)*vx(1)*b1*dt-64*vx(6)*vx(1)*b3*dt+768*vx(6)*
     . vy(6)*c1*dt+512*vx(6)*vy(6)*c3*dt+1536*vx(6)*vy(5)*c1*dt+512*
     . vx(6)*vy(5)*c3*dt+512*vx(6)*vy(4)*c1*dt+512*vx(6)*vy(4)*c3*dt+
     . 192*vx(6)*vy(3)*c1*dt-64*vx(6)*vy(3)*c3*dt-128*vx(6)*vy(2)*c1*
     . dt-64*vx(6)*vy(2)*c3*dt-192*vx(6)*vy(1)*c1*dt-64*vx(6)*vy(1)*
     . c3*dt+868*vx(6)+1536*vx(5)**2*b2*dt+1536*vx(5)**2*b3*dt+1536*
     . vx(5)*vx(4)*b1*dt+1024*vx(5)*vx(4)*b2*dt+768*vx(5)*vx(4)*b3*dt
     . +192*vx(5)*vx(3)*b2*dt+512*vx(5)*vx(3)*b3*dt+512*vx(5)*vx(2)*
     . b2*dt+192*vx(5)*vx(2)*b3*dt-384*vx(5)*vx(1)*b1*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(2*vx(6)*b1*b2*bv+2*vx(6)*b1*b2+vx(6)*b1*b3*bv+
     . vx(6)*b1*b3+vx(6)*b2*b3*bv+vx(6)*b2*b3+vx(6)*b3**2*bv+vx(6)*b3
     . **2+2*vx(6)*c1*c2+vx(6)*c1*c3+vx(6)*c2*c3+vx(6)*c3**2+2*vx(5)*
     . b2**2*bv+2*vx(5)*b2**2+2*vx(5)*b2*b3*bv+2*vx(5)*b2*b3+2*vx(5)*
     . b3**2*bv+2*vx(5)*b3**2+2*vx(5)*c2**2+2*vx(5)*c2*c3+2*vx(5)*c3
     . **2+vx(4)*b1*b2*bv+vx(4)*b1*b2+2*vx(4)*b1*b3*bv+2*vx(4)*b1*b3+
     . vx(4)*b2**2*bv+vx(4)*b2**2+vx(4)*b2*b3*bv+vx(4)*b2*b3+vx(4)*c1
     . *c2+2*vx(4)*c1*c3+vx(4)*c2**2+vx(4)*c2*c3+vx(3)*b2*b3*bv+vx(3)
     . *b2*b3+vx(3)*c2*c3+vx(2)*b2*b3*bv+vx(2)*b2*b3+vx(2)*c2*c3+2*vy
     . (6)*b2*bv*c1+vy(6)*b2*bv*c3+vy(6)*b3*bv*c1+vy(6)*b3*bv*c3+2*vy
     . (5)*b2*bv*c2+vy(5)*b2*bv*c3+vy(5)*b3*bv*c2+2*vy(5)*b3*bv*c3+vy
     . (4)*b2*bv*c1+vy(4)*b2*bv*c2+2*vy(4)*b3*bv*c1+vy(4)*b3*bv*c2+vy
     . (3)*b2*bv*c3+vy(2)*b3*bv*c2)
      ans1=ans2+ans6
      rdl(12)=ans1/(5040*dt)
      ans5=512*vy(4)**2*c2*dt-128*vy(4)*vy(3)*c1*dt-64*vy(4)*vy(3)*c2
     . *dt+64*vy(4)*vy(3)*c3*dt+192*vy(4)*vy(2)*c1*dt+256*vy(4)*vy(2)
     . *c2*dt-128*vy(4)*vy(1)*c1*dt-64*vy(4)*vy(1)*c2*dt+868*vy(4)+
     . 192*vy(3)**2*c3*dt-128*vy(3)*vy(2)*c2*dt-128*vy(3)*vy(2)*c3*dt
     . -64*vy(3)*vy(1)*c1*dt-80*vy(3)*vy(1)*c3*dt+192*vy(2)**2*c2*dt-
     . 64*vy(2)*vy(1)*c1*dt-80*vy(2)*vy(1)*c2*dt+48*vy(1)**2*c1*dt-
     . 217*vy(1)-1568*vy1(6)-3136*vy1(5)-1568*vy1(4)+392*vy1(1)+1064*
     . vy2(6)+2128*vy2(5)+1064*vy2(4)-266*vy2(1)-448*vy3(6)-896*vy3(5
     . )-448*vy3(4)+112*vy3(1)+84*vy4(6)+168*vy4(5)+84*vy4(4)-21*vy4(
     . 1)
      ans4=-128*vx(2)*vy(5)*b2*dt+192*vx(2)*vy(5)*b3*dt+192*vx(2)*vy(
     . 4)*b1*dt-64*vx(2)*vy(4)*b2*dt-128*vx(2)*vy(3)*b3*dt+192*vx(2)*
     . vy(2)*b2*dt-64*vx(2)*vy(1)*b1*dt-192*vx(1)*vy(6)*b1*dt-64*vx(1
     . )*vy(6)*b3*dt-192*vx(1)*vy(5)*b2*dt-192*vx(1)*vy(5)*b3*dt-192*
     . vx(1)*vy(4)*b1*dt-64*vx(1)*vy(4)*b2*dt-80*vx(1)*vy(3)*b3*dt-80
     . *vx(1)*vy(2)*b2*dt+48*vx(1)*vy(1)*b1*dt+768*vy(6)**2*c1*dt+512
     . *vy(6)**2*c3*dt+1536*vy(6)*vy(5)*c1*dt+768*vy(6)*vy(5)*c2*dt+
     . 1024*vy(6)*vy(5)*c3*dt+1024*vy(6)*vy(4)*c1*dt+512*vy(6)*vy(4)*
     . c2*dt+512*vy(6)*vy(4)*c3*dt+192*vy(6)*vy(3)*c1*dt+256*vy(6)*vy
     . (3)*c3*dt-128*vy(6)*vy(2)*c1*dt+64*vy(6)*vy(2)*c2*dt-64*vy(6)*
     . vy(2)*c3*dt-128*vy(6)*vy(1)*c1*dt-64*vy(6)*vy(1)*c3*dt+868*vy(
     . 6)+1536*vy(5)**2*c2*dt+1536*vy(5)**2*c3*dt+1536*vy(5)*vy(4)*c1
     . *dt+1024*vy(5)*vy(4)*c2*dt+768*vy(5)*vy(4)*c3*dt+192*vy(5)*vy(
     . 3)*c2*dt+512*vy(5)*vy(3)*c3*dt+512*vy(5)*vy(2)*c2*dt+192*vy(5)
     . *vy(2)*c3*dt-384*vy(5)*vy(1)*c1*dt-192*vy(5)*vy(1)*c2*dt-192*
     . vy(5)*vy(1)*c3*dt+1736*vy(5)+768*vy(4)**2*c1*dt+ans5
      ans3=-3360*p(3)*c2*dt-1680*p(3)*c3*dt-1680*p(2)*c2*dt-3360*p(2)
     . *c3*dt-1680*p(1)*c2*dt-1680*p(1)*c3*dt-448*voly(6)*dt-896*voly
     . (5)*dt-448*voly(4)*dt+112*voly(1)*dt+768*vx(6)*vy(6)*b1*dt+512
     . *vx(6)*vy(6)*b3*dt+768*vx(6)*vy(5)*b2*dt+512*vx(6)*vy(5)*b3*dt
     . +512*vx(6)*vy(4)*b1*dt+512*vx(6)*vy(4)*b2*dt+320*vx(6)*vy(3)*
     . b3*dt+64*vx(6)*vy(2)*b2*dt+64*vx(6)*vy(1)*b1*dt+1536*vx(5)*vy(
     . 6)*b1*dt+512*vx(5)*vy(6)*b3*dt+1536*vx(5)*vy(5)*b2*dt+1536*vx(
     . 5)*vy(5)*b3*dt+1536*vx(5)*vy(4)*b1*dt+512*vx(5)*vy(4)*b2*dt+
     . 640*vx(5)*vy(3)*b3*dt+640*vx(5)*vy(2)*b2*dt-384*vx(5)*vy(1)*b1
     . *dt+512*vx(4)*vy(6)*b1*dt+512*vx(4)*vy(6)*b3*dt+512*vx(4)*vy(5
     . )*b2*dt+768*vx(4)*vy(5)*b3*dt+768*vx(4)*vy(4)*b1*dt+512*vx(4)*
     . vy(4)*b2*dt+64*vx(4)*vy(3)*b3*dt+320*vx(4)*vy(2)*b2*dt+64*vx(4
     . )*vy(1)*b1*dt+192*vx(3)*vy(6)*b1*dt-64*vx(3)*vy(6)*b3*dt+192*
     . vx(3)*vy(5)*b2*dt-128*vx(3)*vy(5)*b3*dt-128*vx(3)*vy(4)*b1*dt-
     . 64*vx(3)*vy(4)*b2*dt+192*vx(3)*vy(3)*b3*dt-128*vx(3)*vy(2)*b2*
     . dt-64*vx(3)*vy(1)*b1*dt-128*vx(2)*vy(6)*b1*dt-64*vx(2)*vy(6)*
     . b3*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(2*vx(6)*b1*bv*c2+vx(6)*b1*bv*c3+vx(6)*b3*bv*c2
     . +vx(6)*b3*bv*c3+2*vx(5)*b2*bv*c2+vx(5)*b2*bv*c3+vx(5)*b3*bv*c2
     . +2*vx(5)*b3*bv*c3+vx(4)*b1*bv*c2+2*vx(4)*b1*bv*c3+vx(4)*b2*bv*
     . c2+vx(4)*b2*bv*c3+vx(3)*b3*bv*c2+vx(2)*b2*bv*c3+2*vy(6)*b1*b2+
     . vy(6)*b1*b3+vy(6)*b2*b3+vy(6)*b3**2+2*vy(6)*bv*c1*c2+vy(6)*bv*
     . c1*c3+vy(6)*bv*c2*c3+vy(6)*bv*c3**2+2*vy(6)*c1*c2+vy(6)*c1*c3+
     . vy(6)*c2*c3+vy(6)*c3**2+2*vy(5)*b2**2+2*vy(5)*b2*b3+2*vy(5)*b3
     . **2+2*vy(5)*bv*c2**2+2*vy(5)*bv*c2*c3+2*vy(5)*bv*c3**2+2*vy(5)
     . *c2**2+2*vy(5)*c2*c3+2*vy(5)*c3**2+vy(4)*b1*b2+2*vy(4)*b1*b3+
     . vy(4)*b2**2+vy(4)*b2*b3+vy(4)*bv*c1*c2+2*vy(4)*bv*c1*c3+vy(4)*
     . bv*c2**2+vy(4)*bv*c2*c3+vy(4)*c1*c2+2*vy(4)*c1*c3+vy(4)*c2**2+
     . vy(4)*c2*c3+vy(3)*b2*b3+vy(3)*bv*c2*c3+vy(3)*c2*c3+vy(2)*b2*b3
     . +vy(2)*bv*c2*c3+vy(2)*c2*c3)
      ans1=ans2+ans6
      rdl(13)=ans1/(5040*dt)
      ans5=-128*vx(3)*vy(1)*c3*dt+48*vx(2)**2*b2*dt-80*vx(2)*vx(1)*b1
     . *dt-64*vx(2)*vx(1)*b2*dt-384*vx(2)*vy(6)*c2*dt+64*vx(2)*vy(5)*
     . c2*dt+64*vx(2)*vy(4)*c2*dt-64*vx(2)*vy(3)*c2*dt+48*vx(2)*vy(2)
     . *c2*dt-64*vx(2)*vy(1)*c2*dt-217*vx(2)+192*vx(1)**2*b1*dt+640*
     . vx(1)*vy(6)*c1*dt+64*vx(1)*vy(5)*c1*dt+320*vx(1)*vy(4)*c1*dt-
     . 128*vx(1)*vy(3)*c1*dt-80*vx(1)*vy(2)*c1*dt+192*vx(1)*vy(1)*c1*
     . dt-3136*vx1(6)-1568*vx1(5)-1568*vx1(4)+392*vx1(2)+2128*vx2(6)+
     . 1064*vx2(5)+1064*vx2(4)-266*vx2(2)-896*vx3(6)-448*vx3(5)-448*
     . vx3(4)+112*vx3(2)+168*vx4(6)+84*vx4(5)+84*vx4(4)-21*vx4(2)
      ans4=-128*vx(5)*vx(1)*b2*dt-64*vx(5)*vx(1)*b3*dt+1536*vx(5)*vy(
     . 6)*c2*dt+512*vx(5)*vy(6)*c3*dt+768*vx(5)*vy(5)*c2*dt+512*vx(5)
     . *vy(5)*c3*dt+512*vx(5)*vy(4)*c2*dt+512*vx(5)*vy(4)*c3*dt+192*
     . vx(5)*vy(3)*c2*dt-64*vx(5)*vy(3)*c3*dt-192*vx(5)*vy(2)*c2*dt-
     . 64*vx(5)*vy(2)*c3*dt-128*vx(5)*vy(1)*c2*dt-64*vx(5)*vy(1)*c3*
     . dt+868*vx(5)+512*vx(4)**2*b1*dt+768*vx(4)**2*b2*dt-64*vx(4)*vx
     . (3)*b1*dt-128*vx(4)*vx(3)*b2*dt+64*vx(4)*vx(3)*b3*dt-64*vx(4)*
     . vx(2)*b1*dt-128*vx(4)*vx(2)*b2*dt+256*vx(4)*vx(1)*b1*dt+192*vx
     . (4)*vx(1)*b2*dt+512*vx(4)*vy(6)*c1*dt+1536*vx(4)*vy(6)*c2*dt+
     . 512*vx(4)*vy(5)*c1*dt+512*vx(4)*vy(5)*c2*dt+512*vx(4)*vy(4)*c1
     . *dt+768*vx(4)*vy(4)*c2*dt-64*vx(4)*vy(3)*c1*dt-128*vx(4)*vy(3)
     . *c2*dt-64*vx(4)*vy(2)*c1*dt-192*vx(4)*vy(2)*c2*dt-64*vx(4)*vy(
     . 1)*c1*dt+192*vx(4)*vy(1)*c2*dt+868*vx(4)+192*vx(3)**2*b3*dt-64
     . *vx(3)*vx(2)*b2*dt-80*vx(3)*vx(2)*b3*dt-128*vx(3)*vx(1)*b1*dt-
     . 128*vx(3)*vx(1)*b3*dt+640*vx(3)*vy(6)*c3*dt+320*vx(3)*vy(5)*c3
     . *dt+64*vx(3)*vy(4)*c3*dt+192*vx(3)*vy(3)*c3*dt-80*vx(3)*vy(2)*
     . c3*dt+ans5
      ans3=-3360*p(3)*b1*dt-1680*p(3)*b3*dt-1680*p(2)*b1*dt-1680*p(2)
     . *b3*dt-1680*p(1)*b1*dt-3360*p(1)*b3*dt-896*volx(6)*dt-448*volx
     . (5)*dt-448*volx(4)*dt+112*volx(2)*dt+1536*vx(6)**2*b1*dt+1536*
     . vx(6)**2*b3*dt+768*vx(6)*vx(5)*b1*dt+1536*vx(6)*vx(5)*b2*dt+
     . 1024*vx(6)*vx(5)*b3*dt+1024*vx(6)*vx(4)*b1*dt+1536*vx(6)*vx(4)
     . *b2*dt+768*vx(6)*vx(4)*b3*dt+192*vx(6)*vx(3)*b1*dt+512*vx(6)*
     . vx(3)*b3*dt-192*vx(6)*vx(2)*b1*dt-384*vx(6)*vx(2)*b2*dt-192*vx
     . (6)*vx(2)*b3*dt+512*vx(6)*vx(1)*b1*dt+192*vx(6)*vx(1)*b3*dt+
     . 1536*vx(6)*vy(6)*c1*dt+1536*vx(6)*vy(6)*c3*dt+768*vx(6)*vy(5)*
     . c1*dt+512*vx(6)*vy(5)*c3*dt+512*vx(6)*vy(4)*c1*dt+768*vx(6)*vy
     . (4)*c3*dt+192*vx(6)*vy(3)*c1*dt-128*vx(6)*vy(3)*c3*dt-192*vx(6
     . )*vy(2)*c1*dt-192*vx(6)*vy(2)*c3*dt-128*vx(6)*vy(1)*c1*dt+192*
     . vx(6)*vy(1)*c3*dt+1736*vx(6)+768*vx(5)**2*b2*dt+512*vx(5)**2*
     . b3*dt+512*vx(5)*vx(4)*b1*dt+1024*vx(5)*vx(4)*b2*dt+512*vx(5)*
     . vx(4)*b3*dt+192*vx(5)*vx(3)*b2*dt+256*vx(5)*vx(3)*b3*dt-128*vx
     . (5)*vx(2)*b2*dt-64*vx(5)*vx(2)*b3*dt+64*vx(5)*vx(1)*b1*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(2*vx(6)*b1**2*bv+2*vx(6)*b1**2+2*vx(6)*b1*b3*
     . bv+2*vx(6)*b1*b3+2*vx(6)*b3**2*bv+2*vx(6)*b3**2+2*vx(6)*c1**2+
     . 2*vx(6)*c1*c3+2*vx(6)*c3**2+2*vx(5)*b1*b2*bv+2*vx(5)*b1*b2+vx(
     . 5)*b1*b3*bv+vx(5)*b1*b3+vx(5)*b2*b3*bv+vx(5)*b2*b3+vx(5)*b3**2
     . *bv+vx(5)*b3**2+2*vx(5)*c1*c2+vx(5)*c1*c3+vx(5)*c2*c3+vx(5)*c3
     . **2+vx(4)*b1**2*bv+vx(4)*b1**2+vx(4)*b1*b2*bv+vx(4)*b1*b2+vx(4
     . )*b1*b3*bv+vx(4)*b1*b3+2*vx(4)*b2*b3*bv+2*vx(4)*b2*b3+vx(4)*c1
     . **2+vx(4)*c1*c2+vx(4)*c1*c3+2*vx(4)*c2*c3+vx(3)*b1*b3*bv+vx(3)
     . *b1*b3+vx(3)*c1*c3+vx(1)*b1*b3*bv+vx(1)*b1*b3+vx(1)*c1*c3+2*vy
     . (6)*b1*bv*c1+vy(6)*b1*bv*c3+vy(6)*b3*bv*c1+2*vy(6)*b3*bv*c3+2*
     . vy(5)*b1*bv*c2+vy(5)*b1*bv*c3+vy(5)*b3*bv*c2+vy(5)*b3*bv*c3+vy
     . (4)*b1*bv*c1+vy(4)*b1*bv*c2+vy(4)*b3*bv*c1+2*vy(4)*b3*bv*c2+vy
     . (3)*b1*bv*c3+vy(1)*b3*bv*c1)
      ans1=ans2+ans6
      rdl(14)=ans1/(5040*dt)
      ans5=768*vy(4)**2*c2*dt-64*vy(4)*vy(3)*c1*dt-128*vy(4)*vy(3)*c2
     . *dt+64*vy(4)*vy(3)*c3*dt-64*vy(4)*vy(2)*c1*dt-128*vy(4)*vy(2)*
     . c2*dt+256*vy(4)*vy(1)*c1*dt+192*vy(4)*vy(1)*c2*dt+868*vy(4)+
     . 192*vy(3)**2*c3*dt-64*vy(3)*vy(2)*c2*dt-80*vy(3)*vy(2)*c3*dt-
     . 128*vy(3)*vy(1)*c1*dt-128*vy(3)*vy(1)*c3*dt+48*vy(2)**2*c2*dt-
     . 80*vy(2)*vy(1)*c1*dt-64*vy(2)*vy(1)*c2*dt-217*vy(2)+192*vy(1)
     . **2*c1*dt-3136*vy1(6)-1568*vy1(5)-1568*vy1(4)+392*vy1(2)+2128*
     . vy2(6)+1064*vy2(5)+1064*vy2(4)-266*vy2(2)-896*vy3(6)-448*vy3(5
     . )-448*vy3(4)+112*vy3(2)+168*vy4(6)+84*vy4(5)+84*vy4(4)-21*vy4(
     . 2)
      ans4=-192*vx(2)*vy(5)*b2*dt-64*vx(2)*vy(5)*b3*dt-64*vx(2)*vy(4)
     . *b1*dt-192*vx(2)*vy(4)*b2*dt-80*vx(2)*vy(3)*b3*dt+48*vx(2)*vy(
     . 2)*b2*dt-80*vx(2)*vy(1)*b1*dt-128*vx(1)*vy(6)*b1*dt+192*vx(1)*
     . vy(6)*b3*dt-128*vx(1)*vy(5)*b2*dt-64*vx(1)*vy(5)*b3*dt-64*vx(1
     . )*vy(4)*b1*dt+192*vx(1)*vy(4)*b2*dt-128*vx(1)*vy(3)*b3*dt-64*
     . vx(1)*vy(2)*b2*dt+192*vx(1)*vy(1)*b1*dt+1536*vy(6)**2*c1*dt+
     . 1536*vy(6)**2*c3*dt+768*vy(6)*vy(5)*c1*dt+1536*vy(6)*vy(5)*c2*
     . dt+1024*vy(6)*vy(5)*c3*dt+1024*vy(6)*vy(4)*c1*dt+1536*vy(6)*vy
     . (4)*c2*dt+768*vy(6)*vy(4)*c3*dt+192*vy(6)*vy(3)*c1*dt+512*vy(6
     . )*vy(3)*c3*dt-192*vy(6)*vy(2)*c1*dt-384*vy(6)*vy(2)*c2*dt-192*
     . vy(6)*vy(2)*c3*dt+512*vy(6)*vy(1)*c1*dt+192*vy(6)*vy(1)*c3*dt+
     . 1736*vy(6)+768*vy(5)**2*c2*dt+512*vy(5)**2*c3*dt+512*vy(5)*vy(
     . 4)*c1*dt+1024*vy(5)*vy(4)*c2*dt+512*vy(5)*vy(4)*c3*dt+192*vy(5
     . )*vy(3)*c2*dt+256*vy(5)*vy(3)*c3*dt-128*vy(5)*vy(2)*c2*dt-64*
     . vy(5)*vy(2)*c3*dt+64*vy(5)*vy(1)*c1*dt-128*vy(5)*vy(1)*c2*dt-
     . 64*vy(5)*vy(1)*c3*dt+868*vy(5)+512*vy(4)**2*c1*dt+ans5
      ans3=-3360*p(3)*c1*dt-1680*p(3)*c3*dt-1680*p(2)*c1*dt-1680*p(2)
     . *c3*dt-1680*p(1)*c1*dt-3360*p(1)*c3*dt-896*voly(6)*dt-448*voly
     . (5)*dt-448*voly(4)*dt+112*voly(2)*dt+1536*vx(6)*vy(6)*b1*dt+
     . 1536*vx(6)*vy(6)*b3*dt+1536*vx(6)*vy(5)*b2*dt+512*vx(6)*vy(5)*
     . b3*dt+512*vx(6)*vy(4)*b1*dt+1536*vx(6)*vy(4)*b2*dt+640*vx(6)*
     . vy(3)*b3*dt-384*vx(6)*vy(2)*b2*dt+640*vx(6)*vy(1)*b1*dt+768*vx
     . (5)*vy(6)*b1*dt+512*vx(5)*vy(6)*b3*dt+768*vx(5)*vy(5)*b2*dt+
     . 512*vx(5)*vy(5)*b3*dt+512*vx(5)*vy(4)*b1*dt+512*vx(5)*vy(4)*b2
     . *dt+320*vx(5)*vy(3)*b3*dt+64*vx(5)*vy(2)*b2*dt+64*vx(5)*vy(1)*
     . b1*dt+512*vx(4)*vy(6)*b1*dt+768*vx(4)*vy(6)*b3*dt+512*vx(4)*vy
     . (5)*b2*dt+512*vx(4)*vy(5)*b3*dt+512*vx(4)*vy(4)*b1*dt+768*vx(4
     . )*vy(4)*b2*dt+64*vx(4)*vy(3)*b3*dt+64*vx(4)*vy(2)*b2*dt+320*vx
     . (4)*vy(1)*b1*dt+192*vx(3)*vy(6)*b1*dt-128*vx(3)*vy(6)*b3*dt+
     . 192*vx(3)*vy(5)*b2*dt-64*vx(3)*vy(5)*b3*dt-64*vx(3)*vy(4)*b1*
     . dt-128*vx(3)*vy(4)*b2*dt+192*vx(3)*vy(3)*b3*dt-64*vx(3)*vy(2)*
     . b2*dt-128*vx(3)*vy(1)*b1*dt-192*vx(2)*vy(6)*b1*dt-192*vx(2)*vy
     . (6)*b3*dt+ans4
      ans2=re*ae*ans3
      ans6=6720*ae*dt*(2*vx(6)*b1*bv*c1+vx(6)*b1*bv*c3+vx(6)*b3*bv*c1
     . +2*vx(6)*b3*bv*c3+2*vx(5)*b2*bv*c1+vx(5)*b2*bv*c3+vx(5)*b3*bv*
     . c1+vx(5)*b3*bv*c3+vx(4)*b1*bv*c1+vx(4)*b1*bv*c3+vx(4)*b2*bv*c1
     . +2*vx(4)*b2*bv*c3+vx(3)*b3*bv*c1+vx(1)*b1*bv*c3+2*vy(6)*b1**2+
     . 2*vy(6)*b1*b3+2*vy(6)*b3**2+2*vy(6)*bv*c1**2+2*vy(6)*bv*c1*c3+
     . 2*vy(6)*bv*c3**2+2*vy(6)*c1**2+2*vy(6)*c1*c3+2*vy(6)*c3**2+2*
     . vy(5)*b1*b2+vy(5)*b1*b3+vy(5)*b2*b3+vy(5)*b3**2+2*vy(5)*bv*c1*
     . c2+vy(5)*bv*c1*c3+vy(5)*bv*c2*c3+vy(5)*bv*c3**2+2*vy(5)*c1*c2+
     . vy(5)*c1*c3+vy(5)*c2*c3+vy(5)*c3**2+vy(4)*b1**2+vy(4)*b1*b2+vy
     . (4)*b1*b3+2*vy(4)*b2*b3+vy(4)*bv*c1**2+vy(4)*bv*c1*c2+vy(4)*bv
     . *c1*c3+2*vy(4)*bv*c2*c3+vy(4)*c1**2+vy(4)*c1*c2+vy(4)*c1*c3+2*
     . vy(4)*c2*c3+vy(3)*b1*b3+vy(3)*bv*c1*c3+vy(3)*c1*c3+vy(1)*b1*b3
     . +vy(1)*bv*c1*c3+vy(1)*c1*c3)
      ans1=ans2+ans6
      rdl(15)=ans1/(5040*dt)
      aaf(1,1)=(re*ae*(192*vx(6)*b1*dt+320*vx(6)*b3*dt+32*vx(5)*b1*dt
     . +64*vx(5)*b2*dt+64*vx(5)*b3*dt+192*vx(4)*b1*dt+320*vx(4)*b2*dt
     . -24*vx(3)*b1*dt-48*vx(3)*b3*dt-24*vx(2)*b1*dt-48*vx(2)*b2*dt+
     . 416*vx(1)*b1*dt+128*vy(6)*c1*dt+32*vy(5)*c1*dt+128*vy(4)*c1*dt
     . -24*vy(3)*c1*dt-24*vy(2)*c1*dt+208*vy(1)*c1*dt+217)+3360*ae*dt
     . *(b1**2*bv+b1**2+c1**2))/(3360*dt)
      aaf(1,2)=(re*ae*(4*vx(6)*c1+20*vx(6)*c3+4*vx(5)*c2+4*vx(5)*c3+4
     . *vx(4)*c1+20*vx(4)*c2-3*vx(3)*c3-3*vx(2)*c2+13*vx(1)*c1)+210*
     . ae*b1*bv*c1)/210
      aaf(1,3)=(-re*ae*b1)/3
      aaf(1,4)=(re*ae*(64*vx(6)*b1*dt-256*vx(6)*b2*dt-256*vx(6)*b3*dt
     . -256*vx(5)*b2*dt-256*vx(5)*b3*dt-256*vx(4)*b1*dt-768*vx(4)*b2*
     . dt+176*vx(3)*b2*dt+176*vx(3)*b3*dt-288*vx(2)*b2*dt-144*vx(1)*
     . b1*dt-288*vx(1)*b2*dt-256*vy(6)*c2*dt-320*vy(5)*c2*dt-512*vy(4
     . )*c2*dt+176*vy(3)*c2*dt-144*vy(2)*c2*dt-288*vy(1)*c2*dt-217)-
     . 6720*ae*dt*(b1*b2*bv+b1*b2+c1*c2))/(20160*dt)
      aaf(1,5)=(re*ae*(4*vx(6)*c1-16*vx(6)*c3+4*vx(5)*c2-16*vx(5)*c3-
     . 16*vx(4)*c1-16*vx(4)*c2+11*vx(3)*c3-9*vx(2)*c2-9*vx(1)*c1)-420
     . *ae*b1*bv*c2)/1260
      aaf(1,6)=0
      aaf(1,7)=(re*ae*(-256*vx(6)*b1*dt-768*vx(6)*b3*dt-256*vx(5)*b2*
     . dt-256*vx(5)*b3*dt+64*vx(4)*b1*dt-256*vx(4)*b2*dt-256*vx(4)*b3
     . *dt-288*vx(3)*b3*dt+176*vx(2)*b2*dt+176*vx(2)*b3*dt-144*vx(1)*
     . b1*dt-288*vx(1)*b3*dt-512*vy(6)*c3*dt-320*vy(5)*c3*dt-256*vy(4
     . )*c3*dt-144*vy(3)*c3*dt+176*vy(2)*c3*dt-288*vy(1)*c3*dt-217)-
     . 6720*ae*dt*(b1*b3*bv+b1*b3+c1*c3))/(20160*dt)
      aaf(1,8)=(re*ae*(-16*vx(6)*c1-16*vx(6)*c3-16*vx(5)*c2+4*vx(5)*
     . c3+4*vx(4)*c1-16*vx(4)*c2-9*vx(3)*c3+11*vx(2)*c2-9*vx(1)*c1)-
     . 420*ae*b1*bv*c3)/1260
      aaf(1,9)=0
      aaf(1,10)=(re*ae*(-8*vx(6)*b1+12*vx(6)*b2+12*vx(6)*b3-12*vx(5)*
     . b1-8*vx(5)*b2-8*vx(5)*b3-16*vx(4)*b1+24*vx(4)*b2+vx(3)*b1-4*vx
     . (3)*b2-4*vx(3)*b3-4*vx(2)*b1-12*vx(2)*b2+18*vx(1)*b1+30*vx(1)*
     . b2-4*vy(6)*c1+12*vy(6)*c2-12*vy(5)*c1-4*vy(5)*c2-8*vy(4)*c1+12
     . *vy(4)*c2+vy(3)*c1-4*vy(3)*c2-4*vy(2)*c1-4*vy(2)*c2+6*vy(1)*c1
     . +30*vy(1)*c2)+420*ae*(b1*b2*bv+b1*b2+c1*c2))/315
      aaf(1,11)=(4*re*ae*(-vx(6)*c1+3*vx(6)*c3-vx(5)*c2-2*vx(5)*c3-2*
     . vx(4)*c1+3*vx(4)*c2-vx(3)*c3-2*vx(2)*c2+3*vx(1)*c1)+420*ae*b1*
     . bv*c2)/315
      aaf(1,12)=(re*ae*(-192*vx(6)*b1*dt-128*vx(6)*b2*dt-128*vx(6)*b3
     . *dt-384*vx(5)*b2*dt-384*vx(5)*b3*dt-192*vx(4)*b1*dt-128*vx(4)*
     . b2*dt-128*vx(4)*b3*dt-64*vx(3)*b2*dt-64*vx(3)*b3*dt-64*vx(2)*
     . b2*dt-64*vx(2)*b3*dt+48*vx(1)*b1*dt+96*vx(1)*b2*dt+96*vx(1)*b3
     . *dt-128*vy(6)*c2*dt-64*vy(6)*c3*dt-192*vy(5)*c2*dt-192*vy(5)*
     . c3*dt-64*vy(4)*c2*dt-128*vy(4)*c3*dt-64*vy(3)*c2*dt+16*vy(3)*
     . c3*dt+16*vy(2)*c2*dt-64*vy(2)*c3*dt+96*vy(1)*c2*dt+96*vy(1)*c3
     . *dt-217))/(5040*dt)
      aaf(1,13)=(re*ae*(-12*vx(6)*c1-4*vx(6)*c3-12*vx(5)*c2-12*vx(5)*
     . c3-12*vx(4)*c1-4*vx(4)*c2-5*vx(3)*c3-5*vx(2)*c2+3*vx(1)*c1))/
     . 315
      aaf(1,14)=(re*ae*(-16*vx(6)*b1+24*vx(6)*b3-12*vx(5)*b1-8*vx(5)*
     . b2-8*vx(5)*b3-8*vx(4)*b1+12*vx(4)*b2+12*vx(4)*b3-4*vx(3)*b1-12
     . *vx(3)*b3+vx(2)*b1-4*vx(2)*b2-4*vx(2)*b3+18*vx(1)*b1+30*vx(1)*
     . b3-8*vy(6)*c1+12*vy(6)*c3-12*vy(5)*c1-4*vy(5)*c3-4*vy(4)*c1+12
     . *vy(4)*c3-4*vy(3)*c1-4*vy(3)*c3+vy(2)*c1-4*vy(2)*c3+6*vy(1)*c1
     . +30*vy(1)*c3)+420*ae*(b1*b3*bv+b1*b3+c1*c3))/315
      aaf(1,15)=(4*re*ae*(-2*vx(6)*c1+3*vx(6)*c3-2*vx(5)*c2-vx(5)*c3-
     . vx(4)*c1+3*vx(4)*c2-2*vx(3)*c3-vx(2)*c2+3*vx(1)*c1)+420*ae*b1*
     . bv*c3)/315
      aaf(2,1)=(re*ae*(4*vy(6)*b1+20*vy(6)*b3+4*vy(5)*b2+4*vy(5)*b3+4
     . *vy(4)*b1+20*vy(4)*b2-3*vy(3)*b3-3*vy(2)*b2+13*vy(1)*b1)+210*
     . ae*b1*bv*c1)/210
      aaf(2,2)=(re*ae*(128*vx(6)*b1*dt+32*vx(5)*b1*dt+128*vx(4)*b1*dt
     . -24*vx(3)*b1*dt-24*vx(2)*b1*dt+208*vx(1)*b1*dt+192*vy(6)*c1*dt
     . +320*vy(6)*c3*dt+32*vy(5)*c1*dt+64*vy(5)*c2*dt+64*vy(5)*c3*dt+
     . 192*vy(4)*c1*dt+320*vy(4)*c2*dt-24*vy(3)*c1*dt-48*vy(3)*c3*dt-
     . 24*vy(2)*c1*dt-48*vy(2)*c2*dt+416*vy(1)*c1*dt+217)+3360*ae*dt*
     . (b1**2+bv*c1**2+c1**2))/(3360*dt)
      aaf(2,3)=(-re*ae*c1)/3
      aaf(2,4)=(re*ae*(4*vy(6)*b1-16*vy(6)*b3+4*vy(5)*b2-16*vy(5)*b3-
     . 16*vy(4)*b1-16*vy(4)*b2+11*vy(3)*b3-9*vy(2)*b2-9*vy(1)*b1)-420
     . *ae*b2*bv*c1)/1260
      aaf(2,5)=(re*ae*(-256*vx(6)*b2*dt-320*vx(5)*b2*dt-512*vx(4)*b2*
     . dt+176*vx(3)*b2*dt-144*vx(2)*b2*dt-288*vx(1)*b2*dt+64*vy(6)*c1
     . *dt-256*vy(6)*c2*dt-256*vy(6)*c3*dt-256*vy(5)*c2*dt-256*vy(5)*
     . c3*dt-256*vy(4)*c1*dt-768*vy(4)*c2*dt+176*vy(3)*c2*dt+176*vy(3
     . )*c3*dt-288*vy(2)*c2*dt-144*vy(1)*c1*dt-288*vy(1)*c2*dt-217)-
     . 6720*ae*dt*(b1*b2+bv*c1*c2+c1*c2))/(20160*dt)
      aaf(2,6)=0
      aaf(2,7)=(re*ae*(-16*vy(6)*b1-16*vy(6)*b3-16*vy(5)*b2+4*vy(5)*
     . b3+4*vy(4)*b1-16*vy(4)*b2-9*vy(3)*b3+11*vy(2)*b2-9*vy(1)*b1)-
     . 420*ae*b3*bv*c1)/1260
      aaf(2,8)=(re*ae*(-512*vx(6)*b3*dt-320*vx(5)*b3*dt-256*vx(4)*b3*
     . dt-144*vx(3)*b3*dt+176*vx(2)*b3*dt-288*vx(1)*b3*dt-256*vy(6)*
     . c1*dt-768*vy(6)*c3*dt-256*vy(5)*c2*dt-256*vy(5)*c3*dt+64*vy(4)
     . *c1*dt-256*vy(4)*c2*dt-256*vy(4)*c3*dt-288*vy(3)*c3*dt+176*vy(
     . 2)*c2*dt+176*vy(2)*c3*dt-144*vy(1)*c1*dt-288*vy(1)*c3*dt-217)-
     . 6720*ae*dt*(b1*b3+bv*c1*c3+c1*c3))/(20160*dt)
      aaf(2,9)=0
      aaf(2,10)=(4*re*ae*(-vy(6)*b1+3*vy(6)*b3-vy(5)*b2-2*vy(5)*b3-2*
     . vy(4)*b1+3*vy(4)*b2-vy(3)*b3-2*vy(2)*b2+3*vy(1)*b1)+420*ae*b2*
     . bv*c1)/315
      aaf(2,11)=(re*ae*(-4*vx(6)*b1+12*vx(6)*b2-12*vx(5)*b1-4*vx(5)*
     . b2-8*vx(4)*b1+12*vx(4)*b2+vx(3)*b1-4*vx(3)*b2-4*vx(2)*b1-4*vx(
     . 2)*b2+6*vx(1)*b1+30*vx(1)*b2-8*vy(6)*c1+12*vy(6)*c2+12*vy(6)*
     . c3-12*vy(5)*c1-8*vy(5)*c2-8*vy(5)*c3-16*vy(4)*c1+24*vy(4)*c2+
     . vy(3)*c1-4*vy(3)*c2-4*vy(3)*c3-4*vy(2)*c1-12*vy(2)*c2+18*vy(1)
     . *c1+30*vy(1)*c2)+420*ae*(b1*b2+bv*c1*c2+c1*c2))/315
      aaf(2,12)=(re*ae*(-12*vy(6)*b1-4*vy(6)*b3-12*vy(5)*b2-12*vy(5)*
     . b3-12*vy(4)*b1-4*vy(4)*b2-5*vy(3)*b3-5*vy(2)*b2+3*vy(1)*b1))/
     . 315
      aaf(2,13)=(re*ae*(-128*vx(6)*b2*dt-64*vx(6)*b3*dt-192*vx(5)*b2*
     . dt-192*vx(5)*b3*dt-64*vx(4)*b2*dt-128*vx(4)*b3*dt-64*vx(3)*b2*
     . dt+16*vx(3)*b3*dt+16*vx(2)*b2*dt-64*vx(2)*b3*dt+96*vx(1)*b2*dt
     . +96*vx(1)*b3*dt-192*vy(6)*c1*dt-128*vy(6)*c2*dt-128*vy(6)*c3*
     . dt-384*vy(5)*c2*dt-384*vy(5)*c3*dt-192*vy(4)*c1*dt-128*vy(4)*
     . c2*dt-128*vy(4)*c3*dt-64*vy(3)*c2*dt-64*vy(3)*c3*dt-64*vy(2)*
     . c2*dt-64*vy(2)*c3*dt+48*vy(1)*c1*dt+96*vy(1)*c2*dt+96*vy(1)*c3
     . *dt-217))/(5040*dt)
      aaf(2,14)=(4*re*ae*(-2*vy(6)*b1+3*vy(6)*b3-2*vy(5)*b2-vy(5)*b3-
     . vy(4)*b1+3*vy(4)*b2-2*vy(3)*b3-vy(2)*b2+3*vy(1)*b1)+420*ae*b3*
     . bv*c1)/315
      aaf(2,15)=(re*ae*(-8*vx(6)*b1+12*vx(6)*b3-12*vx(5)*b1-4*vx(5)*
     . b3-4*vx(4)*b1+12*vx(4)*b3-4*vx(3)*b1-4*vx(3)*b3+vx(2)*b1-4*vx(
     . 2)*b3+6*vx(1)*b1+30*vx(1)*b3-16*vy(6)*c1+24*vy(6)*c3-12*vy(5)*
     . c1-8*vy(5)*c2-8*vy(5)*c3-8*vy(4)*c1+12*vy(4)*c2+12*vy(4)*c3-4*
     . vy(3)*c1-12*vy(3)*c3+vy(2)*c1-4*vy(2)*c2-4*vy(2)*c3+18*vy(1)*
     . c1+30*vy(1)*c3)+420*ae*(b1*b3+bv*c1*c3+c1*c3))/315
      aaf(3,1)=(re*ae*b1)/3
      aaf(3,2)=(re*ae*c1)/3
      aaf(3,3)=0
      aaf(3,4)=0
      aaf(3,5)=0
      aaf(3,6)=0
      aaf(3,7)=0
      aaf(3,8)=0
      aaf(3,9)=0
      aaf(3,10)=(re*ae*(b1+2*b2))/3
      aaf(3,11)=(re*ae*(c1+2*c2))/3
      aaf(3,12)=(re*ae*(b2+b3))/3
      aaf(3,13)=(re*ae*(c2+c3))/3
      aaf(3,14)=(re*ae*(b1+2*b3))/3
      aaf(3,15)=(re*ae*(c1+2*c3))/3
      aaf(4,1)=(re*ae*(-256*vx(6)*b1*dt-256*vx(6)*b3*dt-256*vx(5)*b1*
     . dt+64*vx(5)*b2*dt-256*vx(5)*b3*dt-768*vx(4)*b1*dt-256*vx(4)*b2
     . *dt+176*vx(3)*b1*dt+176*vx(3)*b3*dt-288*vx(2)*b1*dt-144*vx(2)*
     . b2*dt-288*vx(1)*b1*dt-320*vy(6)*c1*dt-256*vy(5)*c1*dt-512*vy(4
     . )*c1*dt+176*vy(3)*c1*dt-288*vy(2)*c1*dt-144*vy(1)*c1*dt-217)-
     . 6720*ae*dt*(b1*b2*bv+b1*b2+c1*c2))/(20160*dt)
      aaf(4,2)=(re*ae*(4*vx(6)*c1-16*vx(6)*c3+4*vx(5)*c2-16*vx(5)*c3-
     . 16*vx(4)*c1-16*vx(4)*c2+11*vx(3)*c3-9*vx(2)*c2-9*vx(1)*c1)-420
     . *ae*b2*bv*c1)/1260
      aaf(4,3)=0
      aaf(4,4)=(re*ae*(64*vx(6)*b1*dt+32*vx(6)*b2*dt+64*vx(6)*b3*dt+
     . 192*vx(5)*b2*dt+320*vx(5)*b3*dt+320*vx(4)*b1*dt+192*vx(4)*b2*
     . dt-24*vx(3)*b2*dt-48*vx(3)*b3*dt+416*vx(2)*b2*dt-48*vx(1)*b1*
     . dt-24*vx(1)*b2*dt+32*vy(6)*c2*dt+128*vy(5)*c2*dt+128*vy(4)*c2*
     . dt-24*vy(3)*c2*dt+208*vy(2)*c2*dt-24*vy(1)*c2*dt+217)+3360*ae*
     . dt*(b2**2*bv+b2**2+c2**2))/(3360*dt)
      aaf(4,5)=(re*ae*(4*vx(6)*c1+4*vx(6)*c3+4*vx(5)*c2+20*vx(5)*c3+
     . 20*vx(4)*c1+4*vx(4)*c2-3*vx(3)*c3+13*vx(2)*c2-3*vx(1)*c1)+210*
     . ae*b2*bv*c2)/210
      aaf(4,6)=(-re*ae*b2)/3
      aaf(4,7)=(re*ae*(-256*vx(6)*b1*dt-256*vx(6)*b3*dt-256*vx(5)*b2*
     . dt-768*vx(5)*b3*dt-256*vx(4)*b1*dt+64*vx(4)*b2*dt-256*vx(4)*b3
     . *dt-288*vx(3)*b3*dt-144*vx(2)*b2*dt-288*vx(2)*b3*dt+176*vx(1)*
     . b1*dt+176*vx(1)*b3*dt-320*vy(6)*c3*dt-512*vy(5)*c3*dt-256*vy(4
     . )*c3*dt-144*vy(3)*c3*dt-288*vy(2)*c3*dt+176*vy(1)*c3*dt-217)-
     . 6720*ae*dt*(b2*b3*bv+b2*b3+c2*c3))/(20160*dt)
      aaf(4,8)=(re*ae*(-16*vx(6)*c1+4*vx(6)*c3-16*vx(5)*c2-16*vx(5)*
     . c3-16*vx(4)*c1+4*vx(4)*c2-9*vx(3)*c3-9*vx(2)*c2+11*vx(1)*c1)-
     . 420*ae*b2*bv*c3)/1260
      aaf(4,9)=0
      aaf(4,10)=(re*ae*(-8*vx(6)*b1-12*vx(6)*b2-8*vx(6)*b3+12*vx(5)*
     . b1-8*vx(5)*b2+12*vx(5)*b3+24*vx(4)*b1-16*vx(4)*b2-4*vx(3)*b1+
     . vx(3)*b2-4*vx(3)*b3+30*vx(2)*b1+18*vx(2)*b2-12*vx(1)*b1-4*vx(1
     . )*b2-4*vy(6)*c1-12*vy(6)*c2+12*vy(5)*c1-4*vy(5)*c2+12*vy(4)*c1
     . -8*vy(4)*c2-4*vy(3)*c1+vy(3)*c2+30*vy(2)*c1+6*vy(2)*c2-4*vy(1)
     . *c1-4*vy(1)*c2)+420*ae*(b1*b2*bv+b1*b2+c1*c2))/315
      aaf(4,11)=(4*re*ae*(-vx(6)*c1-2*vx(6)*c3-vx(5)*c2+3*vx(5)*c3+3*
     . vx(4)*c1-2*vx(4)*c2-vx(3)*c3+3*vx(2)*c2-2*vx(1)*c1)+420*ae*b2*
     . bv*c1)/315
      aaf(4,12)=(re*ae*(-8*vx(6)*b1-12*vx(6)*b2-8*vx(6)*b3-16*vx(5)*
     . b2+24*vx(5)*b3+12*vx(4)*b1-8*vx(4)*b2+12*vx(4)*b3-4*vx(3)*b2-
     . 12*vx(3)*b3+18*vx(2)*b2+30*vx(2)*b3-4*vx(1)*b1+vx(1)*b2-4*vx(1
     . )*b3-12*vy(6)*c2-4*vy(6)*c3-8*vy(5)*c2+12*vy(5)*c3-4*vy(4)*c2+
     . 12*vy(4)*c3-4*vy(3)*c2-4*vy(3)*c3+6*vy(2)*c2+30*vy(2)*c3+vy(1)
     . *c2-4*vy(1)*c3)+420*ae*(b2*b3*bv+b2*b3+c2*c3))/315
      aaf(4,13)=(4*re*ae*(-2*vx(6)*c1-vx(6)*c3-2*vx(5)*c2+3*vx(5)*c3+
     . 3*vx(4)*c1-vx(4)*c2-2*vx(3)*c3+3*vx(2)*c2-vx(1)*c1)+420*ae*b2*
     . bv*c3)/315
      aaf(4,14)=(re*ae*(-384*vx(6)*b1*dt-384*vx(6)*b3*dt-128*vx(5)*b1
     . *dt-192*vx(5)*b2*dt-128*vx(5)*b3*dt-128*vx(4)*b1*dt-192*vx(4)*
     . b2*dt-128*vx(4)*b3*dt-64*vx(3)*b1*dt-64*vx(3)*b3*dt+96*vx(2)*
     . b1*dt+48*vx(2)*b2*dt+96*vx(2)*b3*dt-64*vx(1)*b1*dt-64*vx(1)*b3
     . *dt-192*vy(6)*c1*dt-192*vy(6)*c3*dt-128*vy(5)*c1*dt-64*vy(5)*
     . c3*dt-64*vy(4)*c1*dt-128*vy(4)*c3*dt-64*vy(3)*c1*dt+16*vy(3)*
     . c3*dt+96*vy(2)*c1*dt+96*vy(2)*c3*dt+16*vy(1)*c1*dt-64*vy(1)*c3
     . *dt-217))/(5040*dt)
      aaf(4,15)=(re*ae*(-12*vx(6)*c1-12*vx(6)*c3-12*vx(5)*c2-4*vx(5)*
     . c3-4*vx(4)*c1-12*vx(4)*c2-5*vx(3)*c3+3*vx(2)*c2-5*vx(1)*c1))/
     . 315
      aaf(5,1)=(re*ae*(4*vy(6)*b1-16*vy(6)*b3+4*vy(5)*b2-16*vy(5)*b3-
     . 16*vy(4)*b1-16*vy(4)*b2+11*vy(3)*b3-9*vy(2)*b2-9*vy(1)*b1)-420
     . *ae*b1*bv*c2)/1260
      aaf(5,2)=(re*ae*(-320*vx(6)*b1*dt-256*vx(5)*b1*dt-512*vx(4)*b1*
     . dt+176*vx(3)*b1*dt-288*vx(2)*b1*dt-144*vx(1)*b1*dt-256*vy(6)*
     . c1*dt-256*vy(6)*c3*dt-256*vy(5)*c1*dt+64*vy(5)*c2*dt-256*vy(5)
     . *c3*dt-768*vy(4)*c1*dt-256*vy(4)*c2*dt+176*vy(3)*c1*dt+176*vy(
     . 3)*c3*dt-288*vy(2)*c1*dt-144*vy(2)*c2*dt-288*vy(1)*c1*dt-217)-
     . 6720*ae*dt*(b1*b2+bv*c1*c2+c1*c2))/(20160*dt)
      aaf(5,3)=0
      aaf(5,4)=(re*ae*(4*vy(6)*b1+4*vy(6)*b3+4*vy(5)*b2+20*vy(5)*b3+
     . 20*vy(4)*b1+4*vy(4)*b2-3*vy(3)*b3+13*vy(2)*b2-3*vy(1)*b1)+210*
     . ae*b2*bv*c2)/210
      aaf(5,5)=(re*ae*(32*vx(6)*b2*dt+128*vx(5)*b2*dt+128*vx(4)*b2*dt
     . -24*vx(3)*b2*dt+208*vx(2)*b2*dt-24*vx(1)*b2*dt+64*vy(6)*c1*dt+
     . 32*vy(6)*c2*dt+64*vy(6)*c3*dt+192*vy(5)*c2*dt+320*vy(5)*c3*dt+
     . 320*vy(4)*c1*dt+192*vy(4)*c2*dt-24*vy(3)*c2*dt-48*vy(3)*c3*dt+
     . 416*vy(2)*c2*dt-48*vy(1)*c1*dt-24*vy(1)*c2*dt+217)+3360*ae*dt*
     . (b2**2+bv*c2**2+c2**2))/(3360*dt)
      aaf(5,6)=(-re*ae*c2)/3
      aaf(5,7)=(re*ae*(-16*vy(6)*b1+4*vy(6)*b3-16*vy(5)*b2-16*vy(5)*
     . b3-16*vy(4)*b1+4*vy(4)*b2-9*vy(3)*b3-9*vy(2)*b2+11*vy(1)*b1)-
     . 420*ae*b3*bv*c2)/1260
      aaf(5,8)=(re*ae*(-320*vx(6)*b3*dt-512*vx(5)*b3*dt-256*vx(4)*b3*
     . dt-144*vx(3)*b3*dt-288*vx(2)*b3*dt+176*vx(1)*b3*dt-256*vy(6)*
     . c1*dt-256*vy(6)*c3*dt-256*vy(5)*c2*dt-768*vy(5)*c3*dt-256*vy(4
     . )*c1*dt+64*vy(4)*c2*dt-256*vy(4)*c3*dt-288*vy(3)*c3*dt-144*vy(
     . 2)*c2*dt-288*vy(2)*c3*dt+176*vy(1)*c1*dt+176*vy(1)*c3*dt-217)-
     . 6720*ae*dt*(b2*b3+bv*c2*c3+c2*c3))/(20160*dt)
      aaf(5,9)=0
      aaf(5,10)=(4*re*ae*(-vy(6)*b1-2*vy(6)*b3-vy(5)*b2+3*vy(5)*b3+3*
     . vy(4)*b1-2*vy(4)*b2-vy(3)*b3+3*vy(2)*b2-2*vy(1)*b1)+420*ae*b1*
     . bv*c2)/315
      aaf(5,11)=(re*ae*(-4*vx(6)*b1-12*vx(6)*b2+12*vx(5)*b1-4*vx(5)*
     . b2+12*vx(4)*b1-8*vx(4)*b2-4*vx(3)*b1+vx(3)*b2+30*vx(2)*b1+6*vx
     . (2)*b2-4*vx(1)*b1-4*vx(1)*b2-8*vy(6)*c1-12*vy(6)*c2-8*vy(6)*c3
     . +12*vy(5)*c1-8*vy(5)*c2+12*vy(5)*c3+24*vy(4)*c1-16*vy(4)*c2-4*
     . vy(3)*c1+vy(3)*c2-4*vy(3)*c3+30*vy(2)*c1+18*vy(2)*c2-12*vy(1)*
     . c1-4*vy(1)*c2)+420*ae*(b1*b2+bv*c1*c2+c1*c2))/315
      aaf(5,12)=(4*re*ae*(-2*vy(6)*b1-vy(6)*b3-2*vy(5)*b2+3*vy(5)*b3+
     . 3*vy(4)*b1-vy(4)*b2-2*vy(3)*b3+3*vy(2)*b2-vy(1)*b1)+420*ae*b3*
     . bv*c2)/315
      aaf(5,13)=(re*ae*(-12*vx(6)*b2-4*vx(6)*b3-8*vx(5)*b2+12*vx(5)*
     . b3-4*vx(4)*b2+12*vx(4)*b3-4*vx(3)*b2-4*vx(3)*b3+6*vx(2)*b2+30*
     . vx(2)*b3+vx(1)*b2-4*vx(1)*b3-8*vy(6)*c1-12*vy(6)*c2-8*vy(6)*c3
     . -16*vy(5)*c2+24*vy(5)*c3+12*vy(4)*c1-8*vy(4)*c2+12*vy(4)*c3-4*
     . vy(3)*c2-12*vy(3)*c3+18*vy(2)*c2+30*vy(2)*c3-4*vy(1)*c1+vy(1)*
     . c2-4*vy(1)*c3)+420*ae*(b2*b3+bv*c2*c3+c2*c3))/315
      aaf(5,14)=(re*ae*(-12*vy(6)*b1-12*vy(6)*b3-12*vy(5)*b2-4*vy(5)*
     . b3-4*vy(4)*b1-12*vy(4)*b2-5*vy(3)*b3+3*vy(2)*b2-5*vy(1)*b1))/
     . 315
      aaf(5,15)=(re*ae*(-192*vx(6)*b1*dt-192*vx(6)*b3*dt-128*vx(5)*b1
     . *dt-64*vx(5)*b3*dt-64*vx(4)*b1*dt-128*vx(4)*b3*dt-64*vx(3)*b1*
     . dt+16*vx(3)*b3*dt+96*vx(2)*b1*dt+96*vx(2)*b3*dt+16*vx(1)*b1*dt
     . -64*vx(1)*b3*dt-384*vy(6)*c1*dt-384*vy(6)*c3*dt-128*vy(5)*c1*
     . dt-192*vy(5)*c2*dt-128*vy(5)*c3*dt-128*vy(4)*c1*dt-192*vy(4)*
     . c2*dt-128*vy(4)*c3*dt-64*vy(3)*c1*dt-64*vy(3)*c3*dt+96*vy(2)*
     . c1*dt+48*vy(2)*c2*dt+96*vy(2)*c3*dt-64*vy(1)*c1*dt-64*vy(1)*c3
     . *dt-217))/(5040*dt)
      aaf(6,1)=0
      aaf(6,2)=0
      aaf(6,3)=0
      aaf(6,4)=(re*ae*b2)/3
      aaf(6,5)=(re*ae*c2)/3
      aaf(6,6)=0
      aaf(6,7)=0
      aaf(6,8)=0
      aaf(6,9)=0
      aaf(6,10)=(re*ae*(2*b1+b2))/3
      aaf(6,11)=(re*ae*(2*c1+c2))/3
      aaf(6,12)=(re*ae*(b2+2*b3))/3
      aaf(6,13)=(re*ae*(c2+2*c3))/3
      aaf(6,14)=(re*ae*(b1+b3))/3
      aaf(6,15)=(re*ae*(c1+c3))/3
      aaf(7,1)=(re*ae*(-768*vx(6)*b1*dt-256*vx(6)*b3*dt-256*vx(5)*b1*
     . dt-256*vx(5)*b2*dt+64*vx(5)*b3*dt-256*vx(4)*b1*dt-256*vx(4)*b2
     . *dt-288*vx(3)*b1*dt-144*vx(3)*b3*dt+176*vx(2)*b1*dt+176*vx(2)*
     . b2*dt-288*vx(1)*b1*dt-512*vy(6)*c1*dt-256*vy(5)*c1*dt-320*vy(4
     . )*c1*dt-288*vy(3)*c1*dt+176*vy(2)*c1*dt-144*vy(1)*c1*dt-217)-
     . 6720*ae*dt*(b1*b3*bv+b1*b3+c1*c3))/(20160*dt)
      aaf(7,2)=(re*ae*(-16*vx(6)*c1-16*vx(6)*c3-16*vx(5)*c2+4*vx(5)*
     . c3+4*vx(4)*c1-16*vx(4)*c2-9*vx(3)*c3+11*vx(2)*c2-9*vx(1)*c1)-
     . 420*ae*b3*bv*c1)/1260
      aaf(7,3)=0
      aaf(7,4)=(re*ae*(-256*vx(6)*b1*dt-256*vx(6)*b2*dt+64*vx(6)*b3*
     . dt-768*vx(5)*b2*dt-256*vx(5)*b3*dt-256*vx(4)*b1*dt-256*vx(4)*
     . b2*dt-288*vx(3)*b2*dt-144*vx(3)*b3*dt-288*vx(2)*b2*dt+176*vx(1
     . )*b1*dt+176*vx(1)*b2*dt-256*vy(6)*c2*dt-512*vy(5)*c2*dt-320*vy
     . (4)*c2*dt-288*vy(3)*c2*dt-144*vy(2)*c2*dt+176*vy(1)*c2*dt-217)
     . -6720*ae*dt*(b2*b3*bv+b2*b3+c2*c3))/(20160*dt)
      aaf(7,5)=(re*ae*(-16*vx(6)*c1+4*vx(6)*c3-16*vx(5)*c2-16*vx(5)*
     . c3-16*vx(4)*c1+4*vx(4)*c2-9*vx(3)*c3-9*vx(2)*c2+11*vx(1)*c1)-
     . 420*ae*b3*bv*c2)/1260
      aaf(7,6)=0
      aaf(7,7)=(re*ae*(320*vx(6)*b1*dt+192*vx(6)*b3*dt+320*vx(5)*b2*
     . dt+192*vx(5)*b3*dt+64*vx(4)*b1*dt+64*vx(4)*b2*dt+32*vx(4)*b3*
     . dt+416*vx(3)*b3*dt-48*vx(2)*b2*dt-24*vx(2)*b3*dt-48*vx(1)*b1*
     . dt-24*vx(1)*b3*dt+128*vy(6)*c3*dt+128*vy(5)*c3*dt+32*vy(4)*c3*
     . dt+208*vy(3)*c3*dt-24*vy(2)*c3*dt-24*vy(1)*c3*dt+217)+3360*ae*
     . dt*(b3**2*bv+b3**2+c3**2))/(3360*dt)
      aaf(7,8)=(re*ae*(20*vx(6)*c1+4*vx(6)*c3+20*vx(5)*c2+4*vx(5)*c3+
     . 4*vx(4)*c1+4*vx(4)*c2+13*vx(3)*c3-3*vx(2)*c2-3*vx(1)*c1)+210*
     . ae*b3*bv*c3)/210
      aaf(7,9)=(-re*ae*b3)/3
      aaf(7,10)=(re*ae*(-128*vx(6)*b1*dt-128*vx(6)*b2*dt-192*vx(6)*b3
     . *dt-128*vx(5)*b1*dt-128*vx(5)*b2*dt-192*vx(5)*b3*dt-384*vx(4)*
     . b1*dt-384*vx(4)*b2*dt+96*vx(3)*b1*dt+96*vx(3)*b2*dt+48*vx(3)*
     . b3*dt-64*vx(2)*b1*dt-64*vx(2)*b2*dt-64*vx(1)*b1*dt-64*vx(1)*b2
     . *dt-64*vy(6)*c1*dt-128*vy(6)*c2*dt-128*vy(5)*c1*dt-64*vy(5)*c2
     . *dt-192*vy(4)*c1*dt-192*vy(4)*c2*dt+96*vy(3)*c1*dt+96*vy(3)*c2
     . *dt-64*vy(2)*c1*dt+16*vy(2)*c2*dt+16*vy(1)*c1*dt-64*vy(1)*c2*
     . dt-217))/(5040*dt)
      aaf(7,11)=(re*ae*(-4*vx(6)*c1-12*vx(6)*c3-4*vx(5)*c2-12*vx(5)*
     . c3-12*vx(4)*c1-12*vx(4)*c2+3*vx(3)*c3-5*vx(2)*c2-5*vx(1)*c1))/
     . 315
      aaf(7,12)=(re*ae*(12*vx(6)*b1+12*vx(6)*b2-8*vx(6)*b3+24*vx(5)*
     . b2-16*vx(5)*b3-8*vx(4)*b1-8*vx(4)*b2-12*vx(4)*b3+30*vx(3)*b2+
     . 18*vx(3)*b3-12*vx(2)*b2-4*vx(2)*b3-4*vx(1)*b1-4*vx(1)*b2+vx(1)
     . *b3+12*vy(6)*c2-4*vy(6)*c3+12*vy(5)*c2-8*vy(5)*c3-4*vy(4)*c2-
     . 12*vy(4)*c3+30*vy(3)*c2+6*vy(3)*c3-4*vy(2)*c2-4*vy(2)*c3-4*vy(
     . 1)*c2+vy(1)*c3)+420*ae*(b2*b3*bv+b2*b3+c2*c3))/315
      aaf(7,13)=(4*re*ae*(3*vx(6)*c1-vx(6)*c3+3*vx(5)*c2-2*vx(5)*c3-2
     . *vx(4)*c1-vx(4)*c2+3*vx(3)*c3-2*vx(2)*c2-vx(1)*c1)+420*ae*b3*
     . bv*c2)/315
      aaf(7,14)=(re*ae*(24*vx(6)*b1-16*vx(6)*b3+12*vx(5)*b1+12*vx(5)*
     . b2-8*vx(5)*b3-8*vx(4)*b1-8*vx(4)*b2-12*vx(4)*b3+30*vx(3)*b1+18
     . *vx(3)*b3-4*vx(2)*b1-4*vx(2)*b2+vx(2)*b3-12*vx(1)*b1-4*vx(1)*
     . b3+12*vy(6)*c1-8*vy(6)*c3+12*vy(5)*c1-4*vy(5)*c3-4*vy(4)*c1-12
     . *vy(4)*c3+30*vy(3)*c1+6*vy(3)*c3-4*vy(2)*c1+vy(2)*c3-4*vy(1)*
     . c1-4*vy(1)*c3)+420*ae*(b1*b3*bv+b1*b3+c1*c3))/315
      aaf(7,15)=(4*re*ae*(3*vx(6)*c1-2*vx(6)*c3+3*vx(5)*c2-vx(5)*c3-
     . vx(4)*c1-2*vx(4)*c2+3*vx(3)*c3-vx(2)*c2-2*vx(1)*c1)+420*ae*b3*
     . bv*c1)/315
      aaf(8,1)=(re*ae*(-16*vy(6)*b1-16*vy(6)*b3-16*vy(5)*b2+4*vy(5)*
     . b3+4*vy(4)*b1-16*vy(4)*b2-9*vy(3)*b3+11*vy(2)*b2-9*vy(1)*b1)-
     . 420*ae*b1*bv*c3)/1260
      aaf(8,2)=(re*ae*(-512*vx(6)*b1*dt-256*vx(5)*b1*dt-320*vx(4)*b1*
     . dt-288*vx(3)*b1*dt+176*vx(2)*b1*dt-144*vx(1)*b1*dt-768*vy(6)*
     . c1*dt-256*vy(6)*c3*dt-256*vy(5)*c1*dt-256*vy(5)*c2*dt+64*vy(5)
     . *c3*dt-256*vy(4)*c1*dt-256*vy(4)*c2*dt-288*vy(3)*c1*dt-144*vy(
     . 3)*c3*dt+176*vy(2)*c1*dt+176*vy(2)*c2*dt-288*vy(1)*c1*dt-217)-
     . 6720*ae*dt*(b1*b3+bv*c1*c3+c1*c3))/(20160*dt)
      aaf(8,3)=0
      aaf(8,4)=(re*ae*(-16*vy(6)*b1+4*vy(6)*b3-16*vy(5)*b2-16*vy(5)*
     . b3-16*vy(4)*b1+4*vy(4)*b2-9*vy(3)*b3-9*vy(2)*b2+11*vy(1)*b1)-
     . 420*ae*b2*bv*c3)/1260
      aaf(8,5)=(re*ae*(-256*vx(6)*b2*dt-512*vx(5)*b2*dt-320*vx(4)*b2*
     . dt-288*vx(3)*b2*dt-144*vx(2)*b2*dt+176*vx(1)*b2*dt-256*vy(6)*
     . c1*dt-256*vy(6)*c2*dt+64*vy(6)*c3*dt-768*vy(5)*c2*dt-256*vy(5)
     . *c3*dt-256*vy(4)*c1*dt-256*vy(4)*c2*dt-288*vy(3)*c2*dt-144*vy(
     . 3)*c3*dt-288*vy(2)*c2*dt+176*vy(1)*c1*dt+176*vy(1)*c2*dt-217)-
     . 6720*ae*dt*(b2*b3+bv*c2*c3+c2*c3))/(20160*dt)
      aaf(8,6)=0
      aaf(8,7)=(re*ae*(20*vy(6)*b1+4*vy(6)*b3+20*vy(5)*b2+4*vy(5)*b3+
     . 4*vy(4)*b1+4*vy(4)*b2+13*vy(3)*b3-3*vy(2)*b2-3*vy(1)*b1)+210*
     . ae*b3*bv*c3)/210
      aaf(8,8)=(re*ae*(128*vx(6)*b3*dt+128*vx(5)*b3*dt+32*vx(4)*b3*dt
     . +208*vx(3)*b3*dt-24*vx(2)*b3*dt-24*vx(1)*b3*dt+320*vy(6)*c1*dt
     . +192*vy(6)*c3*dt+320*vy(5)*c2*dt+192*vy(5)*c3*dt+64*vy(4)*c1*
     . dt+64*vy(4)*c2*dt+32*vy(4)*c3*dt+416*vy(3)*c3*dt-48*vy(2)*c2*
     . dt-24*vy(2)*c3*dt-48*vy(1)*c1*dt-24*vy(1)*c3*dt+217)+3360*ae*
     . dt*(b3**2+bv*c3**2+c3**2))/(3360*dt)
      aaf(8,9)=(-re*ae*c3)/3
      aaf(8,10)=(re*ae*(-4*vy(6)*b1-12*vy(6)*b3-4*vy(5)*b2-12*vy(5)*
     . b3-12*vy(4)*b1-12*vy(4)*b2+3*vy(3)*b3-5*vy(2)*b2-5*vy(1)*b1))/
     . 315
      aaf(8,11)=(re*ae*(-64*vx(6)*b1*dt-128*vx(6)*b2*dt-128*vx(5)*b1*
     . dt-64*vx(5)*b2*dt-192*vx(4)*b1*dt-192*vx(4)*b2*dt+96*vx(3)*b1*
     . dt+96*vx(3)*b2*dt-64*vx(2)*b1*dt+16*vx(2)*b2*dt+16*vx(1)*b1*dt
     . -64*vx(1)*b2*dt-128*vy(6)*c1*dt-128*vy(6)*c2*dt-192*vy(6)*c3*
     . dt-128*vy(5)*c1*dt-128*vy(5)*c2*dt-192*vy(5)*c3*dt-384*vy(4)*
     . c1*dt-384*vy(4)*c2*dt+96*vy(3)*c1*dt+96*vy(3)*c2*dt+48*vy(3)*
     . c3*dt-64*vy(2)*c1*dt-64*vy(2)*c2*dt-64*vy(1)*c1*dt-64*vy(1)*c2
     . *dt-217))/(5040*dt)
      aaf(8,12)=(4*re*ae*(3*vy(6)*b1-vy(6)*b3+3*vy(5)*b2-2*vy(5)*b3-2
     . *vy(4)*b1-vy(4)*b2+3*vy(3)*b3-2*vy(2)*b2-vy(1)*b1)+420*ae*b2*
     . bv*c3)/315
      aaf(8,13)=(re*ae*(12*vx(6)*b2-4*vx(6)*b3+12*vx(5)*b2-8*vx(5)*b3
     . -4*vx(4)*b2-12*vx(4)*b3+30*vx(3)*b2+6*vx(3)*b3-4*vx(2)*b2-4*vx
     . (2)*b3-4*vx(1)*b2+vx(1)*b3+12*vy(6)*c1+12*vy(6)*c2-8*vy(6)*c3+
     . 24*vy(5)*c2-16*vy(5)*c3-8*vy(4)*c1-8*vy(4)*c2-12*vy(4)*c3+30*
     . vy(3)*c2+18*vy(3)*c3-12*vy(2)*c2-4*vy(2)*c3-4*vy(1)*c1-4*vy(1)
     . *c2+vy(1)*c3)+420*ae*(b2*b3+bv*c2*c3+c2*c3))/315
      aaf(8,14)=(4*re*ae*(3*vy(6)*b1-2*vy(6)*b3+3*vy(5)*b2-vy(5)*b3-
     . vy(4)*b1-2*vy(4)*b2+3*vy(3)*b3-vy(2)*b2-2*vy(1)*b1)+420*ae*b1*
     . bv*c3)/315
      aaf(8,15)=(re*ae*(12*vx(6)*b1-8*vx(6)*b3+12*vx(5)*b1-4*vx(5)*b3
     . -4*vx(4)*b1-12*vx(4)*b3+30*vx(3)*b1+6*vx(3)*b3-4*vx(2)*b1+vx(2
     . )*b3-4*vx(1)*b1-4*vx(1)*b3+24*vy(6)*c1-16*vy(6)*c3+12*vy(5)*c1
     . +12*vy(5)*c2-8*vy(5)*c3-8*vy(4)*c1-8*vy(4)*c2-12*vy(4)*c3+30*
     . vy(3)*c1+18*vy(3)*c3-4*vy(2)*c1-4*vy(2)*c2+vy(2)*c3-12*vy(1)*
     . c1-4*vy(1)*c3)+420*ae*(b1*b3+bv*c1*c3+c1*c3))/315
      aaf(9,1)=0
      aaf(9,2)=0
      aaf(9,3)=0
      aaf(9,4)=0
      aaf(9,5)=0
      aaf(9,6)=0
      aaf(9,7)=(re*ae*b3)/3
      aaf(9,8)=(re*ae*c3)/3
      aaf(9,9)=0
      aaf(9,10)=(re*ae*(b1+b2))/3
      aaf(9,11)=(re*ae*(c1+c2))/3
      aaf(9,12)=(re*ae*(2*b2+b3))/3
      aaf(9,13)=(re*ae*(2*c2+c3))/3
      aaf(9,14)=(re*ae*(2*b1+b3))/3
      aaf(9,15)=(re*ae*(2*c1+c3))/3
      aaf(10,1)=(re*ae*(16*vx(6)*b1+12*vx(6)*b3+4*vx(5)*b1-4*vx(5)*b2
     . -8*vx(5)*b3+32*vx(4)*b1+12*vx(4)*b2-5*vx(3)*b1-4*vx(3)*b3-8*vx
     . (2)*b1-8*vx(2)*b2+24*vx(1)*b1+20*vy(6)*c1+4*vy(5)*c1+40*vy(4)*
     . c1-5*vy(3)*c1-8*vy(2)*c1+12*vy(1)*c1)+420*ae*(b1*b2*bv+b1*b2+
     . c1*c2))/315
      aaf(10,2)=(4*re*ae*(-vx(6)*c1+3*vx(6)*c3-vx(5)*c2-2*vx(5)*c3-2*
     . vx(4)*c1+3*vx(4)*c2-vx(3)*c3-2*vx(2)*c2+3*vx(1)*c1)+420*ae*b2*
     . bv*c1)/315
      aaf(10,3)=(re*ae*(-b1-2*b2))/3
      aaf(10,4)=(re*ae*(-4*vx(6)*b1+4*vx(6)*b2-8*vx(6)*b3+16*vx(5)*b2
     . +12*vx(5)*b3+12*vx(4)*b1+32*vx(4)*b2-5*vx(3)*b2-4*vx(3)*b3+24*
     . vx(2)*b2-8*vx(1)*b1-8*vx(1)*b2+4*vy(6)*c2+20*vy(5)*c2+40*vy(4)
     . *c2-5*vy(3)*c2+12*vy(2)*c2-8*vy(1)*c2)+420*ae*(b1*b2*bv+b1*b2+
     . c1*c2))/315
      aaf(10,5)=(4*re*ae*(-vx(6)*c1-2*vx(6)*c3-vx(5)*c2+3*vx(5)*c3+3*
     . vx(4)*c1-2*vx(4)*c2-vx(3)*c3+3*vx(2)*c2-2*vx(1)*c1)+420*ae*b1*
     . bv*c2)/315
      aaf(10,6)=(re*ae*(-2*b1-b2))/3
      aaf(10,7)=(re*ae*(-64*vx(6)*b1*dt-128*vx(6)*b3*dt-64*vx(5)*b2*
     . dt-128*vx(5)*b3*dt-192*vx(4)*b1*dt-192*vx(4)*b2*dt-384*vx(4)*
     . b3*dt+96*vx(3)*b3*dt-80*vx(2)*b2*dt-64*vx(2)*b3*dt-80*vx(1)*b1
     . *dt-64*vx(1)*b3*dt+64*vy(6)*c3*dt+64*vy(5)*c3*dt-384*vy(4)*c3*
     . dt+48*vy(3)*c3*dt-64*vy(2)*c3*dt-64*vy(1)*c3*dt-217))/(5040*dt
     . )
      aaf(10,8)=(re*ae*(-4*vx(6)*c1-12*vx(6)*c3-4*vx(5)*c2-12*vx(5)*
     . c3-12*vx(4)*c1-12*vx(4)*c2+3*vx(3)*c3-5*vx(2)*c2-5*vx(1)*c1))/
     . 315
      aaf(10,9)=(-re*ae*(b1+b2))/3
      aaf(10,10)=(re*ae*(128*vx(6)*b1*dt+96*vx(6)*b2*dt+192*vx(6)*b3*
     . dt+96*vx(5)*b1*dt+128*vx(5)*b2*dt+192*vx(5)*b3*dt+384*vx(4)*b1
     . *dt+384*vx(4)*b2*dt-24*vx(3)*b1*dt-24*vx(3)*b2*dt-48*vx(3)*b3*
     . dt+24*vx(2)*b1*dt+64*vx(2)*b2*dt+64*vx(1)*b1*dt+24*vx(1)*b2*dt
     . +64*vy(6)*c1*dt+96*vy(6)*c2*dt+96*vy(5)*c1*dt+64*vy(5)*c2*dt+
     . 192*vy(4)*c1*dt+192*vy(4)*c2*dt-24*vy(3)*c1*dt-24*vy(3)*c2*dt+
     . 24*vy(2)*c1*dt-16*vy(2)*c2*dt-16*vy(1)*c1*dt+24*vy(1)*c2*dt+
     . 217)+1680*ae*dt*(b1**2*bv+b1**2+b1*b2*bv+b1*b2+b2**2*bv+b2**2+
     . c1**2+c1*c2+c2**2))/(630*dt)
      aaf(10,11)=(8*re*ae*(4*vx(6)*c1+12*vx(6)*c3+4*vx(5)*c2+12*vx(5)
     . *c3+12*vx(4)*c1+12*vx(4)*c2-3*vx(3)*c3+5*vx(2)*c2+5*vx(1)*c1)+
     . 420*ae*bv*(2*b1*c1+b1*c2+b2*c1+2*b2*c2))/315
      aaf(10,12)=(re*ae*(128*vx(6)*b1*dt+128*vx(6)*b2*dt+256*vx(6)*b3
     . *dt+256*vx(5)*b2*dt+384*vx(5)*b3*dt+192*vx(4)*b1*dt+256*vx(4)*
     . b2*dt+384*vx(4)*b3*dt-16*vx(3)*b2*dt-32*vx(3)*b3*dt+64*vx(2)*
     . b2*dt+48*vx(2)*b3*dt+16*vx(1)*b1*dt-16*vx(1)*b2*dt-32*vx(1)*b3
     . *dt+128*vy(6)*c2*dt+128*vy(6)*c3*dt+128*vy(5)*c2*dt+192*vy(5)*
     . c3*dt+128*vy(4)*c2*dt+384*vy(4)*c3*dt-16*vy(3)*c2*dt-48*vy(3)*
     . c3*dt-16*vy(2)*c2*dt+48*vy(2)*c3*dt-16*vy(1)*c2*dt-32*vy(1)*c3
     . *dt+217)+1680*ae*dt*(b1*b2*bv+b1*b2+2*b1*b3*bv+2*b1*b3+b2**2*
     . bv+b2**2+b2*b3*bv+b2*b3+c1*c2+2*c1*c3+c2**2+c2*c3))/(1260*dt)
      aaf(10,13)=(4*re*ae*(8*vx(6)*c1+8*vx(6)*c3+8*vx(5)*c2+12*vx(5)*
     . c3+12*vx(4)*c1+8*vx(4)*c2+vx(3)*c3+5*vx(2)*c2+vx(1)*c1)+420*ae
     . *bv*(b1*c2+2*b1*c3+b2*c2+b2*c3))/315
      aaf(10,14)=(re*ae*(256*vx(6)*b1*dt+384*vx(6)*b3*dt+128*vx(5)*b1
     . *dt+128*vx(5)*b2*dt+256*vx(5)*b3*dt+256*vx(4)*b1*dt+192*vx(4)*
     . b2*dt+384*vx(4)*b3*dt-16*vx(3)*b1*dt-32*vx(3)*b3*dt-16*vx(2)*
     . b1*dt+16*vx(2)*b2*dt-32*vx(2)*b3*dt+64*vx(1)*b1*dt+48*vx(1)*b3
     . *dt+128*vy(6)*c1*dt+192*vy(6)*c3*dt+128*vy(5)*c1*dt+128*vy(5)*
     . c3*dt+128*vy(4)*c1*dt+384*vy(4)*c3*dt-16*vy(3)*c1*dt-48*vy(3)*
     . c3*dt-16*vy(2)*c1*dt-32*vy(2)*c3*dt-16*vy(1)*c1*dt+48*vy(1)*c3
     . *dt+217)+1680*ae*dt*(b1**2*bv+b1**2+b1*b2*bv+b1*b2+b1*b3*bv+b1
     . *b3+2*b2*b3*bv+2*b2*b3+c1**2+c1*c2+c1*c3+2*c2*c3))/(1260*dt)
      aaf(10,15)=(4*re*ae*(8*vx(6)*c1+12*vx(6)*c3+8*vx(5)*c2+8*vx(5)*
     . c3+8*vx(4)*c1+12*vx(4)*c2+vx(3)*c3+vx(2)*c2+5*vx(1)*c1)+420*ae
     . *bv*(b1*c1+b1*c3+b2*c1+2*b2*c3))/315
      aaf(11,1)=(4*re*ae*(-vy(6)*b1+3*vy(6)*b3-vy(5)*b2-2*vy(5)*b3-2*
     . vy(4)*b1+3*vy(4)*b2-vy(3)*b3-2*vy(2)*b2+3*vy(1)*b1)+420*ae*b1*
     . bv*c2)/315
      aaf(11,2)=(re*ae*(20*vx(6)*b1+4*vx(5)*b1+40*vx(4)*b1-5*vx(3)*b1
     . -8*vx(2)*b1+12*vx(1)*b1+16*vy(6)*c1+12*vy(6)*c3+4*vy(5)*c1-4*
     . vy(5)*c2-8*vy(5)*c3+32*vy(4)*c1+12*vy(4)*c2-5*vy(3)*c1-4*vy(3)
     . *c3-8*vy(2)*c1-8*vy(2)*c2+24*vy(1)*c1)+420*ae*(b1*b2+bv*c1*c2+
     . c1*c2))/315
      aaf(11,3)=(re*ae*(-c1-2*c2))/3
      aaf(11,4)=(4*re*ae*(-vy(6)*b1-2*vy(6)*b3-vy(5)*b2+3*vy(5)*b3+3*
     . vy(4)*b1-2*vy(4)*b2-vy(3)*b3+3*vy(2)*b2-2*vy(1)*b1)+420*ae*b2*
     . bv*c1)/315
      aaf(11,5)=(re*ae*(4*vx(6)*b2+20*vx(5)*b2+40*vx(4)*b2-5*vx(3)*b2
     . +12*vx(2)*b2-8*vx(1)*b2-4*vy(6)*c1+4*vy(6)*c2-8*vy(6)*c3+16*vy
     . (5)*c2+12*vy(5)*c3+12*vy(4)*c1+32*vy(4)*c2-5*vy(3)*c2-4*vy(3)*
     . c3+24*vy(2)*c2-8*vy(1)*c1-8*vy(1)*c2)+420*ae*(b1*b2+bv*c1*c2+
     . c1*c2))/315
      aaf(11,6)=(re*ae*(-2*c1-c2))/3
      aaf(11,7)=(re*ae*(-4*vy(6)*b1-12*vy(6)*b3-4*vy(5)*b2-12*vy(5)*
     . b3-12*vy(4)*b1-12*vy(4)*b2+3*vy(3)*b3-5*vy(2)*b2-5*vy(1)*b1))/
     . 315
      aaf(11,8)=(re*ae*(64*vx(6)*b3*dt+64*vx(5)*b3*dt-384*vx(4)*b3*dt
     . +48*vx(3)*b3*dt-64*vx(2)*b3*dt-64*vx(1)*b3*dt-64*vy(6)*c1*dt-
     . 128*vy(6)*c3*dt-64*vy(5)*c2*dt-128*vy(5)*c3*dt-192*vy(4)*c1*dt
     . -192*vy(4)*c2*dt-384*vy(4)*c3*dt+96*vy(3)*c3*dt-80*vy(2)*c2*dt
     . -64*vy(2)*c3*dt-80*vy(1)*c1*dt-64*vy(1)*c3*dt-217))/(5040*dt)
      aaf(11,9)=(-re*ae*(c1+c2))/3
      aaf(11,10)=(8*re*ae*(4*vy(6)*b1+12*vy(6)*b3+4*vy(5)*b2+12*vy(5)
     . *b3+12*vy(4)*b1+12*vy(4)*b2-3*vy(3)*b3+5*vy(2)*b2+5*vy(1)*b1)+
     . 420*ae*bv*(2*b1*c1+b1*c2+b2*c1+2*b2*c2))/315
      aaf(11,11)=(re*ae*(64*vx(6)*b1*dt+96*vx(6)*b2*dt+96*vx(5)*b1*dt
     . +64*vx(5)*b2*dt+192*vx(4)*b1*dt+192*vx(4)*b2*dt-24*vx(3)*b1*dt
     . -24*vx(3)*b2*dt+24*vx(2)*b1*dt-16*vx(2)*b2*dt-16*vx(1)*b1*dt+
     . 24*vx(1)*b2*dt+128*vy(6)*c1*dt+96*vy(6)*c2*dt+192*vy(6)*c3*dt+
     . 96*vy(5)*c1*dt+128*vy(5)*c2*dt+192*vy(5)*c3*dt+384*vy(4)*c1*dt
     . +384*vy(4)*c2*dt-24*vy(3)*c1*dt-24*vy(3)*c2*dt-48*vy(3)*c3*dt+
     . 24*vy(2)*c1*dt+64*vy(2)*c2*dt+64*vy(1)*c1*dt+24*vy(1)*c2*dt+
     . 217)+1680*ae*dt*(b1**2+b1*b2+b2**2+bv*c1**2+bv*c1*c2+bv*c2**2+
     . c1**2+c1*c2+c2**2))/(630*dt)
      aaf(11,12)=(4*re*ae*(8*vy(6)*b1+8*vy(6)*b3+8*vy(5)*b2+12*vy(5)*
     . b3+12*vy(4)*b1+8*vy(4)*b2+vy(3)*b3+5*vy(2)*b2+vy(1)*b1)+420*ae
     . *bv*(b2*c1+b2*c2+2*b3*c1+b3*c2))/315
      aaf(11,13)=(re*ae*(128*vx(6)*b2*dt+128*vx(6)*b3*dt+128*vx(5)*b2
     . *dt+192*vx(5)*b3*dt+128*vx(4)*b2*dt+384*vx(4)*b3*dt-16*vx(3)*
     . b2*dt-48*vx(3)*b3*dt-16*vx(2)*b2*dt+48*vx(2)*b3*dt-16*vx(1)*b2
     . *dt-32*vx(1)*b3*dt+128*vy(6)*c1*dt+128*vy(6)*c2*dt+256*vy(6)*
     . c3*dt+256*vy(5)*c2*dt+384*vy(5)*c3*dt+192*vy(4)*c1*dt+256*vy(4
     . )*c2*dt+384*vy(4)*c3*dt-16*vy(3)*c2*dt-32*vy(3)*c3*dt+64*vy(2)
     . *c2*dt+48*vy(2)*c3*dt+16*vy(1)*c1*dt-16*vy(1)*c2*dt-32*vy(1)*
     . c3*dt+217)+1680*ae*dt*(b1*b2+2*b1*b3+b2**2+b2*b3+bv*c1*c2+2*bv
     . *c1*c3+bv*c2**2+bv*c2*c3+c1*c2+2*c1*c3+c2**2+c2*c3))/(1260*dt)
      aaf(11,14)=(4*re*ae*(8*vy(6)*b1+12*vy(6)*b3+8*vy(5)*b2+8*vy(5)*
     . b3+8*vy(4)*b1+12*vy(4)*b2+vy(3)*b3+vy(2)*b2+5*vy(1)*b1)+420*ae
     . *bv*(b1*c1+b1*c2+b3*c1+2*b3*c2))/315
      aaf(11,15)=(re*ae*(128*vx(6)*b1*dt+192*vx(6)*b3*dt+128*vx(5)*b1
     . *dt+128*vx(5)*b3*dt+128*vx(4)*b1*dt+384*vx(4)*b3*dt-16*vx(3)*
     . b1*dt-48*vx(3)*b3*dt-16*vx(2)*b1*dt-32*vx(2)*b3*dt-16*vx(1)*b1
     . *dt+48*vx(1)*b3*dt+256*vy(6)*c1*dt+384*vy(6)*c3*dt+128*vy(5)*
     . c1*dt+128*vy(5)*c2*dt+256*vy(5)*c3*dt+256*vy(4)*c1*dt+192*vy(4
     . )*c2*dt+384*vy(4)*c3*dt-16*vy(3)*c1*dt-32*vy(3)*c3*dt-16*vy(2)
     . *c1*dt+16*vy(2)*c2*dt-32*vy(2)*c3*dt+64*vy(1)*c1*dt+48*vy(1)*
     . c3*dt+217)+1680*ae*dt*(b1**2+b1*b2+b1*b3+2*b2*b3+bv*c1**2+bv*
     . c1*c2+bv*c1*c3+2*bv*c2*c3+c1**2+c1*c2+c1*c3+2*c2*c3))/(1260*dt
     . )
      aaf(12,1)=(re*ae*(-128*vx(6)*b1*dt-64*vx(6)*b3*dt-384*vx(5)*b1*
     . dt-192*vx(5)*b2*dt-192*vx(5)*b3*dt-128*vx(4)*b1*dt-64*vx(4)*b2
     . *dt-64*vx(3)*b1*dt-80*vx(3)*b3*dt-64*vx(2)*b1*dt-80*vx(2)*b2*
     . dt+96*vx(1)*b1*dt+64*vy(6)*c1*dt-384*vy(5)*c1*dt+64*vy(4)*c1*
     . dt-64*vy(3)*c1*dt-64*vy(2)*c1*dt+48*vy(1)*c1*dt-217))/(5040*dt
     . )
      aaf(12,2)=(re*ae*(-12*vx(6)*c1-4*vx(6)*c3-12*vx(5)*c2-12*vx(5)*
     . c3-12*vx(4)*c1-4*vx(4)*c2-5*vx(3)*c3-5*vx(2)*c2+3*vx(1)*c1))/
     . 315
      aaf(12,3)=(-re*ae*(b2+b3))/3
      aaf(12,4)=(re*ae*(-8*vx(6)*b1+4*vx(6)*b2-4*vx(6)*b3+32*vx(5)*b2
     . +12*vx(5)*b3+12*vx(4)*b1+16*vx(4)*b2-8*vx(3)*b2-8*vx(3)*b3+24*
     . vx(2)*b2-4*vx(1)*b1-5*vx(1)*b2+4*vy(6)*c2+40*vy(5)*c2+20*vy(4)
     . *c2-8*vy(3)*c2+12*vy(2)*c2-5*vy(1)*c2)+420*ae*(b2*b3*bv+b2*b3+
     . c2*c3))/315
      aaf(12,5)=(4*re*ae*(-2*vx(6)*c1-vx(6)*c3-2*vx(5)*c2+3*vx(5)*c3+
     . 3*vx(4)*c1-vx(4)*c2-2*vx(3)*c3+3*vx(2)*c2-vx(1)*c1)+420*ae*b3*
     . bv*c2)/315
      aaf(12,6)=(re*ae*(-b2-2*b3))/3
      aaf(12,7)=(re*ae*(12*vx(6)*b1+16*vx(6)*b3+12*vx(5)*b2+32*vx(5)*
     . b3-8*vx(4)*b1-4*vx(4)*b2+4*vx(4)*b3+24*vx(3)*b3-8*vx(2)*b2-8*
     . vx(2)*b3-4*vx(1)*b1-5*vx(1)*b3+20*vy(6)*c3+40*vy(5)*c3+4*vy(4)
     . *c3+12*vy(3)*c3-8*vy(2)*c3-5*vy(1)*c3)+420*ae*(b2*b3*bv+b2*b3+
     . c2*c3))/315
      aaf(12,8)=(4*re*ae*(3*vx(6)*c1-vx(6)*c3+3*vx(5)*c2-2*vx(5)*c3-2
     . *vx(4)*c1-vx(4)*c2+3*vx(3)*c3-2*vx(2)*c2-vx(1)*c1)+420*ae*b2*
     . bv*c3)/315
      aaf(12,9)=(re*ae*(-2*b2-b3))/3
      aaf(12,10)=(re*ae*(256*vx(6)*b1*dt+128*vx(6)*b2*dt+128*vx(6)*b3
     . *dt+384*vx(5)*b1*dt+256*vx(5)*b2*dt+192*vx(5)*b3*dt+384*vx(4)*
     . b1*dt+256*vx(4)*b2*dt-32*vx(3)*b1*dt-16*vx(3)*b2*dt+16*vx(3)*
     . b3*dt+48*vx(2)*b1*dt+64*vx(2)*b2*dt-32*vx(1)*b1*dt-16*vx(1)*b2
     . *dt+128*vy(6)*c1*dt+128*vy(6)*c2*dt+384*vy(5)*c1*dt+128*vy(5)*
     . c2*dt+192*vy(4)*c1*dt+128*vy(4)*c2*dt-32*vy(3)*c1*dt-16*vy(3)*
     . c2*dt+48*vy(2)*c1*dt-16*vy(2)*c2*dt-48*vy(1)*c1*dt-16*vy(1)*c2
     . *dt+217)+1680*ae*dt*(b1*b2*bv+b1*b2+2*b1*b3*bv+2*b1*b3+b2**2*
     . bv+b2**2+b2*b3*bv+b2*b3+c1*c2+2*c1*c3+c2**2+c2*c3))/(1260*dt)
      aaf(12,11)=(4*re*ae*(8*vx(6)*c1+8*vx(6)*c3+8*vx(5)*c2+12*vx(5)*
     . c3+12*vx(4)*c1+8*vx(4)*c2+vx(3)*c3+5*vx(2)*c2+vx(1)*c1)+420*ae
     . *bv*(b2*c1+b2*c2+2*b3*c1+b3*c2))/315
      aaf(12,12)=(re*ae*(192*vx(6)*b1*dt+96*vx(6)*b2*dt+128*vx(6)*b3*
     . dt+384*vx(5)*b2*dt+384*vx(5)*b3*dt+192*vx(4)*b1*dt+128*vx(4)*
     . b2*dt+96*vx(4)*b3*dt+24*vx(3)*b2*dt+64*vx(3)*b3*dt+64*vx(2)*b2
     . *dt+24*vx(2)*b3*dt-48*vx(1)*b1*dt-24*vx(1)*b2*dt-24*vx(1)*b3*
     . dt+96*vy(6)*c2*dt+64*vy(6)*c3*dt+192*vy(5)*c2*dt+192*vy(5)*c3*
     . dt+64*vy(4)*c2*dt+96*vy(4)*c3*dt+24*vy(3)*c2*dt-16*vy(3)*c3*dt
     . -16*vy(2)*c2*dt+24*vy(2)*c3*dt-24*vy(1)*c2*dt-24*vy(1)*c3*dt+
     . 217)+1680*ae*dt*(b2**2*bv+b2**2+b2*b3*bv+b2*b3+b3**2*bv+b3**2+
     . c2**2+c2*c3+c3**2))/(630*dt)
      aaf(12,13)=(8*re*ae*(12*vx(6)*c1+4*vx(6)*c3+12*vx(5)*c2+12*vx(5
     . )*c3+12*vx(4)*c1+4*vx(4)*c2+5*vx(3)*c3+5*vx(2)*c2-3*vx(1)*c1)+
     . 420*ae*bv*(2*b2*c2+b2*c3+b3*c2+2*b3*c3))/315
      aaf(12,14)=(re*ae*(384*vx(6)*b1*dt+256*vx(6)*b3*dt+384*vx(5)*b1
     . *dt+192*vx(5)*b2*dt+256*vx(5)*b3*dt+256*vx(4)*b1*dt+128*vx(4)*
     . b2*dt+128*vx(4)*b3*dt+48*vx(3)*b1*dt+64*vx(3)*b3*dt-32*vx(2)*
     . b1*dt+16*vx(2)*b2*dt-16*vx(2)*b3*dt-32*vx(1)*b1*dt-16*vx(1)*b3
     . *dt+192*vy(6)*c1*dt+128*vy(6)*c3*dt+384*vy(5)*c1*dt+128*vy(5)*
     . c3*dt+128*vy(4)*c1*dt+128*vy(4)*c3*dt+48*vy(3)*c1*dt-16*vy(3)*
     . c3*dt-32*vy(2)*c1*dt-16*vy(2)*c3*dt-48*vy(1)*c1*dt-16*vy(1)*c3
     . *dt+217)+1680*ae*dt*(2*b1*b2*bv+2*b1*b2+b1*b3*bv+b1*b3+b2*b3*
     . bv+b2*b3+b3**2*bv+b3**2+2*c1*c2+c1*c3+c2*c3+c3**2))/(1260*dt)
      aaf(12,15)=(4*re*ae*(12*vx(6)*c1+8*vx(6)*c3+12*vx(5)*c2+8*vx(5)
     . *c3+8*vx(4)*c1+8*vx(4)*c2+5*vx(3)*c3+vx(2)*c2+vx(1)*c1)+420*ae
     . *bv*(2*b2*c1+b2*c3+b3*c1+b3*c3))/315
      aaf(13,1)=(re*ae*(-12*vy(6)*b1-4*vy(6)*b3-12*vy(5)*b2-12*vy(5)*
     . b3-12*vy(4)*b1-4*vy(4)*b2-5*vy(3)*b3-5*vy(2)*b2+3*vy(1)*b1))/
     . 315
      aaf(13,2)=(re*ae*(64*vx(6)*b1*dt-384*vx(5)*b1*dt+64*vx(4)*b1*dt
     . -64*vx(3)*b1*dt-64*vx(2)*b1*dt+48*vx(1)*b1*dt-128*vy(6)*c1*dt-
     . 64*vy(6)*c3*dt-384*vy(5)*c1*dt-192*vy(5)*c2*dt-192*vy(5)*c3*dt
     . -128*vy(4)*c1*dt-64*vy(4)*c2*dt-64*vy(3)*c1*dt-80*vy(3)*c3*dt-
     . 64*vy(2)*c1*dt-80*vy(2)*c2*dt+96*vy(1)*c1*dt-217))/(5040*dt)
      aaf(13,3)=(-re*ae*(c2+c3))/3
      aaf(13,4)=(4*re*ae*(-2*vy(6)*b1-vy(6)*b3-2*vy(5)*b2+3*vy(5)*b3+
     . 3*vy(4)*b1-vy(4)*b2-2*vy(3)*b3+3*vy(2)*b2-vy(1)*b1)+420*ae*b2*
     . bv*c3)/315
      aaf(13,5)=(re*ae*(4*vx(6)*b2+40*vx(5)*b2+20*vx(4)*b2-8*vx(3)*b2
     . +12*vx(2)*b2-5*vx(1)*b2-8*vy(6)*c1+4*vy(6)*c2-4*vy(6)*c3+32*vy
     . (5)*c2+12*vy(5)*c3+12*vy(4)*c1+16*vy(4)*c2-8*vy(3)*c2-8*vy(3)*
     . c3+24*vy(2)*c2-4*vy(1)*c1-5*vy(1)*c2)+420*ae*(b2*b3+bv*c2*c3+
     . c2*c3))/315
      aaf(13,6)=(re*ae*(-c2-2*c3))/3
      aaf(13,7)=(4*re*ae*(3*vy(6)*b1-vy(6)*b3+3*vy(5)*b2-2*vy(5)*b3-2
     . *vy(4)*b1-vy(4)*b2+3*vy(3)*b3-2*vy(2)*b2-vy(1)*b1)+420*ae*b3*
     . bv*c2)/315
      aaf(13,8)=(re*ae*(20*vx(6)*b3+40*vx(5)*b3+4*vx(4)*b3+12*vx(3)*
     . b3-8*vx(2)*b3-5*vx(1)*b3+12*vy(6)*c1+16*vy(6)*c3+12*vy(5)*c2+
     . 32*vy(5)*c3-8*vy(4)*c1-4*vy(4)*c2+4*vy(4)*c3+24*vy(3)*c3-8*vy(
     . 2)*c2-8*vy(2)*c3-4*vy(1)*c1-5*vy(1)*c3)+420*ae*(b2*b3+bv*c2*c3
     . +c2*c3))/315
      aaf(13,9)=(re*ae*(-2*c2-c3))/3
      aaf(13,10)=(4*re*ae*(8*vy(6)*b1+8*vy(6)*b3+8*vy(5)*b2+12*vy(5)*
     . b3+12*vy(4)*b1+8*vy(4)*b2+vy(3)*b3+5*vy(2)*b2+vy(1)*b1)+420*ae
     . *bv*(b1*c2+2*b1*c3+b2*c2+b2*c3))/315
      aaf(13,11)=(re*ae*(128*vx(6)*b1*dt+128*vx(6)*b2*dt+384*vx(5)*b1
     . *dt+128*vx(5)*b2*dt+192*vx(4)*b1*dt+128*vx(4)*b2*dt-32*vx(3)*
     . b1*dt-16*vx(3)*b2*dt+48*vx(2)*b1*dt-16*vx(2)*b2*dt-48*vx(1)*b1
     . *dt-16*vx(1)*b2*dt+256*vy(6)*c1*dt+128*vy(6)*c2*dt+128*vy(6)*
     . c3*dt+384*vy(5)*c1*dt+256*vy(5)*c2*dt+192*vy(5)*c3*dt+384*vy(4
     . )*c1*dt+256*vy(4)*c2*dt-32*vy(3)*c1*dt-16*vy(3)*c2*dt+16*vy(3)
     . *c3*dt+48*vy(2)*c1*dt+64*vy(2)*c2*dt-32*vy(1)*c1*dt-16*vy(1)*
     . c2*dt+217)+1680*ae*dt*(b1*b2+2*b1*b3+b2**2+b2*b3+bv*c1*c2+2*bv
     . *c1*c3+bv*c2**2+bv*c2*c3+c1*c2+2*c1*c3+c2**2+c2*c3))/(1260*dt)
      aaf(13,12)=(8*re*ae*(12*vy(6)*b1+4*vy(6)*b3+12*vy(5)*b2+12*vy(5
     . )*b3+12*vy(4)*b1+4*vy(4)*b2+5*vy(3)*b3+5*vy(2)*b2-3*vy(1)*b1)+
     . 420*ae*bv*(2*b2*c2+b2*c3+b3*c2+2*b3*c3))/315
      aaf(13,13)=(re*ae*(96*vx(6)*b2*dt+64*vx(6)*b3*dt+192*vx(5)*b2*
     . dt+192*vx(5)*b3*dt+64*vx(4)*b2*dt+96*vx(4)*b3*dt+24*vx(3)*b2*
     . dt-16*vx(3)*b3*dt-16*vx(2)*b2*dt+24*vx(2)*b3*dt-24*vx(1)*b2*dt
     . -24*vx(1)*b3*dt+192*vy(6)*c1*dt+96*vy(6)*c2*dt+128*vy(6)*c3*dt
     . +384*vy(5)*c2*dt+384*vy(5)*c3*dt+192*vy(4)*c1*dt+128*vy(4)*c2*
     . dt+96*vy(4)*c3*dt+24*vy(3)*c2*dt+64*vy(3)*c3*dt+64*vy(2)*c2*dt
     . +24*vy(2)*c3*dt-48*vy(1)*c1*dt-24*vy(1)*c2*dt-24*vy(1)*c3*dt+
     . 217)+1680*ae*dt*(b2**2+b2*b3+b3**2+bv*c2**2+bv*c2*c3+bv*c3**2+
     . c2**2+c2*c3+c3**2))/(630*dt)
      aaf(13,14)=(4*re*ae*(12*vy(6)*b1+8*vy(6)*b3+12*vy(5)*b2+8*vy(5)
     . *b3+8*vy(4)*b1+8*vy(4)*b2+5*vy(3)*b3+vy(2)*b2+vy(1)*b1)+420*ae
     . *bv*(2*b1*c2+b1*c3+b3*c2+b3*c3))/315
      aaf(13,15)=(re*ae*(192*vx(6)*b1*dt+128*vx(6)*b3*dt+384*vx(5)*b1
     . *dt+128*vx(5)*b3*dt+128*vx(4)*b1*dt+128*vx(4)*b3*dt+48*vx(3)*
     . b1*dt-16*vx(3)*b3*dt-32*vx(2)*b1*dt-16*vx(2)*b3*dt-48*vx(1)*b1
     . *dt-16*vx(1)*b3*dt+384*vy(6)*c1*dt+256*vy(6)*c3*dt+384*vy(5)*
     . c1*dt+192*vy(5)*c2*dt+256*vy(5)*c3*dt+256*vy(4)*c1*dt+128*vy(4
     . )*c2*dt+128*vy(4)*c3*dt+48*vy(3)*c1*dt+64*vy(3)*c3*dt-32*vy(2)
     . *c1*dt+16*vy(2)*c2*dt-16*vy(2)*c3*dt-32*vy(1)*c1*dt-16*vy(1)*
     . c3*dt+217)+1680*ae*dt*(2*b1*b2+b1*b3+b2*b3+b3**2+2*bv*c1*c2+bv
     . *c1*c3+bv*c2*c3+bv*c3**2+2*c1*c2+c1*c3+c2*c3+c3**2))/(1260*dt)
      aaf(14,1)=(re*ae*(32*vx(6)*b1+12*vx(6)*b3+4*vx(5)*b1-8*vx(5)*b2
     . -4*vx(5)*b3+16*vx(4)*b1+12*vx(4)*b2-8*vx(3)*b1-8*vx(3)*b3-5*vx
     . (2)*b1-4*vx(2)*b2+24*vx(1)*b1+40*vy(6)*c1+4*vy(5)*c1+20*vy(4)*
     . c1-8*vy(3)*c1-5*vy(2)*c1+12*vy(1)*c1)+420*ae*(b1*b3*bv+b1*b3+
     . c1*c3))/315
      aaf(14,2)=(4*re*ae*(-2*vx(6)*c1+3*vx(6)*c3-2*vx(5)*c2-vx(5)*c3-
     . vx(4)*c1+3*vx(4)*c2-2*vx(3)*c3-vx(2)*c2+3*vx(1)*c1)+420*ae*b3*
     . bv*c1)/315
      aaf(14,3)=(re*ae*(-b1-2*b3))/3
      aaf(14,4)=(re*ae*(-192*vx(6)*b1*dt-384*vx(6)*b2*dt-192*vx(6)*b3
     . *dt-128*vx(5)*b2*dt-64*vx(5)*b3*dt-64*vx(4)*b1*dt-128*vx(4)*b2
     . *dt-64*vx(3)*b2*dt-80*vx(3)*b3*dt+96*vx(2)*b2*dt-80*vx(1)*b1*
     . dt-64*vx(1)*b2*dt-384*vy(6)*c2*dt+64*vy(5)*c2*dt+64*vy(4)*c2*
     . dt-64*vy(3)*c2*dt+48*vy(2)*c2*dt-64*vy(1)*c2*dt-217))/(5040*dt
     . )
      aaf(14,5)=(re*ae*(-12*vx(6)*c1-12*vx(6)*c3-12*vx(5)*c2-4*vx(5)*
     . c3-4*vx(4)*c1-12*vx(4)*c2-5*vx(3)*c3+3*vx(2)*c2-5*vx(1)*c1))/
     . 315
      aaf(14,6)=(-re*ae*(b1+b3))/3
      aaf(14,7)=(re*ae*(12*vx(6)*b1+32*vx(6)*b3+12*vx(5)*b2+16*vx(5)*
     . b3-4*vx(4)*b1-8*vx(4)*b2+4*vx(4)*b3+24*vx(3)*b3-4*vx(2)*b2-5*
     . vx(2)*b3-8*vx(1)*b1-8*vx(1)*b3+40*vy(6)*c3+20*vy(5)*c3+4*vy(4)
     . *c3+12*vy(3)*c3-5*vy(2)*c3-8*vy(1)*c3)+420*ae*(b1*b3*bv+b1*b3+
     . c1*c3))/315
      aaf(14,8)=(4*re*ae*(3*vx(6)*c1-2*vx(6)*c3+3*vx(5)*c2-vx(5)*c3-
     . vx(4)*c1-2*vx(4)*c2+3*vx(3)*c3-vx(2)*c2-2*vx(1)*c1)+420*ae*b1*
     . bv*c3)/315
      aaf(14,9)=(re*ae*(-2*b1-b3))/3
      aaf(14,10)=(re*ae*(256*vx(6)*b1*dt+384*vx(6)*b2*dt+192*vx(6)*b3
     . *dt+128*vx(5)*b1*dt+256*vx(5)*b2*dt+128*vx(5)*b3*dt+256*vx(4)*
     . b1*dt+384*vx(4)*b2*dt-16*vx(3)*b1*dt-32*vx(3)*b2*dt+16*vx(3)*
     . b3*dt-16*vx(2)*b1*dt-32*vx(2)*b2*dt+64*vx(1)*b1*dt+48*vx(1)*b2
     . *dt+128*vy(6)*c1*dt+384*vy(6)*c2*dt+128*vy(5)*c1*dt+128*vy(5)*
     . c2*dt+128*vy(4)*c1*dt+192*vy(4)*c2*dt-16*vy(3)*c1*dt-32*vy(3)*
     . c2*dt-16*vy(2)*c1*dt-48*vy(2)*c2*dt-16*vy(1)*c1*dt+48*vy(1)*c2
     . *dt+217)+1680*ae*dt*(b1**2*bv+b1**2+b1*b2*bv+b1*b2+b1*b3*bv+b1
     . *b3+2*b2*b3*bv+2*b2*b3+c1**2+c1*c2+c1*c3+2*c2*c3))/(1260*dt)
      aaf(14,11)=(4*re*ae*(8*vx(6)*c1+12*vx(6)*c3+8*vx(5)*c2+8*vx(5)*
     . c3+8*vx(4)*c1+12*vx(4)*c2+vx(3)*c3+vx(2)*c2+5*vx(1)*c1)+420*ae
     . *bv*(b1*c1+b1*c2+b3*c1+2*b3*c2))/315
      aaf(14,12)=(re*ae*(192*vx(6)*b1*dt+384*vx(6)*b2*dt+256*vx(6)*b3
     . *dt+384*vx(5)*b2*dt+256*vx(5)*b3*dt+128*vx(4)*b1*dt+256*vx(4)*
     . b2*dt+128*vx(4)*b3*dt+48*vx(3)*b2*dt+64*vx(3)*b3*dt-32*vx(2)*
     . b2*dt-16*vx(2)*b3*dt+16*vx(1)*b1*dt-32*vx(1)*b2*dt-16*vx(1)*b3
     . *dt+384*vy(6)*c2*dt+128*vy(6)*c3*dt+192*vy(5)*c2*dt+128*vy(5)*
     . c3*dt+128*vy(4)*c2*dt+128*vy(4)*c3*dt+48*vy(3)*c2*dt-16*vy(3)*
     . c3*dt-48*vy(2)*c2*dt-16*vy(2)*c3*dt-32*vy(1)*c2*dt-16*vy(1)*c3
     . *dt+217)+1680*ae*dt*(2*b1*b2*bv+2*b1*b2+b1*b3*bv+b1*b3+b2*b3*
     . bv+b2*b3+b3**2*bv+b3**2+2*c1*c2+c1*c3+c2*c3+c3**2))/(1260*dt)
      aaf(14,13)=(4*re*ae*(12*vx(6)*c1+8*vx(6)*c3+12*vx(5)*c2+8*vx(5)
     . *c3+8*vx(4)*c1+8*vx(4)*c2+5*vx(3)*c3+vx(2)*c2+vx(1)*c1)+420*ae
     . *bv*(2*b1*c2+b1*c3+b3*c2+b3*c3))/315
      aaf(14,14)=(re*ae*(384*vx(6)*b1*dt+384*vx(6)*b3*dt+96*vx(5)*b1*
     . dt+192*vx(5)*b2*dt+128*vx(5)*b3*dt+128*vx(4)*b1*dt+192*vx(4)*
     . b2*dt+96*vx(4)*b3*dt+24*vx(3)*b1*dt+64*vx(3)*b3*dt-24*vx(2)*b1
     . *dt-48*vx(2)*b2*dt-24*vx(2)*b3*dt+64*vx(1)*b1*dt+24*vx(1)*b3*
     . dt+192*vy(6)*c1*dt+192*vy(6)*c3*dt+96*vy(5)*c1*dt+64*vy(5)*c3*
     . dt+64*vy(4)*c1*dt+96*vy(4)*c3*dt+24*vy(3)*c1*dt-16*vy(3)*c3*dt
     . -24*vy(2)*c1*dt-24*vy(2)*c3*dt-16*vy(1)*c1*dt+24*vy(1)*c3*dt+
     . 217)+1680*ae*dt*(b1**2*bv+b1**2+b1*b3*bv+b1*b3+b3**2*bv+b3**2+
     . c1**2+c1*c3+c3**2))/(630*dt)
      aaf(14,15)=(8*re*ae*(12*vx(6)*c1+12*vx(6)*c3+12*vx(5)*c2+4*vx(5
     . )*c3+4*vx(4)*c1+12*vx(4)*c2+5*vx(3)*c3-3*vx(2)*c2+5*vx(1)*c1)+
     . 420*ae*bv*(2*b1*c1+b1*c3+b3*c1+2*b3*c3))/315
      aaf(15,1)=(4*re*ae*(-2*vy(6)*b1+3*vy(6)*b3-2*vy(5)*b2-vy(5)*b3-
     . vy(4)*b1+3*vy(4)*b2-2*vy(3)*b3-vy(2)*b2+3*vy(1)*b1)+420*ae*b1*
     . bv*c3)/315
      aaf(15,2)=(re*ae*(40*vx(6)*b1+4*vx(5)*b1+20*vx(4)*b1-8*vx(3)*b1
     . -5*vx(2)*b1+12*vx(1)*b1+32*vy(6)*c1+12*vy(6)*c3+4*vy(5)*c1-8*
     . vy(5)*c2-4*vy(5)*c3+16*vy(4)*c1+12*vy(4)*c2-8*vy(3)*c1-8*vy(3)
     . *c3-5*vy(2)*c1-4*vy(2)*c2+24*vy(1)*c1)+420*ae*(b1*b3+bv*c1*c3+
     . c1*c3))/315
      aaf(15,3)=(re*ae*(-c1-2*c3))/3
      aaf(15,4)=(re*ae*(-12*vy(6)*b1-12*vy(6)*b3-12*vy(5)*b2-4*vy(5)*
     . b3-4*vy(4)*b1-12*vy(4)*b2-5*vy(3)*b3+3*vy(2)*b2-5*vy(1)*b1))/
     . 315
      aaf(15,5)=(re*ae*(-384*vx(6)*b2*dt+64*vx(5)*b2*dt+64*vx(4)*b2*
     . dt-64*vx(3)*b2*dt+48*vx(2)*b2*dt-64*vx(1)*b2*dt-192*vy(6)*c1*
     . dt-384*vy(6)*c2*dt-192*vy(6)*c3*dt-128*vy(5)*c2*dt-64*vy(5)*c3
     . *dt-64*vy(4)*c1*dt-128*vy(4)*c2*dt-64*vy(3)*c2*dt-80*vy(3)*c3*
     . dt+96*vy(2)*c2*dt-80*vy(1)*c1*dt-64*vy(1)*c2*dt-217))/(5040*dt
     . )
      aaf(15,6)=(-re*ae*(c1+c3))/3
      aaf(15,7)=(4*re*ae*(3*vy(6)*b1-2*vy(6)*b3+3*vy(5)*b2-vy(5)*b3-
     . vy(4)*b1-2*vy(4)*b2+3*vy(3)*b3-vy(2)*b2-2*vy(1)*b1)+420*ae*b3*
     . bv*c1)/315
      aaf(15,8)=(re*ae*(40*vx(6)*b3+20*vx(5)*b3+4*vx(4)*b3+12*vx(3)*
     . b3-5*vx(2)*b3-8*vx(1)*b3+12*vy(6)*c1+32*vy(6)*c3+12*vy(5)*c2+
     . 16*vy(5)*c3-4*vy(4)*c1-8*vy(4)*c2+4*vy(4)*c3+24*vy(3)*c3-4*vy(
     . 2)*c2-5*vy(2)*c3-8*vy(1)*c1-8*vy(1)*c3)+420*ae*(b1*b3+bv*c1*c3
     . +c1*c3))/315
      aaf(15,9)=(re*ae*(-2*c1-c3))/3
      aaf(15,10)=(4*re*ae*(8*vy(6)*b1+12*vy(6)*b3+8*vy(5)*b2+8*vy(5)*
     . b3+8*vy(4)*b1+12*vy(4)*b2+vy(3)*b3+vy(2)*b2+5*vy(1)*b1)+420*ae
     . *bv*(b1*c1+b1*c3+b2*c1+2*b2*c3))/315
      aaf(15,11)=(re*ae*(128*vx(6)*b1*dt+384*vx(6)*b2*dt+128*vx(5)*b1
     . *dt+128*vx(5)*b2*dt+128*vx(4)*b1*dt+192*vx(4)*b2*dt-16*vx(3)*
     . b1*dt-32*vx(3)*b2*dt-16*vx(2)*b1*dt-48*vx(2)*b2*dt-16*vx(1)*b1
     . *dt+48*vx(1)*b2*dt+256*vy(6)*c1*dt+384*vy(6)*c2*dt+192*vy(6)*
     . c3*dt+128*vy(5)*c1*dt+256*vy(5)*c2*dt+128*vy(5)*c3*dt+256*vy(4
     . )*c1*dt+384*vy(4)*c2*dt-16*vy(3)*c1*dt-32*vy(3)*c2*dt+16*vy(3)
     . *c3*dt-16*vy(2)*c1*dt-32*vy(2)*c2*dt+64*vy(1)*c1*dt+48*vy(1)*
     . c2*dt+217)+1680*ae*dt*(b1**2+b1*b2+b1*b3+2*b2*b3+bv*c1**2+bv*
     . c1*c2+bv*c1*c3+2*bv*c2*c3+c1**2+c1*c2+c1*c3+2*c2*c3))/(1260*dt
     . )
      aaf(15,12)=(4*re*ae*(12*vy(6)*b1+8*vy(6)*b3+12*vy(5)*b2+8*vy(5)
     . *b3+8*vy(4)*b1+8*vy(4)*b2+5*vy(3)*b3+vy(2)*b2+vy(1)*b1)+420*ae
     . *bv*(2*b2*c1+b2*c3+b3*c1+b3*c3))/315
      aaf(15,13)=(re*ae*(384*vx(6)*b2*dt+128*vx(6)*b3*dt+192*vx(5)*b2
     . *dt+128*vx(5)*b3*dt+128*vx(4)*b2*dt+128*vx(4)*b3*dt+48*vx(3)*
     . b2*dt-16*vx(3)*b3*dt-48*vx(2)*b2*dt-16*vx(2)*b3*dt-32*vx(1)*b2
     . *dt-16*vx(1)*b3*dt+192*vy(6)*c1*dt+384*vy(6)*c2*dt+256*vy(6)*
     . c3*dt+384*vy(5)*c2*dt+256*vy(5)*c3*dt+128*vy(4)*c1*dt+256*vy(4
     . )*c2*dt+128*vy(4)*c3*dt+48*vy(3)*c2*dt+64*vy(3)*c3*dt-32*vy(2)
     . *c2*dt-16*vy(2)*c3*dt+16*vy(1)*c1*dt-32*vy(1)*c2*dt-16*vy(1)*
     . c3*dt+217)+1680*ae*dt*(2*b1*b2+b1*b3+b2*b3+b3**2+2*bv*c1*c2+bv
     . *c1*c3+bv*c2*c3+bv*c3**2+2*c1*c2+c1*c3+c2*c3+c3**2))/(1260*dt)
      aaf(15,14)=(8*re*ae*(12*vy(6)*b1+12*vy(6)*b3+12*vy(5)*b2+4*vy(5
     . )*b3+4*vy(4)*b1+12*vy(4)*b2+5*vy(3)*b3-3*vy(2)*b2+5*vy(1)*b1)+
     . 420*ae*bv*(2*b1*c1+b1*c3+b3*c1+2*b3*c3))/315
      aaf(15,15)=(re*ae*(192*vx(6)*b1*dt+192*vx(6)*b3*dt+96*vx(5)*b1*
     . dt+64*vx(5)*b3*dt+64*vx(4)*b1*dt+96*vx(4)*b3*dt+24*vx(3)*b1*dt
     . -16*vx(3)*b3*dt-24*vx(2)*b1*dt-24*vx(2)*b3*dt-16*vx(1)*b1*dt+
     . 24*vx(1)*b3*dt+384*vy(6)*c1*dt+384*vy(6)*c3*dt+96*vy(5)*c1*dt+
     . 192*vy(5)*c2*dt+128*vy(5)*c3*dt+128*vy(4)*c1*dt+192*vy(4)*c2*
     . dt+96*vy(4)*c3*dt+24*vy(3)*c1*dt+64*vy(3)*c3*dt-24*vy(2)*c1*dt
     . -48*vy(2)*c2*dt-24*vy(2)*c3*dt+64*vy(1)*c1*dt+24*vy(1)*c3*dt+
     . 217)+1680*ae*dt*(b1**2+b1*b3+b3**2+bv*c1**2+bv*c1*c3+bv*c3**2+
     . c1**2+c1*c3+c3**2))/(630*dt)
      return
      end


