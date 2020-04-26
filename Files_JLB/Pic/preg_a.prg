' *******************************************************************
'
'    Estimating the equations of the first model
'
' *******************************************************************
'    We set the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'    We direct the output to preg_a.rtf 
'    deleting any existing file

output(r,o) preg_a
'
'     We open the workfile
'
close pic_q.wf1
open pic_q.wf1

'     We select the page

pageselect pic_a

'------------------------------------------------------
'   Estimating the change in inventories
'------------------------------------------------------
'
coef(12) c_ic
smpl 1962Q1 2004Q4
'
'    The tested equations (without residual)

'    The last yearly change

equation eq_ic1.ls(p) ic/q(-1)=c_ic(1)*@pchy(q) 

'    The last yearly change with a 1968 dummy

equation eq_ic2.ls(p) ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*@pchy(q) 

'    The last five quarterly changes

equation eq_ic3.ls(p) ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*@pch(q)+c_ic(3)*@pch(q(-1))+c_ic(4)*@pch(q(-2))+c_ic(5)*@pch(q(-3))+c_ic(6)*@pch(q(-4))
 
'   The last  five quarterly changes with a constant term

equation eq_ic4.ls(p) ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*@pch(q)+c_ic(3)*@pch(q(-1))+c_ic(4)*@pch(q(-2))+c_ic(5)*@pch(q(-3))+c_ic(6)*@pch(q(-4))+c_ic(7)  
 
'   The same with a lagged change in inventories

equation eq_ic5.ls(p) ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*ic(-1)/q(-2)+c_ic(3)*@pch(q)+c_ic(4)*@pch(q(-1))+c_ic(5)*@pch(q(-2))+c_ic(6)*@pch(q(-3))+c_ic(7)*@pch(q(-4))+c_ic(8)  

'    The same with polynomial distributed lags

 ls(p) ic/q(-1)   (t=1968.25) ic(-1)/q(-2) c PDL(@pch(Q),4,3,2)

vector(12) p_ic

p_ic(7)=c(7)
p_ic(8)=c(8)
p_ic(9)=c(9)
p_ic(10)=c(10)
p_ic(11)=c(11)

equation eq_ic6.ls(p)  ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*ic(-1)/q(-2)+c_ic(3) +p_ic(7)*@pch(q)+p_ic(8)*@pch(q(-1))+p_ic(9)*@pch(q(-2))+p_ic(10)*@pch(q(-3))+p_ic(11)*@pch(q(-4))

'    Two terms,  two trends

equation eq_ic7.ls(p) ic/q(-1)=c_ic(1)*@pch(q)+c_ic(2)*@pch(q(-1))+c_ic(3)*(t-1975)*(t<=1975)+c_ic(4)*(t-1975)*(t>=1975)

'   The selected equation (with residual, presently 0)

'     We set the residual to zero

genr ec_ic=0

'      We define the equation for ic as eq_ic with :

'      * a vector of coefficients called c_ic
'      * a residual called ec_ic

equation eq_ic.ls(p)  ic/q(-1)=c_ic(1)*(t=1968.25)+c_ic(2)*ic(-1)/q(-2)+c_ic(3) +p_ic(7)*@pch(q)+p_ic(8)*@pch(q(-1))+p_ic(9)*@pch(q(-2))+p_ic(10)*@pch(q(-3))+p_ic(11)*@pch(q(-4))+ec_ic

'      We store the estimation residual in ec_ic

genr ec_ic=resid

'--------------------------------
'   Estimating imports
'--------------------------------

coef(10) c_m
smpl 1962Q1 2004Q4
genr ec_m=0
equation eq_m1.ls(p) log(m)=c_m(1)*log(fd)+c_m(2) 
equation eq_m2.ls(p) log(m)=c_m(1)*log(fd)+c_m(2)+[ar(1)=c_m(3)]  
equation eq_m3.ls(p) log(m)=c_m(1)*log(fd)+c_m(2)*t+c_m(3) 
equation eq_m4.ls(p) log(m)=c_m(1)*log(fd)+c_m(2)*t+c_m(3)+[ar(1)=c_m(4)] 
equation eq_m.ls(p) log(m)=c_m(1)*log(fd)+c_m(2)*t+c_m(3)+[ar(1)=c_m(4)]+ec_m 
genr ec_m=log(m)-(c_m(1)*log(fd)+c_m(2)*t+c_m(3)+[ar(1)=c_m(4)])

smpl 1962Q1 2004Q4

'--------------------------------
'   Estimating exports
'--------------------------------

coef(10) c_x
smpl 1962Q1 2004Q4
genr ec_x=0
equation eq_x1.ls(p) log(x)=c_x(1)*log(wd)+c_x(2) 
equation eq_x2.ls(p) log(x)=c_x(1)*log(wd)+c_x(2)+[ar(1)=c_x(3)] 
equation eq_x.ls(p) log(x)=c_x(1)*log(wd)+c_x(2)+[ar(1)=c_x(3)]+ec_x
genr ec_x=log(x)-(c_x(1)*log(wd)+c_x(2)+[ar(1)=c_x(3)])  

'--------------------------------
'   Estimating investment
'--------------------------------

coef(10) c_i
smpl 1963Q1 2004Q4
genr ec_i=0
equation eq_i1.ls(p) i=c_i(1)*(q-q(-4))+c_i(2) 
equation eq_i2.ls(p) i=c_i(1)*(q-q(-4))+c_i(2)*k(-1) 
equation eq_i3.ls(p) i/k(-1)=c_i(1)*(q-q(-4))/k(-1)+c_i(2) 
equation eq_i4.ls(p) i/k(-1)=c_i(1)*@pchy(q)+c_i(2) 
equation eq_i5.ls(p) i/k(-1)=c_i(1)*1/4*@pchy(q)+c_i(2) 
equation eq_i6.ls(p) i/k(-1)=c_i(1)*i(-1)/k(-2)+c_i(2)*1/4*@pchy(q)+c_i(3) 
equation eq_i.ls(p) i/k(-1)=c_i(1)*i(-1)/k(-2)+c_i(2)*1/8*(q-q(-8))/q(-8)+c_i(3)+ec_i
genr ec_i=resid

'--------------------------------
'    Estimating firms employment
'--------------------------------

'     Labour productivity (the long term equation)

coef(10) c_prle
smpl 1962Q1 2004Q4
genr _z=na
genr prle=q/le
equation eq_prle1.ls(p) log(q/le)=c(1)+c(2)*t

'     The Chow tests

scalar min=100000
for !i=0 to 11
for !j=0 to 11
%a1=@otod(38+!i) 
%b1=@otod(39+!i) 
%a2=@otod(114+!j) 
%b2=@otod(115+!j)
smpl 1962Q1 2004Q1 
equation eq_prle1.ls log(q/le)=c(1)+c(2)*t
scalar v0=@ssr 
eq_prle1.chow(p) %a1 %a2
smpl 1962Q1 %a1 
equation eq_prle1.ls log(q/le)=c(1)+c(2)*t
scalar v1=@ssr 
smpl %b1 %a2 
equation eq_prle1.ls log(q/le)=c(1)+c(2)*t
scalar v2=@ssr 
smpl %b2 2002:1 
equation eq_prle1.ls log(q/le)=c(1)+c(2)*t
scalar v3=@ssr 
scalar logl=(V1+v2+v3)/v0
if logl<min then
%break1=%a1 
%break2=%a2 
min=logl
endif
next
next
smpl %break1 %break2
genr _z=0
scalar t1=0.25*(@dtoo(%break1))+1962
scalar t2=0.25*(@dtoo(%break2))+1962
scalar t2=1992.25
smpl 1962Q1 2004Q4
equation eq_prle.ls(p) log(prle)=c_prle(1)+c_prle(2)*(t-2004)+c_prle(3)*(t-t1)*(t<t1)+c_prle(4)*(t-t2)*(t<t2)
genr res_prle=resid
uroot(1,p) res_prle
uroot(h,p) res_prle

'      Estimating the dynamic equation

coef(10) c_le
genr ec_le=0
genr log(prle_t)=c_prle(1)+c_prle(2)*(t-2004)+c_prle(3)*(t-t1)*(t<t1)+c_prle(4)*(t-t2)*(t<t2)
genr led=q/prle_t
equation eq_le.ls(p) dlog(le)=c_le(1)*dlog(led)+c_le(2)*log(led(-1)/le(-1))+c_le(3)+c_le(4)*((t=1968.25)-(t=1968.50))+ec_le
genr ec_le=resid

'      Saving the workfile

save pic_q


