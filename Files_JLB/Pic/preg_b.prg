' *****************************************************
'
'    Estimating the second model

'******************************************************
'    we set the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'   we direct the output to preg_b.rtf
'
output(r,o) preg_b
'
'     we open the pic_q workfile
'
close pic_q.wf1 
wfopen pic_q 

'     We delete the page "pic_b" if it exists

pagedelete pic_b

'     We copy the page "pic_a" into 'pic_b"

pageselect pic_a

pagecopy(page=pic_b)

'    We compute the supply side elements

smpl 1963Q1 2004q4

'    We get capacity from the OECD page

link cap
cap.linkto oecd::fra_caps

'    We set the technical coefficient to one

genr ct=1

'    We compute the productivity of capital and the rate of use

genr pk=CAP/K(-1)
genr UR=Q/CAP  

'   We reestimate investment including the rate of use

coef(10) c_i
smpl 1980Q1 2004Q4
genr ec_i=0

'      We suppose the target rate of use was reached on average on the past

scalar ur_star=@mean(ur)
equation eq_i7.ls i/k(-1)=c_i(1)*i(-1)/k(-2)+c_i(2)*(ur_star-ur)/ur+c_i(3)*.125*q/q(-8)+c_i(4)
equation eq_i.ls i/k(-1)=c_i(1)*i(-1)/k(-2)+c_i(2)*(ur_star-ur)/ur+c_i(3)*.125*q/q(-8)+c_i(4)+ec_i
genr ec_i=resid
smpl 1960Q1 2004Q4

'   We reestimate exports

'    First the cointegrating equation

smpl 1978Q2 2004Q4
genr ec_x=0
genr res_x=0
group g_x log(x/wd) log(ur)

'    We test the stationarity of the two elements

uroot(t,p) log(x/wd)  	 
uroot(t,p) log(ur) 

'     As it fails, we can test the cointegration of the two

coint(d,p,3) g_x

'     It works but does not give the value of coefficients as parameters
'     However, they can be obtained by testing the VAR
'     They are contained in the first line of the _var_x.b (hidden)

var _var_x.ec(d,p) 1 3 g_x
vector(10) p_x
p_x(1)=_var_x.b(1,1)
p_x(2)=_var_x.b(1,2)
p_x(3)=_var_x.b(1,3)

'      We compute the residual

genr res_x=p_x(1)*log(x/wd)+p_x(2)*log(ur)+p_x(3)*@trend(60S1)
            
'   We look if Ordinary Least Squares gives different results

equation eq_res_x.ls 0=log(x/wd)+c_x(1)+c_x(2)*t+c_x(3)*ur 

'   Then we estimate the dynamic equation

coef(10) c_x
genr ec_x=0
genr res_x2=-resid
equation eq_x3.ls dlog(x)=c_x(4)*dlog(wd)+c_x(6)*res_x(-1)+c_x(7)
equation eq_x.ls dlog(x)=c_x(4)*dlog(wd)+c_x(6)*res_x(-1)+c_x(7)+ec_x
genr ec_x=resid

'   We reestimate imports using the same methodology

coef(10) c_m
vector(10) p_m
genr ec_m=0
smpl 1978Q3 2004Q4
genr res_m=0

'    We test the stationarity of the two elements
'    using the share of imports in total demand

uroot(t,p) log(m/(fd+ct*q))  	 
uroot(t,p) log(ur)

'     It fails
'     We test the cointegration

coint(d,p,3) log(m/(fd+ct*q))  log(ur)

'     It fails too
  
'link compm1 
'compm1.linkto oecd::fra_compm
'genr compm=compm1(-8) 
 
link compm
compm.linkto oecd::fra_compm 

uroot(t,p) log(compm)
group g_m log(m/(fd+ct*q)) log(ur) log(compm)
coint(d,p,3) log(m/(fd+ct*q))  log(compm)
coint(d,p,3) log(compm)  log(ur)
coint(d,p,3) g_m
var _var_m.ec(d,p) 1 3 g_m
vector(10) p_m
p_m(1)=_var_m.b(1,1)
p_m(2)=_var_m.b(1,2)
p_m(3)=_var_m.b(1,3)
p_m(4)=_var_m.b(1,4)
genr res_m=p_m(1)*log(m/(fd+ct*q))+p_m(2)*log(ur)+p_m(3)*log(compm)+ p_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))                  
'
'   Only for testing if OLS gives different results
'
'  equation eq_res_m.ls(p) 0=log(m/(fd+ct*q))+c_m(2)*log(ur)+ c_m(3)*log(compm)+c_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
equation eq_res_m.ls(p) 0=log(m/(fd+ct*q))+c_m(2)*log(ur)+c_m(3)*log(compm)+c_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))

'     Now the dynamic equation

genr ec_m=0
genr res_m2=-resid
p_m(5)=1
smpl 1960Q1 2004Q4
equation eq_m5.ls dlog(m)=p_m(5)*dlog(fd+ct*q)+c_m(6)*dlog(ur)+c_m(7)*res_m(-1)+c_m(8)
equation eq_m6.ls dlog(m)=c_m(5)*dlog(fd+ct*q)+c_m(6)*dlog(ur)+c_m(7)*res_m(-1)+c_m(8)
equation eq_m.ls dlog(m)=c_m(5)*dlog(fd+ct*q)+c_m(6)*dlog(ur)+c_m(7)*dlog(compm)+c_m(8)*res_m(-1)+c_m(9)+ec_m
genr ec_m=resid

'      We create a new model

if @isobject("pic_b") then
delete pic_b
endif

model pic_b
pic_b.append cap=pk*k(-1)
pic_b.append ur=q/cap
pic_b.append q+m=fd+x
pic_b.merge eq_i
pic_b.append log(prle_t)=c_prle(1)+c_prle(2)*(t-2002)+c_prle(3)*(t-t1)*(t<t1)+c_prle(4)*(t-t2)*(t<t2)
pic_b.append led=q/prle_t
pic_b.merge eq_le
pic_b.append lt=le+lg
pic_b.append rhi=wr*lt+r_rhiq*q
pic_b.append co=rhi*(1-sr)
pic_b.append ih=r_ih*rhi
pic_b.merge eq_ic
pic_b.append fd=co+i+gd+ic+ih
pic_b.append res_m=p_m(1)*log(m/(fd+ct*q))+p_m(2)*log(ur)+p_m(3)*log(compm)+ p_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_b.merge eq_m
pic_b.append res_x=p_x(1)*log(x/wd)+p_x(2)*log(ur)+p_x(3)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_b.merge eq_x
pic_b.append k=k(-1)*(1-dr)+i

'    We add the new variables to the groups

g_viden.drop cap ur res_x res_m  
g_viden.add cap ur res_x res_m  
pic_b.makegroup(a,n) g_vexog @exog
pic_b.makegroup(a,n) g_vendo @endog

'     Again, we check model - data consistency

smpl 1987Q3 2004Q4
pic_b.append assign @all _c
pic_b.solveopt(n=t m=1000,o=g,d=f) 
solve pic_b
for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series dc_{%1}={%1}-{%1}_c
series pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next

'     We export the results to Excel

group errors  pc_* dc_* 
smpl 1987Q3 2004Q4
write(t=xls,t) check.xls errors

'      We save the results

save pic_q
smpl 1978Q1 2004Q4


