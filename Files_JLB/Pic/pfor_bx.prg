' *******************************************************************
'
'    Producing rational expectations simulations
'
' *******************************************************************
'    We set the directory
'
cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'    We direct the output to pfor_px.rtf

output(o,r) pfor_ps
'
'     we set the workfile to pic_p, quarterly from 1970 to 2010
'
close pic_p.wf1
open pic_p.wf1

'   We save the workfile as pic_px

wfsave pic_px

'   We delete the page pic_x if it exists

pagedelete pic_x

'    We select the page pic_b

pageselect pic_b

'    We save it under pic_x

pagecopy(page=pic_x)

'    We extend it to 2110 (including all the series)

pagestruct(end=2110Q4)  *

'     We set the long term growth rates

scalar txq=exp(0.006)-1
scalar txn=exp(0.0005)-1

'     We extend the "time" series
 
smpl 2005Q1 2110Q4
genr T=t(-1)+0.25

'       We give values to the exogenous
'       using the growth rates associated with their dimension

for %1 gd wd
genr {%1}={%1}(-1)*(1+txq)
next

for %1 rfdx r_rhiq dr sr pk compm ec_i ec_le ec_m ec_x  ec_ic ct r_ih  
genr {%1}={%1}(-1)
next

genr ec_ic=ec_ic+.002

for %1 lg 
genr {%1}={%1}(-1)*(1+txn)
next

for %1 wr 
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

genr T=t(-1)+0.25

'   We initialize the endogenous
'   again, according to their dimension

for %1 ic i m  x co fd k rhi q cap  ih
genr {%1}={%1}(-1)*(1+txq)
next

for %1 ur res_m  res_x  
genr {%1}={%1}(-1)
next

for %1 le lt led
genr {%1}={%1}(-1)*(1+txn)
next

for %1 prle_t 
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next 

'     We estimate the investment equation again

coef(10) c_i
smpl 1980q1 2004Q4  
genr ec_i=0 
equation eq_i.ls i/k(-1)=c_i(1)*i(-1)/k(-2)+c_i(2)*(ur_star-ur)/ur+c_i(3)*.125*(q-q(-8))/q(-8)+c_i(4)+ec_i
genr ec_i=resid
smpl 2005Q1 2110Q4 
genr ec_i=ec_i(-1)

'     We transfer the coefficients to a vector of parameters
'     except for the constant term

vector(10) b_i
b_i(1)=c_i(1)
b_i(2)=c_i(2)
b_i(3)=c_i(3) 

'    We create the model
'    Identical to the base version 

delete pic_x
model pic_x
pic_x.append cap=pk*k(-1)
pic_x.append ur=q/cap
pic_x.append q+m=fd+x
pic_x.merge eq_i
pic_x.append log(prle_t)=c_prle(1)+c_prle(2)*(t-2004)+c_prle(3)*(t-t1)*(t<t1)+c_prle(4)*(t-t2)*(t<t2)
pic_x.append led=q/prle_t
pic_x.merge eq_le
pic_x.append lt=le+lg
pic_x.append rhi=wr*lt+r_rhiq*q
pic_x.append co=rhi*(1-sr)
pic_x.append ih=r_ih*rhi
pic_x.merge eq_ic
pic_x.append fd=co+i+gd+ic+ih
pic_x.append res_m=p_m(1)*log(m/(fd+ct*q))+p_m(2)*log(ur)+p_m(3)*log(compm)+ p_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_x.merge eq_m
pic_x.append res_x=p_x(1)*log(x/wd)+p_x(2)*log(ur)+p_x(3)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_x.merge eq_x
pic_x.append k=k(-1)*(1-dr)+i 

'    We solve the model using the suffix "_b0"

pic_x.scenario "scenario 1"
pic_x.append assign @all _b0
pic_x.override
solve(m=9000,o=g,i=p,c=1e-6)  pic_x 

'      Now we test the four cases
'      using a different accelerator in the investment equation

'      Case 1 : full backward : q/q(-8)
'      Case 2 : partial forward : q(+4)/q(-4)
'      Case 3 : full forward : q(+8)/q
'      Case 4 : full long term forward : q(+20)/q

'     We treate the four cases in a loop

for !j=1 to 4

'    We must merge the equation and update the model 
'    to have the change in the formula taken into account

'    We could also have used a varying parameter
'     we think this presentation is clearer

smpl 1980Q1 2004Q4
genr ec_i=0

if !j=1 then 
equation eq_i.ls i/k(-1)=b_i(1)*i(-1)/k(-2)+b_i(2)*(ur_star-ur)/ur+b_i(3)*.125*(q-q(-8))/q(-8)+c_i(4)+ec_i 
else if !j=2 then
equation eq_i.ls i/k(-1)=b_i(1)*i(-1)/k(-2)+b_i(2)*(ur_star-ur)/ur+b_i(3)*.125*(q(4)-q(-4))/q(-4)+c_i(4)+ec_i 
else if !j=3 then
equation eq_i.ls i/k(-1)=b_i(1)*i(-1)/k(-2)+b_i(2)*(ur_star-ur)/ur+b_i(3)*.125*(q(8)-q)/q+c_i(4)+ec_i 
else if !j=4 then
equation eq_i.ls i/k(-1)=b_i(1)*i(-1)/k(-2)+b_i(2)*(ur_star-ur)/ur+b_i(3)*.025*(q(40)-q)/q+c_i(4)+ec_i 
endif
endif
endif
endif

genr ec_i=resid

pic_x.merge eq_i

'    We update the model to take into account the new values

pic_x.update

'     We extend the estimated residual 

smpl 2005Q1 2110Q4
genr ec_i=ec_i(-1)

'   We give to the endogenous the value of the base simulation

for %1 ic i m  x co fd k rhi q cap  ih
genr {%1}={%1}_b0
next

for %1 ur res_m  res_x 
genr {%1}={%1}_b0
next

for %1 le lt led
genr {%1}={%1}_b0
next

for %1 prle_t 
genr {%1}={%1}_b0
next

'   we assign the suffix for the base simulation
'   "_b" and the case number

'    We set the maximum number of iterations to a large number
'    different in each case
'    This will help identify the non-converging cases
'     as the error message will contain this number

'    We solve using the unshocked set of assumptions

smpl 2005Q1 2100Q4 
pic_x.scenario "scenario 1"
pic_x.append assign @all _b{!j}
pic_x.override
solve(m=900{!j},o=g,i=p,c=1e-6)  pic_x

'    We compute the shocked variable
'    by adding one point of the simulated GDP
'    between 2010 and 2040

'    Simulations run until 2100
'    but the 10 year ahead expectations call for values until 2110

smpl 2005Q1 2110Q4 
genr gd_v{!j}=gd+.01*q_b{!j}*(t>=2010)*(t<=2040)
smpl 2005Q1 2100Q4 

'   We assign the suffix for the shocked simulation
'    as "_v" followed by the number of the case

pic_x.append assign @all _v{!j}
pic_x.override gd
solve(m=901{!j},o=g,i=p,c=1e-6)  pic_x

'    The differences also use the number of the test

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr dv{!j}_{%1}={%1}_v{!j}-{%1}_b{!j}
genr pv{!j}_{%1}=100*dv{!j}_{%1}/({%1}_b{!j}+({%1}_b{!j}=0))
next
next

'    We create two sets of graphs

'    First set : a graph for each case, with GDP, capacity and rate of use 

%title_1="Full backward"
%title_2="Mixed"
%title_3="Full forward"
%title_4="Full forward, long horizon"

for !i=1 to 4
%gr="_gr1_"+@str({!i})
if @isobject(%gr) then
delete  _gr1_{!i}
endif
graph _gr1_{!i} pv{!i}_q pv{!i}_cap pv{!i}_ur  ' This list can be increased (with the associated "setelem")
_gr1_{!i}.options gridl gridauto 
_gr1_{!i}.legend position(l) 
_gr1_{!i}.options gridl gridauto 
_gr1_{!i}.setelem(1) symbol(1) lcolor(blue) lwidth(1.50) lpat(1) legend(Gross domestic product) symsize(xs) 
_gr1_{!i}.setelem(2) symbol(2) lcolor(red) lwidth(1.50) lpat(2) legend(Capacity) symsize(xs)
_gr1_{!i}.setelem(3) symbol(3)lcolor(green) lwidth(1.50) lpat(3) legend(Rate of use) symsize(xs)
_gr1_{!i}.addtext(0.7,-0.55,font=12,x) %title_{!i}
next

'     Second set : a graph for each important variable (GDP, rate of use, investment and final demand) with all cases together :
'
%title_q="Gross domestic product"
%title_ur="Rate of use"
%title_i="Investment"
%title_fd="Final demand"

for %1 q ur i fd   '   This list can be increased (with the associated %title" above)
%gr="_gr2_"+%1
if @isobject(%gr) then
delete  gr2_{%1}
endif
graph _gr2_{%1} pv1_{%1} pv2_{%1} pv3_{%1} pv4_{%1}
_gr2_{%1}.setelem(1) symbol(1) lcolor(blue) lwidth(1.50) lpat(1) legend(Full backward) symsize(xs) 
_gr2_{%1}.setelem(2) symbol(2) lcolor(red) lwidth(1.50) lpat(2) legend(Mixed backward-forward) symsize(xs)
_gr2_{%1}.setelem(3) symbol(8)lcolor(green) lwidth(1.50) lpat(3) legend(Full forward) symsize(xs)
_gr2_{%1}.setelem(4) symbol(4) lcolor(orange) lwidth(1.50) lpat(4) legend(Full forward long horizon)  symsize(xs)
_gr2_{%1}.options gridl gridauto 
_gr2_{%1}.addtext(0.7,-0.55,font=12,x) %title_{%1}
next

'    We save the workfile

wfsave pic_p


