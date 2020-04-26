' *******************************************************************
'
'    Testing the model on the future
'
' *******************************************************************
'    We set the directory
'
cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'    We direct the output to pfor_b.rtf

output(o,r) pfor_b
'
'    We set the workfile to pic_q, quarterly from 1962 to 2010
'
close pic_q.wf1
close pic_p.wf1
open pic_q.wf1

'    We save it under the name pic_p
 
wfsave pic_p.wf1 
pageselect pic_b

'     We copy the pic_b page to pic_p
'     and extend it to 2100

pagecopy(page=pic_p)
pagestruct(end=2100Q4)  *

'     We set the two theoretical growth rates (or rather Dlogs)
'     txq : quantities
'     txn : populations

scalar txq=exp(0.006)-1
scalar txn=exp(0.0005)-1

'   We ewtend the time trend

smpl 2005Q1 2100Q4
genr T=t(-1)+0.25
 

'    We establish the exogenous assumptions

for %1 rfdx r_rhiq dr sr pk compm ec_i ec_le ec_m ec_x  ec_ic ct r_ih  
genr {%1}={%1}(-1)
next

for %1 gd wd
genr {%1}={%1}(-1)*(1+txq)
next

for %1 lg 
genr {%1}={%1}(-1)*(1+txn)
next

for %1 wr 
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

'    We establish the endogenous theoretical values

for %1 ur res_m  res_x
genr {%1}={%1}(-1)
next

for %1 ic i m  x co fd k rhi q cap  ih
genr {%1}={%1}(-1)*(1+txq)
next

for %1 le lt led
genr {%1}={%1}(-1)*(1+txn)
next

for %1 prle_t 
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

'      We recompute the coeffcient of the labour productivity trend
'      according to the assumptions on txq and txn

'      The constant term can keep its previous value
'      as the trend starts from the first forecasted period
'      and changing it does not affect the starting value

c_prle(2)=4*log((1+txq)/(1+txn)) 

'       We solve the model over the "future"

smpl 2005Q1 2100Q4
pic_b.scenario "Scenario 1"
pic_b.append assign @all _p

'    Just to make sure, a useful precaution in any case

pic_b.override
pic_b.solveopt(n=t m=1000,o=g,d=d) 
solve  pic_b

'      We compute the Dlogs of the solution ("tc_*")
'      The condition ({%1}_p=0) allows a zero variable to give 0
'      as Log(1/1)
'      and not "na" (and an error message)

'     We compute the difference to the theoretical growth rate tc0_*
'     It should decrease to zero in the long run

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr tc_{%1}=100*log(({%1}_p+({%1}_p=0))/({%1}_p(-1)+({%1}_p(-1)=0))) 
genr tc0_{%1}=100*log(({%1}+({%1}=0))/({%1}(-1)+({%1}(-1)=0))) 
genr dtc_{%1}=tc_{%1}-tc0_{%1}
next

'      We produce a shock starting in 2007

genr gd_v=gd+.01*q_p*(t>=2007)

pic_b.solveopt(n=t m=1002,o=g,d=d) 
pic_b.scenario "Scenario 1"
pic_b.append assign @all _v
pic_b.override gd

smpl 2005Q1 2100Q4
solve  pic_b

'    We compute the difference to the baseline solution

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series dv_{%1}={%1}_v-{%1}_p
series pv_{%1}=100*dv_{%1}/({%1}_p+({%1}_p=0))
next

'     We transfer the difference to an Excel file

group chocs  pv_* dv_* 
write(t=xls)  varia_p.xls chocs

'       We create groups for 

'      the supply - demand elements
'      the production elements

group g_v1 pv_fd pv_q pv_x pv_m
group g_v2 pv_q pv_cap pv_ur pv_le pv_k pv_i

'    We create the two associated graphs 

'    The "_" will place them at the beginning of the workfile sheet

if @isobject("_gr1") then
delete _gr1
endif

graph _gr1.line g_v1
_gr1.setelem(1) symbol(1) lcolor(blue) lwidth(1.50) lpat(1) legend(Final demand) symsize(xs)
_gr1.setelem(2) symbol(2) lcolor(red) lwidth(1.50) lpat(2) legend(Gross domestic product) symsize(xs)
_gr1.setelem(3) symbol(8)lcolor(green) lwidth(1.50) lpat(3) legend(Exports) symsize(xs)
_gr1.setelem(4) symbol(4) lcolor(orange) lwidth(1.50) lpat(4) legend(Imports)    symsize(xs)
_gr1.legend position(l) 
_gr1.options gridl gridauto
_gr1.addtext(0.7,-0.55,font=12,x) The supply - demand equilibrium    

close _gr1  
 
if @isobject("_gr2") then
delete _gr2
endif

graph _gr2.line g_v2
_gr2.setelem(1) symbol(1) lcolor(blue) lwidth(1.50) lpat(1) legend(Gross domestic product) symsize(xs)
_gr2.setelem(2) symbol(2) lcolor(red) lwidth(1.50) lpat(2) legend(Capacity) symsize(xs)
_gr2.setelem(3) symbol(8)lcolor(green) lwidth(1.50) lpat(3) legend(Rate of use) symsize(xs)
_gr2.setelem(4) symbol(4) lcolor(orange) lwidth(1.50) lpat(4) legend(Firms employment)  symsize(xs)
_gr2.setelem(5) symbol(3) lcolor(gray) lwidth(1.50) lpat(5) legend(Capital)  symsize(xs) 
_gr2.setelem(6) symbol(10) lcolor(ltgray) lwidth(1.50) lpat(6) legend(Investment)  symsize(xs)
_gr2.legend position(l) 
_gr2.options gridl gridauto 
_gr2.addtext(0.7,-0.55,font=12,x) Production and its factors   
 
close _gr2


