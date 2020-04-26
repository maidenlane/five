' *******************************************************************
'
'    Producing a stochastic simulation
'
' *******************************************************************
'    We set the directory
'
cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'    We direct the output to pfor_ps.rtf

output(o,r) pfor_ps
'
'     we set the workfile to pic_ps, quarterly to 2100
'
close pic_q.wf1
close pic_ps.wf1
open pic_q.wf1
 
wfsave pic_ps.wf1 
pageselect pic_b
pagecopy(page=pic_s)
pagestruct(end=2100Q4) 

'    We define the growth rates for quantities and employment
 
scalar txq=exp(0.006)-1
scalar txn=exp(0.0005)-1

'     We set the time trend

smpl 2005Q1 2100Q4
genr T=t(-1)+0.25
 
'   We give values to the exogenous

'    Constant elements

for %1 rfdx r_rhiq dr sr pk compm ec_i ec_le ec_m ec_x  ec_ic ct r_ih  
genr {%1}={%1}(-1)
next

'    Quantities

for %1 gd wd
genr {%1}={%1}(-1)*(1+txq)
next

'     Populations

for %1 lg 
genr {%1}={%1}(-1)*(1+txn)
next

'     Real wage rate (=productivity of labor) and labour productivity trend

for %1 wr prle_t
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

'   We give values to the  endogenous
'   Not necessary in principle

'   Two purposes

'   1 : alllow an alternate starting point
'   2 : allow comparisons between simulated and theoretical growth rates

'      Constant

for %1 ur res_m  res_x
genr {%1}={%1}(-1)
next

'     Growth rate of quantities

for %1 ic i m  x co fd k rhi q cap  ih
genr {%1}={%1}(-1)*(1+txq)
next

'     Populations

for %1 le lt led
genr {%1}={%1}(-1)*(1+txn)
next

'   We define the model

'   First we delete it if it exists

if @isobject("pic_s") then
delete pic_s
endif

'    We define it completely 
'    without the productivity trend  
'    as it does not represent a behavior 
'    but the best approximation we have of an exact formula

model pic_s
pic_s.append cap=pk*k(-1)
pic_s.append ur=q/cap
pic_s.append q+m=fd+x
pic_s.merge eq_i 
pic_s.append led=q/prle_t
pic_s.merge eq_le
pic_s.append lt=le+lg
pic_s.append rhi=wr*lt+r_rhiq*q
pic_s.append co=rhi*(1-sr)
pic_s.append ih=r_ih*rhi
pic_s.merge eq_ic
pic_s.append fd=co+i+gd+ic+ih
pic_s.append res_m=p_m(1)*log(m/(fd+ct*q))+p_m(2)*log(ur)+p_m(3)*log(compm)+ p_m(4)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_s.merge eq_m
pic_s.append res_x=p_x(1)*log(x/wd)+p_x(2)*log(ur)+p_x(3)*(@trend(60:1)*(t<=2004)+@elem(@trend(60:1),"2004q4")*(t>2004))
pic_s.merge eq_x
pic_s.append k=k(-1)*(1-dr)+i

'    Note : as we have dropped the trend in labor productivity from the model
'    It has to be transferred to the "exogenous" list

pic_s.makegroup(a,n) g_vendo @endog 
pic_s.makegroup(a,n) g_vexog @exog

'     We add equations for the growth rates as *_tc
'      As we want to measure their uncertainty

'     The group g_vendo1 corresponds to the extended model

group g_vendo1
for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
pic_s.append {%1}_tc=100*({%1}-{%1}(-1))/{%1}(-1)
genr {%1}_tc=na
g_vendo1.add {%1}
g_vendo1.add {%1}_tc
next

'      We produce a deterministic siulation

smpl 2005Q1 2100Q4
pic_s.scenario "scenario 1"
pic_s.append assign @all _d
pic_s.override  
pic_s.exclude  
pic_s.solveopt(n=t m=1000,o=g,d=d,s=d) 
solve  pic_s

'     We produce a stochastic simulation
'     with 1000 replications (more?)

pic_s.scenario "scenario 1"
pic_s.append assign @all _s
pic_s.override  
pic_s.exclude  
pic_s.solveopt(n=t m=1000,o=g,d=d,s=a,r=10000) 
solve  pic_s

'     We compute the deterministic growth rates
'     and the ratios to the mean of

'     the standard error (in percentage) : se_*
'     the lowest bound     : shr_*
'     the highest bound    :  slr_*
'     the deterministic solution : sdr_*

'     note : displaying *r_q gives the three last elements

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series tc_{%1}=100*log(({%1}_d+({%1}_d=0))/({%1}_d(-1)+({%1}_d(-1)=0))) 
genr se_{%1}={%1}_ss/{%1}_sm*100
genr shr_{%1}={%1}_sh/{%1}_sm 
genr slr_{%1}={%1}_sl/{%1}_sm 
genr sdr_{%1}={%1}_d/{%1}_sm 
next

'     We save the workfile

wfsave pic_ps


