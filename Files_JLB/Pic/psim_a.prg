' *******************************************************************
'   
'     Creating and testing the first model
'
'********************************************************************
'    We set the directory
'
cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'   we direct the output to psim_a.rtf

output(r,o) psim_a
'
'     we open the workfile
'
close pic_q.wf1
open pic_q.wf1
pageselect pic_a

'    Question 1 : Producing the model

'     we add the new model variables to the group of identity elements
'      We drop them first in case a previous run has created them

g_viden.drop led prle_t
g_viden.add led prle_t
group g_vendo g_viden g_vbeha

'     The new model

'    We delete the model in case it exists already
'    This is the only way to delete identities

delete pic_a

'     We create an empty model

model pic_a

'      We append the identities and merge the estimated equations

pic_a.append q=fd+x-m
pic_a.merge eq_i
pic_a.append log(prle_t)=c_prle(1)+c_prle(2)*(t-2004)+c_prle(3)*(t-t1)*(t<t1)+c_prle(4)*(t-t2)*(t<t2)
pic_a.append led=q/prle_t
pic_a.merge eq_le
pic_a.append lt=le+lg
pic_a.append rhi=wr*lt+r_rhiq*q
pic_a.append ih=r_ih*rhi
pic_a.append co=rhi*(1-sr)
pic_a.merge eq_ic
pic_a.append fd=co+i+gd+ic+ih
pic_a.merge eq_m
pic_a.merge eq_x
pic_a.append k=k(-1)*(1-dr)+i

'    Question 2 : Checking model-data consistency

'     We check the residuals

smpl 1975q2 2004Q4

'     We set the suffix for simulations to "_c"

pic_a.append assign @all _c 

'     We solve using the dynamicity option "f"

solve(d=f) pic_a

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr dc_{%1}={%1}-{%1}_c
genr pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next 

'      Question 3 : Ex-post simulation

'     We solve over the past
'     with residuals set to zero

pic_a.scenario "scenario 1"

'   The new suffix is "_s"

pic_a.append assign @all _s

'     We create a loop over the estimated equations

'     We create a blank list of overriden variables

pic_a.override 

'     For each estimated, we create an alternate variable with value zero
'     and we add the variable to the list of overriden

'    In this way, any change will be taken into account automatically

smpl 1975q2 2004Q4
for !i=1 to g_vbeha.@count
%1=g_vbeha.@seriesname(!i)
genr ec_{%1}_s=0
pic_a.override(m) ec_{%1}
next 

'     We solve the model dynamically

solve(d=d) pic_a

'    We compute the residuals and their averages

'    *   the relative errors
'    *   the absolute errors on the growth rates

'    We store the results in a (n x 2) matrix 

matrix(14,2)  v_psm_a
for !i=1 to g_vendo.@count
%st1=g_vendo.@seriesname(!i)
%st2=g_vendo.@seriesname(!i)+"_s"
series psa_{%st1}=100*({%st2}-{%st1})/({%st1}+({%st1}=0))
series tc_{%st2}=100*({%st2}-{%st2}(-1))/({%st2}(-1)+({%st2}(-1)=0))
series tc_{%st1}=100*({%st1}-{%st1}(-1))/({%st1}(-1)+({%st1}(-1)=0))
series psd_{%st1}=tc_{%st2}-tc_{%st1}
v_psm_a(!i,1)=(@mean(@abs(psa_{%st1})))
v_psm_a(!i,2)=(@mean(@abs(psd_{%st1})))
smpl 1975q2 2004Q4
next

'      We export the residuals to an Excel file

group errors  psa_* psd_*
write(t=xls,t) simul.xls errors

'     Question 4 : A shock

'     We produce a shock

'    First, an unshocked simulation

smpl 1980Q1 2004Q4
pic_a.scenario "scenario 1"

'    The simulation suffix is now "_b"

pic_a.append assign @all _b

'      We eliminate any previous override

pic_a.override  
pic_a.solveopt(n=t,m=1001,o=g,d=d) 
solve pic_a

'     The shocked simulation

smpl 1980Q1 2004Q4

'      We shock Government demand by 1% of GDP starting in 1981 first quarter

genr dv_gd=.01*q_b*(t>=1981) 
genr gd_v=gd+dv_gd 
pic_a.solveopt(n=t,m=1002,o=g,d=d) 
pic_a.scenario "scenario 1"

'     The suffix is now "_v"

pic_a.append assign @all _v

'      We override gd with gd_v

pic_a.override gd
solve pic_a

'     We compute the differences
'     in absolute and relative terms

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series dv_{%1}={%1}_v-{%1}_b
series pv_{%1}=100*dv_{%1}/({%1}_b+({%1}_b=0))
next

'     We can export the values to Excel

group chocs  pv_* dv_* 
write(t=xls,t) varia.xls chocs

'     We set the sample period to maximum

smpl @all

'      We store the workfile

save pic_q


