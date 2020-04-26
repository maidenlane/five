' **************************************************
'    Solving and testing the second model

' **************************************************
'    we set the directory
'
cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'   we direct the output to psim_b.rtf

output(o,r) psim_b
'
'     we open the workfile
'
close pic_q.wf1
open pic_q.wf1
pageselect pic_b

'     We check the residuals

'   We have to set the scenario back to "baseline"
'   This is a way to drop the previous overrides

smpl 1980Q1 2004Q2
pic_b.scenario "baseline"
pic_b.append assign @all _c
pic_b.solveopt(n=t m=1000,o=g,d=f) 
solve pic_b

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr dc_{%1}={%1}-{%1}_c
genr pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next

'     We solve over the past
'     with residuals set to zero
'     as we did for the first model

for !i=1 to g_vbeha.@count
%1=g_vbeha.@seriesname(!i)
genr ec_{%1}_s=0
next

pic_b.scenario "scenario 1"
pic_b.append assign @all _s
pic_b.solveopt(n=t m=1000,o=g,d=d) 
pic_b.override ec_k ec_ic ec_le ec_m ec_x
solve pic_b

'     We empty the group of overriiden variables
'      to avoid later mistakes

pic_b.override  

'    We compute again the residuals and their averages
'    - the relative errors
'    - the absolute errors on the growth rates

'     We can compare with the previous model
'     (only the common variables)

matrix(18,2)  v_psm_b
for !i=1 to g_vendo.@count
%st1=g_vendo.@seriesname(!i)
%st2=g_vendo.@seriesname(!i)+"_s"
series psa_{%st1}=100*({%st2}-{%st1})/({%st1}+({%st1}=0))
series tc_{%st2}=100*({%st2}-{%st2}(-1))/({%st2}(-1)+({%st2}(-1)=0))
series tc_{%st1}=100*({%st1}-{%st1}(-1))/({%st1}(-1)+({%st1}(-1)=0))
series psd_{%st1}=tc_{%st2}-tc_{%st1}
v_psm_b(!i,1)=(@mean(@abs(psa_{%st1})))
v_psm_b(!i,2)=(@mean(@abs(psd_{%st1})))
smpl 1980Q1 2004Q4
next

'      We export the residuals to an Excel file

group errors  psa_*  psd_*
smpl 1980Q1 2004Q4
write(t=xls,t) simul.xls errors

'     We produce a shock

'     First, an unshocked simulation, as before

pic_b.append assign @all _b
pic_b.solveopt(n=t m=1001,o=g,d=d) 
pic_b.override 
solve pic_b

'     The shocked simulation

pic_b.solveopt(n=t m=1002,o=g,d=d) 
smpl 1980Q1 2004Q4
genr gd_v=gd+.01*q_b*(t>=1981)
genr dv_gd=+.01*q_b*(t>=1981)

'     We have to use a new scenario

pic_b.scenario "scenario 1"
pic_b.append assign @all _v
pic_b.override gd
solve pic_b

'     We compute the differences
'     in absolute and relative terms

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series dv_{%1}={%1}_v-{%1}_b
series pv_{%1}=100*dv_{%1}/({%1}_b+({%1}_b=0))
next

'     We export the values to Excel

group chocs  pv_* dv_* 
write(t=xls,t) varia.xls chocs

'     We save the workfile

save pic_q


