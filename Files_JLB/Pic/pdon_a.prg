
'    
' ************************************************************************
'
'       This program creates the data and the framework for the first model
'       The estimated equations are just stated as formal links
'
'       This allows to observe the logic of the model
'        and to check that identities hold true

' ************************************************************************

'       We set the directory
'       to guarantee that we will access the right files
'      This should be updated according to the local situation

cd "c:\users\jean louis\skydrive\eviews\book_files\pic"

'   we direct the output to pdon_a.rtf

output(r,o) pdon_a
' 
'     we open the work file pic_a (semi-annual from 1962Q1 to 2010Q4)
'     but the historical data ends in 2002S1

'    Questions 1 and 2 : creating the workfile and the oecd sheet.

close pic_q
wfcreate(wf=pic_q,page=oecd) q 1962Q1 2010Q4

'   Question 3 :  Importing the data

'      We delete all data in case the file existed already
'      in which case the previous version is accessed
'      and it could contain irrelevant data

delete *

'     we read the French OECD data from fra_q.xls
'
smpl 1962Q1 2010Q4
read(t=xls,t,s=fra_1) fra_q.xls 72 
genr fra_gdv=fra_igv+fra_cgv
genr fra_wager=fra_wsss/fra_pcp/fra_et

'    Question 4 : creating the model sheet

pagecreate(page=pic_a) q 1962Q1 2010Q4
'
'     Question 5 : Creating the model
'
scalar f=0
if @isobject("pic_a") then
delete pic_a
endif

model pic_a
pic_a.append ic=f*(q)
pic_a.append le=f*(q)
pic_a.append i=f*(q)
pic_a.append lt=le+lg
pic_a.append rhi=wr*lt+r_rhiq*q
pic_a.append co=rhi*(1-sr)
pic_a.append ih=r_ih*rhi
pic_a.append fd=co+i+gd+ic+ih
pic_a.append m=f*(fd)
pic_a.append x=f*(wd)
pic_a.append q+m=fd+x
pic_a.append k=k(-1)*(1-dr)+i

'    Question 6 : Producing the data

'     We create a time trend : the last two figures in the yearly date, +0.5 in the second semester

'     This will be used for creating dummies and trends
'     Using this method, no computation is necessary
'
smpl 1962Q1 1962Q1
genr T=1962
smpl 1962Q2 2010Q4
genr T=t(-1)+0.25
'
'     Now we create the data

'     We start with the simple transfers
'
smpl 1962Q1 2010Q4
for %1 CO FD Q I IH IC K LG LT M X RHI WD gd wr
link {%1}
next

'      We create the missing series

'   We create additional OECD series

pageselect oecd
genr fra_gdv=fra_igv+fra_cgv
genr fra_wager=fra_wsss/fra_pcp/fra_et

'     We move back to pic_a

pageselect pic_a
co.linkto oecd::fra_cpv
FD.linkto oecd::fra_tddv
Q.linkto oecd::fra_gdpv
I.linkto oecd::fra_ibv
IH.linkto oecd::fra_ihv
IC.linkto oecd::fra_iskv
K.linkto oecd::fra_kbv
LG.linkto oecd::fra_eg
LT.linkto oecd::fra_et
M.linkto oecd::fra_mgsv
X.linkto oecd::fra_xgsv
RHI.linkto oecd::fra_ydrh
WD.linkto oecd::fra_xgvmkt
gd.linkto oecd::fra_gdv
wr.linkto oecd::fra_wager 

'     Now we deal with tranformations
'     in the right order

genr LE=lt-lg
genr SR=(RHI-CO)/RHI
genr r_rhiq=(rhi-wr*lt)/q
genr DR=(i+k(-1)-k)/k(-1)
genr r_ih=ih/rhi

'   This method is the fastest and clearest
'   But it violates (a litlle) the principle on keeping a pure original set
'   Even it this set is not technically original
'
'    Question 7
'
'    Defining the groups
'
pic_a.makegroup(a,n) g_vendo @endog
group g_vbeha ic i m le x
pic_a.makegroup(a,n) g_viden @endog
g_viden.drop ic i m le x
pic_a.makegroup(a,n) g_vexog @exog
group g_varia g_vendo g_vexog

'     An alternate (automatic but tricky) version


pic_a.makegroup(a,n) g_vendo @endog
pic_a.makegroup(a,n) g_vexog @exog
group g_varia g_vendo g_vexog


pic_a.makegroup(a,n) g_viden @endog
group g_vbeha  
smpl 1986Q1 2004Q4
pic_a.append assign @all _c
scalar f=0
solve(d=f) pic_a
scalar f=1
pic_a.update
pic_a.append assign @all _d
solve(d=f) pic_a

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
series pf_{%1}=100*({%1}_d-{%1}_c)/({%1}_c+({%1}_c=0))
genr  {%1}_test=1
if @max(@abs(pf_{%1}))>1e-5 then 
g_viden.drop {%1}
g_vbeha.add {%1}
genr  {%1}_test=0
endif
next

'
'   Question 8
'
'    Checking the consistency of identities
'
smpl 1974Q1 2004Q4
pic_a.append assign @all _c
solve(d=f) pic_a
 

'     The following computations bypass an EViews bug
 
genr lt_c=le+lg
genr rhi_c=wr*lt+r_rhiq*q
genr co_c=rhi*(1-sr)
genr ih_c=r_ih*rhi
genr fd_c=co+i+gd+ic+ih 
genr q_c+m=fd+x
genr k_c=k(-1)*(1-dr)+i

for !i=1 to g_viden.@count
%1=g_viden.@seriesname(!i)
series dc_{%1}={%1}-{%1}_c
series pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next

'    We restore the normal period

smpl @all

'     Question 10

save pic_q


