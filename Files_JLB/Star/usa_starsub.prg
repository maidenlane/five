'============================================

'       This program computes for a set of exogenous variables
'        the values
'       which allow a set of endogenous to reach given solutions
 
'       The two sets must have the same dimension

'       The USA model is used as an example
'        but the program is quite general

'       The present program applies to a single period
'       but it can be generalized quite easily

'=============================================

'      We set the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\star"

'      Results can be dumped as text to a file called jacob.txt
'      overriding any previous elements

'      This can be done by suppressing the quote mark before each print statement

output(r,o) jacob

'
'     We create a specific workfile called usa_star
'     from the original usa_model

'     We make sure no file of this name is open at present
'
'

'********************************************
'    User-dependent information  
'
'********************************************
'
'     This paragraph defines the context-dependent elements

'      Before anything we need to open a workfile
'      In our case it will be usa_model
'      and we save it under usa_star

'      You should put your own names

'      (not through a parameter as creating a parameter requires an open workfile...)

close usa_model.wf1
close usa_star

wfopen usa_model
wfsave usa_star

'       Now we start creating the elements

'       The starting and ending periods
'       The name of the model
'       The two lists of variables which change status
'       The target values for the new exogenous

'        They must be typed in the appropriate spaces
'         in the lines showing : ***** User information ************

'        No other editing is necessary

'    We define the simulation periods

%date_1="1980"        '  ***** User information ************
%date_2="1995"        '  ***** User information ************

'       We define the sample range

sample _range %date_1 %date_2

%model= "_usa_1"      '  ***** User information  ************

'       We define the lists of elements which change status
'       exogenous to endogenous
'       endogenous to exogenous
'       The numbers must be the same

'       If not, the program stops with a message

'       present elements are : u_tcval : export/import ratio at current prices
'                                              u_cfgp : budget deficit in GDP points
'                                              u_tcho : unemployment rate (0.xx)
'                                              u_pu : local demand deflator 

'                                              u_dfg : Government demand at constant prices
'                                              u_tcse : Social contributions rate for firms
'                                              u_tcss : Social contributions rate for workers
'                                              u_lg : Government employment

group g_vendo1 u_cfgp u_tcval u_tcho u_pu         '  ***** User information ************   The list of new exogenous 
group g_vexog1 u_dfg u_tcss  u_tcse u_lg            '  ***** User information ************   The list of new endogenous

'    We copy the user model into _model

 copy %model _model           '  The model name

'       Now we solve the model once as Scenario a
'       as we shall define the targets as deviations from the base solution

'       This seems to better describe algorithm efficiency

'       In operational cases target values are probably pre-defined as figures 
'       and the three next lines will not be necessary

tic

_model.scenario(n) "Scenario a"
_model.append assign @all _a

 
smpl _range

solve _model

toc

'       The target values for the new exogenous

genr u_tcval_star=u_tcval_a+0.01       '  ***** User information ************   The target values
genr u_cfgp_star=u_cfgp_a+2.0         '  ***** User information ************   The target values
genr u_tcho_star=u_tcho_a+0.02        '  ***** User information ************   The target values
genr u_pu_star=u_pu_a*0.98               '  ***** User information ************   The target values


'*****************************************

'   End of user-defined statements

'*****************************************

'      We delete Scenario 1 (it exists)

_model.scenario(d) "Scenario 1"

'      We set the sample at the full range 

smpl @first @last
genr _dum2=1


smpl _range

'    We create a dummy variable for the full range
'    anc compute the number of observations      

if @isobject("_dum") then 
delete _dum
endif

genr _dum=1 

scalar _obsnb=@obssmpl

for !i=1 to g_vexog1.@count
vector(_obsnb) delv_{!i}
next
 

'      nstar is the common number

scalar nstar=g_vexog1.@count 

'      The loop will run on all observations in the range

'      The condition identifies the !iper th period in the sample
'      for which a lag of !iper will produce an NA
'      but not a lag of !iper+1 

for !iper=1 to _obsnb 
smpl _range if _dum(-!iper)=na and _dum(-!iper+1)<>na

'      We build Scenario 0 as the base
'      It will contain the current solution
'      The associated suffix will be "_0"
'      We declare all the overriden exogenous
'      (with the currently best solution)

'      (note : we start with a blank list and fill it one by one)

_model.scenario(n) "Scenario 0"

_model.append assign @all _0

_model.override  
for !i=1 to g_vexog1.@count
%1=g_vexog1.@seriesname(!i)
_model.override(m) {%1}
next

'     Now we create the alternate scenarios 
'     for computation of the Jacobian
'     They are numbered 1 to nstar (here 2)
'     The suffix is "_n"
'     The exogenous are the same as Scenario 0

'     Note : all exogenous have to be overriden
'     but only one will change from the _0 value
'     for a given scenario

for !i=1 to nstar
_model.scenario(n,a={!i},i="Scenario 0") Scenario {!i}
_model.append assign @all _{!i}
next

'     Now we define the Jacobian

matrix(nstar,nstar) jacob 


'   print jacob

'     Now we initialize the exogenous instruments
'     used to reach the target

for !i=1 to g_vexog1.@count
%1=g_vexog1.@seriesname(!i)
genr {%1}_cur={%1}
genr {%1}_0={%1}
for !j=1 to g_vexog1.@count
genr {%1}_{!j}={%1}
next
next

'      And the endogenous target variables

for !i=1 to g_vendo1.@count
%1=g_vendo1.@seriesname(!i)
genr {%1}_0={%1}
genr {%1}_cur={%1}_0
next

'     We set the maximum number of iterations
'     and the convergence criterion

scalar nitmax=20
scalar sconv=1e-6

'     We initialize as control variables
'      the number of iterations 
'      And the variable controlling the convergence

!niter=0
!iconv=0

'     We double the elements so they can be displayed

scalar niter=0
scalar iconv=0

'    We display several elements for control
     
'   print g_vexog1 g_vendo1
'   print *_cur *_star

'---------------------------------------------------------
'      Now we start the loop
'---------------------------------------------------------

'     It will run while convergence is not achieved 
'     and the maximum numbre of loops is not reached

while !niter<=nitmax and !iconv=0 

'     We increment the iteration counter

!niter=!niter+1
scalar niter=niter+1

'       We test for convergence
'       We suppose it is achieved (!iconv=1)
'       Then we look at the relative difference 
'        between the present solution and the target
'        for each targetted endogenous

'        If the relative difference is higher than the criterion
'        for any target
'        convergence is not yet reached (!iconv=0)

!iconv=1
scalar iconv=1

for !j=1 to g_vendo1.@count
%1=g_vendo1.@seriesname(!j)

genr dele_{!j}=({%1}_star-{%1}_cur)/({%1}_cur+({%1}_cur=0))
stom(dele_{!j},delv_{!j},_range) 

if @abs(delv_{!j}({!iper}))>sconv then
!iconv=0
scalar iconv=0
endif

next

'      We display the differences

'   print dele_*

'       We compute the base solution
'        defined as Scenario 0

_model.scenario "Scenario 0"
solve _model

'       The solution values are set as current

for !i=1 to g_vendo1.@count
%1=g_vendo1.@seriesname(!i)
genr {%1}_cur={%1}_0
if niter=1 then
genr {%1}_base={%1}_0
endif
next

'        We save the initial solution (for comparison with the shocked one later)

if niter=1 then
for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr {%1}_base={%1}_0
next
endif

'      We display the current solution (exogenous + endogenous) and the targets

'   print niter
'   print *_cur  *_star

'      Now we compute the Jacobian
'      We start a loop on the shocked instruments

for !i=1 to g_vexog1.@count

'       For each exogenous, we compute the whole set of overriding instruments
'       shocking only the current one by 0.01 %
'       while the others keep their base value
'       This is necessary to implement the current changes

for !j=1 to g_vexog1.@count
%2=g_vexog1.@seriesname(!j)
genr {%2}_{!i}={%2}_cur*(1+.001*({!i}={!j}))
next

'       We solve the model under Scenario !i

_model.scenario Scenario {!i}
_model.solve

'       Now we compute the relative change in the target endogenous
'       This will give a column of the Jacobian matrix

for !j=1 to g_vendo1.@count
%2=g_vendo1.@seriesname(!j)
genr _z=({%2}_{!i}-{%2}_cur)/({%2}_cur+({%2}_cur=0))/0.001
stom(_z,_zscal,_range)
jacob(!j,!i)=_zscal({!iper})
next

next

'      This ends the computation of the Jacobian
 
'       We compute its inverse

'   print jacob

matrix jacob_inv=@inverse(jacob)

'   print jacob_inv

'       We apply to each exogenous a change equal to
'       the inverse of the jacobian
'       times the remaining error.

'       This gives a new solution for the exogenous
'        We start the process again until convergence (hopefully)

for !i=1 to g_vexog1.@count
%1=g_vexog1.@seriesname(!i)
genr {%1}_old={%1}_cur
for !j=1 to g_vendo1.@count
%2=g_vendo1.@seriesname(!j)
genr {%1}_cur={%1}_cur+{%1}_old*jacob_inv(!i,!j)*({%2}_star-{%2}_cur)/({%2}_cur+({%2}_cur=0))
next
genr {%1}_{!i}={%1}_cur
genr {%1}_0={%1}_cur
genr delxc_{%1}=({%1}_cur-{%1}_old)/{%1}_old
genr delxt_{%1}=({%1}_cur-{%1})/{%1}
next
'   print delxc_*
'   print delxt_*
wend

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr delf_{%1}=100*({%1}_0-{%1}_base)/({%1}_base+({%1}_base=0))
next

'    we set the endogenous to the solution values

for !i=1 to g_vendo.@count
%1=g_vendo.@seriesname(!i)
genr {%1}={%1}_0
next

for !i=0 to g_vendo1.@count
_model.scenario(d) Scenario !i
next

next   '  end of the loop on periods

'     We restore the full sample

smpl _range

toc


