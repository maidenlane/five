'   
'   This program describes the functions associated to the building of a macroeconometric model under EViews

'    Although the model is extremely simple, all the main functions are treated

'  ----------- The model
'  
'   (1) Private consumption CO is a function of GDP : CO=f(Q). This function will be estimated

'   (2) GDP is the sum of private and public consumption g : Q=CO+g. This equation is supposed exact

'-------------  The program

'    First we set the directory
'    This is useful if we manage several projects at the same time

'    This guarantees that programs will access the right files
'                                       files will be stored in the right place
'                                       no file outside the directory will be wrongly affected

'    You should specify your own directory in the following statement 
'    or save the file using ""Save as" and clicking "Update default directory"
'
cd "c:\users\jean louis\skydrive\eviews\book_files\nano"

'   We direct the output to nano.rtf

'   This means that all "printed" output (mostly the results of tests for which the "p" option has been specified)
'    will go to the specified file

'    r calls for the RTF format (the richer, read by Word and the like). "t" would call for charecter text.
'    o specifies that any previous resukts stored under the same name will be overriden

output(r,o) nano
' 
'     We create the work file nano_q  (quarterly from 1975q1 to 2010q4) with a page callled "origin"
'     This page will contain the original series

'     If the workfile exists, it will be open
'     But if it exists and is open, a new version will be open in a new window
'     This is obviously dangerous, to avoid it we ask any open version to be closed
'     We also destroy any existing element (to start from a blank file)

'  *****  Creating the framework and the data

close nano_q.wf1
wfcreate(wf=nano_q,page=origin) q 1975q1 2010q4
delete *
'
'     We read  from nano_q.xls2 series of the French economy (OECD data) for
'     FRA_CPV : Private Consumption at constant 1995 prices
'     FRA_GDPV : Gross Domestic Product at constant 1995 prices

'     The data is organized as a table with series in colums, and its upper left cell is B2
'      (second row, second colum)
'
smpl 1975q1 2010q4
read(t=xls,b2) nano_q.xls 2

'      Now we create a page for the model elements

pagecreate(page=base)  q 1975q1 2010q4

'     We start creating the data
'
'     We create a time trend : the yearly date for the first quarter, then +0.25 per quarter

'
smpl 1975q1 1975q1
genr T=1975
smpl 1975q2 2010q4
genr T=t(-1)+0.25
'
'     We create the model data from two orginal series :

'     CO from FRA_CPV : private consumption at 1995 prices
'     Q from FRA_GDPV : gross domestic product at 1995 prices
'     g as the difference between Q and CO

smpl 1975q1 2010q4

'     We will create CO and Q by reference to original elements

'     First we define them as "linked" variables

link CO
link Q

'     Then we create the link giving the reference to page and the series 

CO.linkto origin::FRA_CPV
Q.linkto origin::FRA_GDPV

'     g is not available as such. We have to use the equation reversed

genr g=Q-CO

'     We create a first version of the model
'      just to check its structure
'      and the fact that it can be estilated
'

'      We ask EViews to destroy any existing version
'      to start from scratch
'      We do it only if the model exists
'       (this avoids an error message)
'       
if @isobject("nano") then 
delete nano
endif

'     We create a new model called "nano"

model nano

'   We add the first equation (identity)

nano.append Q=CO+g

'    For the second, we just declare our intention
'    We would like to write CO=f(Q)
'     but EViews will call for the function "f"

'    So we have to trick EViews into accepting the syntax
'    This is the closest we have found
'     f is defined as a scalar to avoid its identification as a (spurious) exogenous

scalar f=0
nano.append CO=f*(Q)
'
'    We produce a residual check

'    The model is broken into single equation ones
'    and each model is solved based on actual values
'
'     We use the largest period for which CO, Q and g are all available
'     equivalent to the availability of their product

smpl 1975q1 2004q4 if co*q*g<>na

'     We ask the results to be given the suffix "_c"

nano.append assign @all _c

'     We solve the model using the option "f"

solve(d=f) nano

'    We check that the identity holds true
'    by computing the difference between actual and computed
'    We also know that the behavioral equation can be estimated

genr dc_q=q_c-q
genr pc_q=dc_q*100/q

'     ******  Estimating the equation

'    We estimate the equation

'    First, a very simple case (linear in logs)

smpl 1975Q1 2004q4
ls(p) log(co) log(q) c

'    Equivalent to

ls(p) log(co)=c(1)*log(q)+c(2)

'    The first two positions of the "c" vector will be filled with estimated values
'     as well as the "Resid" series with the residual (actual left hand side - estimated right hand side)

'    second, with an autoregressive term

ls(p) log(co) log(q) c ar(1)

'     Now, a more sophisticated form using error correction

'     We suppose that the long term elasticity between CO and Q is unitary
'     but we leave it free in the short run

'    First, we test the stationarity of Log(CO/Q)

uroot(trend,p ) log(co/q)
uroot(trend,p,pp) log(co/q)

'   It works (the  negative T statistic is high enough)

'    Now two techniques

'   *****  The fast one (good for the exploratory phase)

ls(p) dlog(co) dlog(q) log(co(-1)/q(-1)) t c

'     problem : estimates will be erased by the next estimation
'     and where is the equation?

'     a little better : the equation is stored

equation eq1.ls(p)  dlog(co) dlog(q) log(co(-1)/q(-1)) c t

'   ****** the slow one (good once the choice is made)

'    We create a residual, with 0 as the initial value
'     The name of the residual connects it with the variable

genr ec_co=0

'    We create a vector of coefficients
'     The name of the vector also connects it with the variable

coef(10) c_co

'    We specify the equation using coefficient and residual
'     The name of the equation again connects it with the variable

'    The equation is estimated immediately using least (not necessarily ordinary) squares
'     To delay estimation, one must drop the ".ls"

equation eq_co.ls(p) dlog(co)=c_co(1)*dlog(q)+c_co(2)*log(co(-1)/q(-1))+c_co(3)*t+c_co(4)+ec_co

'    The residual is given the value of the left side - right side

genr ec_co=resid

'     We also compute the difference between the estimated and observed variables

genr co_h=exp(c_co(1)*dlog(q)+c_co(2)*log(co(-1)/q(-1))+c_co(3)*t+c_co(4))
genr dh_co=co-co_h
genr ph_co=dh_co*100/co

'    **** creating the model

'    Now we have our model
'    We delete the old version

delete nano

'     We create a new blank one

model nano

'     We introduce the same identity as before

nano.append Q=CO+g

'      And we merge the estimated equation
'      "merging" also includes statistics

nano.merge eq_co

'     Now we have to check model consistency again
'     We shall use the suffix "_d"

nano.append assign @all _d
solve(d=f) nano

'    We will use a loop (first form)
'    It starts with "for" followed by a parameter and a list, and ends with "next"

'   The statements in between will be repeated 
'    as many times as there are elements in the "for" list
'    each time the parameter is replaced by the current element

'   The brackets are just for locating the parameter
'    they are formally dropped after the replacement

'    A trick: if a variable is naturally zero (like a contemplated tax with a present zero rate) the error will be zero too
'    Computing the relative error in EViews will divide 0 by 0 generating an error, with a message
'    For us, 0/0

for %1 CO Q
series dd_{%1}={%1}-{%1}_d
series pd_{%1}=100*dd_{%1}/({%1}+({%1}=0))
next

'    Solving the model

'    Assessing the reliability over the past
'    Gives an idea of the reliability of forecasts
'    In the future, we will not know the residuals
'    So we set the residual at its most probable value : zero

'   ****** The "old" way

'    We store the residual value under an alternate name
  
genr ec_co_h=ec_co

'    We set the base value to zero

genr ec_co=0

'     We solve the model with the suffix "_s"

nano.append assign @all _s
solve(d=d) nano

'     We recover the estimation residual

genr ec_co=ec_co_h

'      This method is tedious, and runs the risk of losing the residual

'    The "scenario" way

'     We decide on a scenario, with the suffix "_s"

nano.scenario "scenario 1"
nano.append assign @all _s

'     We compute a zero residual with the suffix "_s"

genr ec_co_s=0

'     We declare that ec_co must use an alternate value
'      The suffixed variable must override the base one

nano.override ec_co
solve(d=d) nano


'     We compute the difference between simulation and history

for %1 CO Q
series ds_{%1}={%1}-{%1}_s
series ps_{%1}=100*ds_{%1}/({%1}+({%1}=0))
next

'     Now we test the economic properties of the model (!)
'     by measuring the response of the solution to a shock on assumptions

smpl 1964Q1 2004Q4

'    We still use "Scenario 1"
'    We must drop any overriding (in our case ec_co)
'    This is done by declaring a blank list

nano.override

'     The base solution (should take the historical values)

smpl 1975Q2 2004Q4

nano.append assign @all _b
solve(d=d) nano

'      We create an alternate g, increased by one point of GDP
'       starting in 1980, to check that with a zero shock we get the base solution

genr g_v=g+q*.01*(t>=1980)
nano.scenario "scenario 1"

'    Now the suffix is "_v"

nano.append assign @all _v

'    We override g

nano.override g 
solve(d=d) nano

'     We compute the difference between "_v" and "_b"

smpl 1979q1 2004q4
for %1 CO Q
series dv_{%1}={%1}_v-{%1}_b
series pv_{%1}=100*dv_{%1}/({%1}_b+({%1}_b=0))
next

'   Now we work on the future

'   We copy the page under a new name

pagecopy(page=forecast)

'     We extend the period to 2100 using all (*) elements

pagestruct(end=2100Q4) *

'      We modify the estimated equation 
'      We block the trend in the forecasting period
'      to ensure a steady state solution

'      Starting the trend in 2005 allows a zero value over the future
'      This does not affect the estimation or the estimated residual 

equation eq_co.ls(p) dlog(co)=c_co(1)*dlog(q)+c_co(2)*log(co(-1)/q(-1))+c_co(3)*(t-2005)*(t<=2005)+c_co(4)+ec_co

'      We store the news version of the equation

nano.merge eq_co

'     We extend the assumptions

'     First, the time trend!

smpl 2005Q1 2100Q4
genr t=t(-1)+0.25

'     The residual is kept at its last value

smpl 2005Q1 2100Q4
genr ec_co=ec_co(-1)

'     g will grow at 0.5% per quarter

genr g=g(-1)*1.005

'     We also initialize endogenous variables using the same assumption
'     This will give us an alternate starting point
'      and allow to check if the solution deviates from the theoretical value

genr q=q(-1)*1.005
genr co=co(-1)*1.005

'      We have to estimate the the equation again

smpl 1975 2004
eq_co.ls(p)

'      We solve the model with the suffix "_p"
'      making sure that no overriden value is called

smpl 2005Q2 2100Q4
nano.scenario "scenario 1"
nano.append assign @all _p
nano.override
solve(d=d) nano

'       Again, we produce a shock, starting in 2006

genr g_v=g+q_p*.01*(t>=2006)
nano.append assign @all _v
nano.override g
solve(d=d) nano

'     and we compute the deviations from the base

smpl 2005Q2 2100Q4
for %1 CO Q
series dw_{%1}={%1}_v-{%1}_p
series pw_{%1}=100*dw_{%1}/({%1}_p+({%1}_p=0))
next

pageselect forecast


