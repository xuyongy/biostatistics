DATA LIST Free/x.
Begin data
16.80      
14.80      
19.50      
35.80      
15.40      
12.30     
10.20     
21.40     
48.30      
27.50      
10.60      
17.10      
17.80      
19.00      
End data.
FLIP /VARIABLES=x.
Save OUTFILE=worktemp/Drop CASE_LBL.
SET MXLOOP =9999.
NEW FILE.
DEFINE lambda (Obs =!TOKENS(1) !default(1000)) 
INPUT PROGRAM.
- VECTOR X(1).
- LOOP Obs = 1 TO !Obs.
- LOOP #J = 1 TO 1.
- END LOOP.
- END CASE.
- END LOOP.
- END FILE.
END INPUT PROGRAM.
!ENDDEFINE.

Lambda.

COMPUTE x=x+0.005.
LEAVE x.
Comp lam=x-2.
EXECUTE.
MATCH FILES /FILE=*/FILE=worktemp.
EXECUTE.
Matrix.
GET X
  /VARIABLES = All
  /MISSING =accept
  /SYSMIS =0.
*PRINT X(1:5,5:18)/FORMAT "f10.1"/ TITLE="ÂíÊÏ¾àÀë" /SPACE=3.
LOOP #J=2 TO NROW(X).
+ COMPUTE X(#J,5:18)=X(1,5:18).
END LOOP.
*PRINT X(1:5,5:18)/FORMAT "f10.1"/ TITLE="ÂíÊÏ¾àÀë" /SPACE=3.
SAVE {X(:,2:18)} /OUTFILE=*
/VARIABLES = obs x lam y1 TO y14.

End matrix.


VECTOR V=Var001 TO Var014.
LOOP #I=1 TO 14.
+ COMPUTE V(#I)=ln(V(#I)).
END LOOP.
LIST Var001 Var014.

MISSING VALUES V1 TO V8 (99).
COMPUTE MEANSUB=MEAN(V1 TO V8).


DATA LIST FREE /V1 V2 V3 V4 V5 V6 V7 V8.
MISSING VALUES V1 TO V8 (99).
COMPUTE MEANSUB=MEAN(V1 TO V8).
VECTOR V=V1 TO V8.
LOOP #I=1 TO 8.
+ DO IF MISSING (V(#I)).
+ COMPUTE V(#I)=MEANSUB.
+ END IF.
END LOOP.
BEGIN DATA
1 99 2 3 5 6 7 8
2 3 4 5 6 7 8 9
2 3 5 5 6 7 8 99
END DATA.
LIST.



INPUT PROGRAM.
+ LOOP #I = 1 TO 1000.
+ DO REPEAT RESPONSE = R1 TO R400.
+ COMPUTE RESPONSE = UNIFORM(1) > 0.5.
+ END REPEAT.
+ COMPUTE AVG = MEAN(R1 TO R400).
+ END CASE.
+ END LOOP.
+ END FILE.
END INPUT PROGRAM.
FREQUENCIES VARIABLE=AVG.




INPUT PROGRAM.
*DATA LIST FILE=PRICES /YEAR 1-4 QUARTER 6 PRICE 8-12(2).
DO IF (YEAR GE 1881). /*Stop reading when reaching 1881
END FILE.
END IF.
END INPUT PROGRAM.
LIST.
DATA LIST Free/x.
Begin data
16.80      
14.80      
19.50      
35.80      
15.40      
12.30     
10.20     
21.40     
48.30      
27.50      
10.60      
17.10      
17.80      
19.00      
End data.
DEFINE lambda (Obs =!TOKENS(1) !default(1000)) 
INPUT PROGRAM.
- VECTOR X(1).
- LOOP Obs = 1 TO !Obs.
- LOOP #J = 1 TO 1.
- END LOOP.
- END CASE.
- END LOOP.
- END FILE.
END INPUT PROGRAM.
!ENDDEFINE.

Lambda.

COMPUTE x=x+0.005.
LEAVE x.

Comp lam=x-2.




* Factorial routine.
DATA LIST FREE / X.
BEGIN DATA
1 2 3 4 5 6 7
END DATA.
COMPUTE FACTOR=1.
LOOP #I=1 TO X.
COMPUTE FACTOR=FACTOR * #I.
END LOOP.
LIST.


* Example of nested loops: compute every possible combination of values for each variable.
INPUT PROGRAM.
-LOOP #I=1 TO 4. /* LOOP TO NUMBER OF VALUES FOR I
+ LOOP #J=1 TO 3. /* LOOP TO NUMBER OF VALUES FOR J
   LOOP #K=1 TO 4. /* LOOP TO NUMBER OF VALUES FOR K
    COMPUTE I=#I.
    COMPUTE J=#J.
    COMPUTE K=#K.
   END CASE.
   END LOOP.
+ END LOOP.
-END LOOP.
END FILE.
END INPUT PROGRAM.
LIST.



New file.
DATA LIST Free/x.
Begin data
16.80      
14.80      
19.50      
35.80      
15.40      
12.30     
10.20     
21.40     
48.30      
27.50      
10.60      
17.10      
17.80      
19.00      
End data.
New file.
LOOP.
COMPUTE X1=X1+1.
END LOOP IF (X EQ 115).
EXEcute.
PPLOT
  /VARIABLES=x
  /NOLOG
  /NOSTANDARDIZE
  /TYPE=Q-Q
  /Plot=Normal
  /FRACTION=BLOM
  /TIES=MEAN
  /DIST=NORMAL.
Comp lnx=ln(x).
Desc lnx /statis=sum.
Save OUTFILE=worktemp/Keep x.

NEW FILE.
DEFINE lambda (Obs =!TOKENS(1) !default(1000)) 
INPUT PROGRAM.
- VECTOR X(1).
- LOOP Obs = 1 TO !Obs.
- LOOP #J = 1 TO 1.
- END LOOP.
- END CASE.
- END LOOP.
- END FILE.
END INPUT PROGRAM.
!ENDDEFINE.

Lambda.

COMPUTE x=x+0.005.
LEAVE x.

Comp lam=x-2.

Comp y1=16.8.	
Comp y2=14.8.
Comp y3=19.5.
Comp y4=35.8.
Comp y5=15.4.
Comp y6=12.3.
Comp y7=10.2.
Comp y8=21.4.
Comp y9=48.3.
Comp y10=27.5.
Comp y11=10.6.
Comp y12=17.1.
Comp y13=17.8.
Comp y14=19.0.
EXECUTE.
VECTOR Y=y1 TO y14.
LOOP #I=1 TO 14.
+ COMPUTE Y(#I)=ln(Y(#I)).
END LOOP.
Comp lnsum=Sum(y1 to y14).
LOOP #I=1 TO 14.
+ COMPUTE Y(#I)=(exp(Y(#I))**lam-1)/lam.
END LOOP.


Comp funclam=-(14/2)*ln(((13)/14)*Variance(y1 to y14))+(lam-1)*lnsum.

Select if lam LT 2.

GRAPH
  /SCATTERPLOT(BIVAR)=lam WITH funclam
  /MISSING=LISTWISE .

Select if lam GT -1 AND lam LT -0.5.

GRAPH
  /SCATTERPLOT(BIVAR)=lam WITH funclam
  /MISSING=LISTWISE .

Select if lam GT -0.8 AND lam LT -0.6.

GRAPH
  /SCATTERPLOT(BIVAR)=lam WITH funclam
  /MISSING=LISTWISE .
Format lam funclam (f10.5).
List lam funclam.

Get FILE=worktemp.
Comp X070=(x**(-0.70)-1)/(-0.70).
PPLOT
  /VARIABLES=x x070
  /NOLOG
  /NOSTANDARDIZE
  /TYPE=Q-Q
  /Plot=Normal
  /FRACTION=BLOM
  /TIES=MEAN
  /DIST=NORMAL.
FLIP
  VARIABLES=x  .
