100 LET N = 0
110 LET A = 0
120 LET B = 1
130 PRINT A
140 IF N > 20 THEN 250
150 GOSUB 200
160 GOTO 130
200 LET T = B
210 LET B = A + B
220 LET A = T
230 LET N = N + 1
240 RETURN
250 END