--%  ft| BASIN_B0 sn| fig.~7 n| #0 d| 1 n| #1 d| 55 n| #2 d| 150 n| #3 d| 0.3 n| #4 d| 4.46 n| #5 d| 1 n| #6 d| 1000 n| #7 d| 300 n| #8 d| 150 n| #9 d| 30 n| #10 d| 0 n| #11 d| 55 n| #12 d| 0 n| #13 d| 55 
--%  ft| BASIN_B0 sn| fig.9 n| #0 d| 1 n| #1 d| 58.5 n| #2 d| 200 n| #3 d| 0.3 n| #4 d| 30 n| #5 d| 1 n| #6 d| 1000 n| #7 d| 300 n| #8 d| 150 n| #9 d| 50 n| #10 d| 0 n| #11 d| 60 n| #12 d| 0 n| #13 d| 60 
--%  ft| TRAJECTORY_T0_V0_A1_O0 sn| cY:~20~~cM:~10~~wY:~1~~wM:~58.5~~r:~200~~b:~0.3~~kM:~30~~kO:~1~~ n| #0 d| 20 n| #1 d| 10 n| #2 d| 1 n| #3 d| 58.5 n| #4 d| 200 n| #5 d| 0.3 n| #6 d| 30 n| #7 d| 1 n| #8 d| 20 n| #9 d| 100 n| #10 d| 300 n| #11 d| cY n| #12 d| cM 
--@@
name = "Ol3G"
description = "Overlapping 3-Generation Model "
type = "D"
parameters = {"wY", "wM",  "r", "b", "kM",  "kO"}
variables = {"cY", "cM"}

function f (wY, wM,  r, b, kM, kO, cY, cM)


       v=r*b/math.exp(b*cY)
                 u=kM*wM/(v*(cY-wY)+kM+kO)

                 cY1=wY+wM-u*(1+(kO/kM^2)*cM*v)
                 cM1=u

return cY1, cM1

end
