# EBLUP
This is a project that relate to extension of sae methodology especially area level with non sampled using auxiliary information. EBLUP_FH seem like eblupFH in molina package sae, but i add some line for obtain random area effect (u) for another use.

## Usage

```R
data(milk)
result <- eblupFH(formula = milk$yi~as.factor(milk$MajorArea), vardir = milk$SD, method = "REML")
```

if you want to obtain u cap (random area effect) for each area, use `u_Cap` list: 

```R
result$u_Cap

$u_Cap
           [,1]
1   0.053781372
2   0.079412853
3   0.099762288
4  -0.207371921
5  -0.122031614
6   0.006183708
7   0.090263314
8  -0.003192931
9   0.120575827
10  0.094176429
11 -0.315753586
12  0.112976500
13  0.108690154
14 -0.117472392
15 -0.008710463
16 -0.039436960
17  0.031205957
18  0.090513449
19  0.041189515
20  0.039824795
21 -0.104833281
22 -0.002829470
23 -0.073488205
24  0.027894416
25 -0.001329750
26  0.035831578
27  0.038067136
28  0.006956420
29  0.043041544
30 -0.113446078
31  0.042667951
32  0.068937092
33  0.045430813
34 -0.116657696
35 -0.026709661
36  0.032390803
37 -0.197001198
38  0.016558711
39  0.028011663
40  0.043303945
41  0.021228483
42  0.077189394
43 -0.045800901
```

