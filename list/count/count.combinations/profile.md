choose_tail is more time-consuming!

Eshell V9.0.1  (abort with ^G)
1> c(choose).
{ok,choose}
2> choose:profile(choose, [12928, 2]).
Execution time: 2902916 microseconds
Output: 83560128
2902916
3> choose:profile(choose_tail, [12928, 2]).
Execution time: 3607434 microseconds
Output: 83560128
3607434
4> choose:profile(choose_tail, [12928, 2]).
Execution time: 3605644 microseconds
Output: 83560128
3605644
5> choose:profile(choose_tail, [12928, 2]).
Execution time: 3593908 microseconds
Output: 83560128
3593908
6> choose:profile(choose_tail, [12928, 2]).
Execution time: 3591577 microseconds
Output: 83560128
3591577
7> choose:profile(choose, [12928, 2]).     
Execution time: 2878899 microseconds
Output: 83560128
2878899
8> choose:profile(choose, [12928, 2]).
Execution time: 2903946 microseconds
Output: 83560128
2903946
9> choose:profile(choose, [12928, 2]).
Execution time: 2884763 microseconds
Output: 83560128
2884763
10> choose:profile(choose, [12928, 2]).
Execution time: 2887442 microseconds
Output: 83560128
2887442
11> choose:profile(choose, [12928, 2]).
Execution time: 2888292 microseconds
Output: 83560128
2888292
12> choose:profile(choose, [129281, 2]).
Execution time: 292436555 microseconds
Output: 8356723840
292436555
13> choose:profile(choose_tail, [129281, 2]).
Execution time: 368218088 microseconds
Output: 8356723840
368218088
14> 
