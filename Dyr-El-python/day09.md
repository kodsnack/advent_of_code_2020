# Proof of my day 09 solution

At some point is an interval that adds up to the goal sum S.
Lets call this interval (a..b). We search for it by handling
a sum T for the interval (i..j).

For every iteraration if T < S we increase T by increasing
j. If T > S we decrease T by increasing i. If T = S we have
found our interaval. This can be implemented by by one addition
or subtraction from T at every iteration and either i or j is
increased, but not both which guarantee at most n*2 iterations.

This would fail to find the target interval if at any point
j steps past b or i steps past a. From the beginning both
i < a and j < b. Since one of them increase every iteration
at some point either i=a or j=b:

* if i = a and j < b then T < S since (i..j) is a
  subinterval of (a..b) and so b will increase until j = b
* if j = b and i < a then S < T since (a..b) is a
  subinterval of (i..j) and then a will increase until i = a
  