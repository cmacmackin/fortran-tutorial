set xrange  [0:15]
set xlabel  'Distance [m]'
set ylabel  'Electric Field [N/C]'
set y2label 'Electric Potential [V]'

set ytics  nomirror
set y2tics

V(x) = (sin(x)*x - 0.2*x**2)*(1.5 - 1.05*exp(0.03*x))
E(x) = -(sin(x)*x - 0.2*x**2)*(-0.0315*exp(0.03*x)) - \
       (1.5 - 1.05*exp(0.03*x))*(cos(x)*x + sin(x) - 0.4*x)

set terminal pdf

set output 'interesting.pdf'
plot V(x) title 'Potential Function' axes x1y2, \
     E(x) title 'Predicted Field', \
     'interesting_in.dat' notitle axes x1y2 w points pt 1 lc 0, \
     'interesting_calced.dat' notitle w points pt 1 lc 0
