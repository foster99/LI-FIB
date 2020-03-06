intersec(X,Y,I) :- sort(X,XS), sort(Y,YS), auxintersec(XS,YS,I).

auxintersec()
