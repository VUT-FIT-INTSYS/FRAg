!count.

+!count <-+fct(0, 1).

+fct(X, Y) : X < 8 <-
    Z = X + 1;
    W = Z * Y;
    +fct(Z, W).

+fct(X, Y) : X == 8 <-
    .print("fact 8 ==", Y).
