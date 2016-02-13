-module(geometry).
-export([area/1, perimeter/1, test/0]).


test() ->
    12 = round(perimeter({right_triangle, 3, 4})),
    tests_worked.

area({rectangle, Width, Height}) -> Width*Height;
area({square, Side}) -> Side*Side;
area({circle, Radius}) -> math:pi()*Radius*Radius;
area({right_triangle, Base, Hauteur}) -> Base*Hauteur/2.

perimeter({rectangle, Width, Height}) -> (Width+Height)*2;
perimeter({square, Side}) -> Side*4;
perimeter({circle, Radius}) -> math:pi()*Radius*2;
perimeter({right_triangle, Base, Hauteur}) -> Base + Hauteur + math:sqrt(Base*Base+Hauteur*Hauteur).
