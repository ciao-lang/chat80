:- module(ndtabl, [nd/3, nd/4, nd/5], [assertions]).
% NDTABL - Meta-information about database relations.

:- set_prolog_flag(discontiguous_warnings,off).

% :- pred nd(+,-,-).
:- pred nd/3 : nonvar * var * var => ground * ground * ground.
% :- pred nd(+,-,-,-).
:- pred nd/4 : nonvar * var * var * var => ground * ground * ground * ground.
% :- pred nd(+,-,-,-).
:- pred nd/5 : nonvar * var * var * var * var => ground * ground * ground * ground * ground.

nd(african,19,26).
nd(american,19,26).
nd(area,51,51).
nd(area,22,22,51).
nd(asian,21,26).
nd(aggregate,103,3,100,51).
nd(one_of,99,200,-99).
nd(ratio,99,51,51,3).
nd(card,99,100,3).
nd(borders,29,22,22).
nd(capital,22,22).
nd(capital,22,22,23).
nd(city,18,18).
nd(continent,8,8).
nd(country,22,22).
nd(drains,16,16,10).
nd(eastof,40,22,22).
nd(european,19,26).
nd(exceeds,99,51,51).
nd(flows,19,16,22).
nd(flows,19,16,22,22).
nd(in,29,26,15).
nd(latitude,23,23).
nd(latitude,22,22,23).
nd(longitude,26,26).
nd(longitude,22,22,26).
nd(northof,40,22,22).
nd(ocean,7,7).
nd(population,51,51).
nd(population,23,23,51).
nd(region,12,12).
nd(rises,16,16,22).
nd(river,16,16).
nd(sea,8,8).
nd(place,23,23).
nd(seamass,10,10).
nd(southof,40,22,22).
nd(westof,40,22,22).
nd(=<,99,51,51).
nd(<,99,51,51).
nd(>,99,51,51).
nd(>=,99,51,51).
