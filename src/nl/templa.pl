:- module(templa,
    [ adjunction/4,
      aggr_adj/4,
      aggr_noun/4,
      attribute/6,
      comparator/5,
      intrans/6,
      measure/4,
      meta_noun/7,
      name_template/2,
      property/9,
      restriction/4,
      sign/2,
      thing/6,
      trans/9,
      units/2
    ],
    [assertions]).

:- use_module(chat80(db/world0), [ circle_of_latitude/1, city/1, continent/1,
    country/1, region/1, river/1, seamass/1 ]).

:- include(chat80(chatops)).

/* Nouns */

property(area,measure&area,X,feature&place&_,Y,area(Y,X),[],_,_).
property(capital,feature&city,X,feature&place&country,Y,
     capital(Y,X),[],_,_).
property(latitude,
     measure&position,X,feature&_,Y,latitude(Y,X),[],_,_).
property(longitude,measure&position,X,feature&_,Y,
     longitude(Y,X),[],_,_).
property(population,
     measure&heads,X,feature&_,Y,population(Y,X),[],_,_).

thing(place,feature&place&_,X,place(X),[],_).
thing(area,measure&area,X,area(X),[],_).
thing(capital,feature&city,X,capital(X),[],_).
thing(city,feature&city,X,city(X),[],_).
thing(continent,feature&place&continent,X,continent(X),[],_).
thing(country,feature&place&country,X,country(X),[],_).
thing(latitude,measure&position,X,latitude(X),[],_).
thing(longitude,measure&position,X,longitude(X),[],_).
thing(ocean,feature&place&seamass,X,ocean(X),[],_).
thing(person,_,X,person(X),[],_).
thing(population,measure&heads,X,population(X),[],_).
thing(region,feature&place&_,X,region(X),[],_).
thing(river,feature&river,X,river(X),[],_).
thing(sea,feature&place&seamass,X,sea(X),[],_).
thing(seamass,feature&place&seamass,X,seamass(X),[],_).

aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun(number,_,V,feature&_,X,P,numberof(X,P,V)).

/* Proper nouns */
% :- pred name_template(+,-).
:- pred name_template(A,B) : nonvar(A) => ground * ground.
name_template(X,feature&circle) :- circle_of_latitude(X).
name_template(X,feature&city) :- city(X).
name_template(X,feature&place&continent) :- continent(X).
name_template(X,feature&place&country) :- country(X).
name_template(X,feature&place&_) :- region(X).
name_template(X,feature&river) :- river(X).
name_template(X,feature&place&seamass) :- seamass(X).

/* Verbs */

trans(border,
      feature&place&_,X,feature&place&_,Y,borders(X,Y),[],_,_).
trans(contain,feature&place&_,X,feature&_,Y,in(Y,X),[],_,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,feature&river,X,drains(X,Y),
   [slot(prep(into),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y),
   [slot(prep(through),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y,Z),
   [slot(prep(into),feature&place&_,Z,_,free),
    slot(prep(from),feature&place&_,Y,_,free)],_).
intrans(rise,feature&river,X,rises(X,Y),
   [slot(prep(in),feature&place&_,Y,_,free)],_).

/* Adjectives */

restriction(african,feature&_,X,african(X)).
restriction(american,feature&_,X,american(X)).
restriction(asian,feature&_,X,asian(X)).
restriction(european,feature&_,X,european(X)).

attribute(large,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(small,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(great,measure&Type,X,measure&Type,Y,X=Y).
attribute(populous,feature&_,X,measure&heads,Y,population(X,Y)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Prepositions */

adjunction(in,feature&_-X,feature&place&_-Y,in(X,Y)).
adjunction(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction(southof,feature&_-X,feature&_-Y,southof(X,Y)).

/* Measure */

measure(ksqmile,measure&area,[],ksqmiles).
measure(sqmile,measure&area,[],sqmiles).
measure(degree,measure&position,[],degrees).
measure(thousand,measure&heads,[],thousand).
measure(million,measure&heads,[],million).

units(large,measure&_).
units(small,measure&_).

%sign(big,+).        % added by PBC: without it (1) loops forever
sign(large,+).
sign(small,-).
sign(great,+).
sign(populous,+).

% (1) which is the biggest country.

/* Proportions and the like */

comparator(proportion,_,V,[],proportion(V)).
comparator(percentage,_,V,[],proportion(V)).
