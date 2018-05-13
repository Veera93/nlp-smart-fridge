% ===========================================================
% Main loop:
% 1. Repeat "input-response" cycle until input starts with "bye"
%    Each "input-response" cycle consists of:
% 		1.1 Reading an input string and convert it to a tokenized list
% 		1.2 Processing tokenized list
% ===========================================================

chat:-
 repeat,
   readinput(Input),
   process(Input), 
  (Input = [bye| _] ),!.
  

% ===========================================================
% Read input:
% 1. Read char string from keyboard. 
% 2. Convert char string to atom char list.
% 3. Convert char list to lower case.
% 4. Tokenize (based on spaces).
% ===========================================================

readinput(TokenList):-
   read_line_to_codes(user_input,InputString),
   string_to_atom(InputString,CharList),
   string_lower(CharList,LoweredCharList),
   tokenize_atom(LoweredCharList,TokenList).


% ===========================================================
%  Process tokenized input
% 1. Parse morphology and syntax, to obtain semantic representation
% 2. Evaluate input in the model
% If input starts with "bye" terminate.
% ===========================================================

process(Input):-
	parse(Input,SemanticRepresentation),
	modelchecker(SemanticRepresentation,Evaluation),
	respond(Evaluation),!,
	nl,nl.
	
process([bye|_]):-
   write('> bye!').


% ===========================================================
%  Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

parse(Input, SemanticRepresentation):-
        srparse([],Input, SemanticRepresentation).
 
srparse([X],[], X).
srparse([X],[], X):-
	numbervars(X,0,_).

srparse([Y,X|MoreStack],Words,SemanticRepresentation):-
       rule(LHS,[X,Y]),
       srparse([LHS|MoreStack],Words,SemanticRepresentation).

srparse([Z,Y,X|MoreStack],Words,SemanticRepresentation):-
       rule(LHS,[X,Y,Z]),
       srparse([LHS|MoreStack],Words,SemanticRepresentation).

srparse([X|MoreStack],Words,SemanticRepresentation):-
       rule(LHS,[X]),
       srparse([LHS|MoreStack],Words,SemanticRepresentation).

srparse(Stack,[Word|Words],SemanticRepresentation):-
        lex(X,Word),
        srparse([X|Stack],Words,SemanticRepresentation).


% ===========================================================
% Grammar
% 1. List of lemmas
% 2. Lexical items
% 3. Phrasal rules
% ===========================================================

% --------------------------------------------------------------------
% Lemmas are uninflected, except for irregular inflection
% lemma(+Lemma,+Category)
% --------------------------------------------------------------------
lemma(a,dtexists).
lemma(an,dtexists).
lemma(some,dtexists).

lemma(each,dtforall).
lemma(all,dtforall).
lemma(every,dtforall).

lemma(the,dtthe).

lemma(no,dtnot).

lemma(box,n).
lemma(ham,n).
lemma(freezer,n).
lemma(egg,n).
lemma(bowl,n).
lemma(house,n).
lemma(meat,n).
lemma(fruit,n).
lemma(sandwich,n).
lemma(container,n).
lemma(shelf,n).
lemma(banana,n).
lemma(almond,n).
lemma(milk,n).
lemma(shelf,n).
lemma(apple,n).

lemma(tom,pn).
lemma(mia,pn).
lemma(sue,pn).

lemma(eat,tv).
lemma(ate,tv).
lemma(contain,tv).
lemma(like,tv).
lemma(sneeze,tv).
lemma(has,tv).
lemma(drank,tv).
lemma(punch,tv).

lemma(put,dtv).

lemma(belong,pv).
lemma(rely,pv).

lemma(in,p).
lemma(under,p).
lemma(on,p).
lemma(near,p).
lemma(over,p).
lemma(inside,p).

lemma(on,vacp).   
lemma(to,vacp).

lemma(one,num).
lemma(two,num).
lemma(three,num).
lemma(four,num).
lemma(five,num).
lemma(six,num).
lemma(seven,num).
lemma(eight,num).
lemma(nine,num).
lemma(nine,num).
lemma(ten,num).

lemma(blue,adj).
lemma(white,adj).
lemma(yellow,adj).
lemma(red,adj).
lemma(green,adj).
lemma(black,adj).
lemma(happy,adj).
lemma(bottom,adj).
lemma(almond,adj).
lemma(top,adj).
lemma(middle,adj).

% Questions
lemma(will,aux).
lemma(did,aux).
lemma(does,aux).

lemma(is,be).
lemma(was,be).
lemma(are,be).

lemma(who,whpr1).
lemma(what,whpr2).
lemma(which,whpr2).

lemma(and,coord).
lemma(but,coord).
lemma(or,coord).

lemma(that,rel).
lemma(which,rel).
lemma(to,rel).
lemma(who,rel).

 
% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------

% Noun

lex(n(X^P),Word):-
	member(Suffix,['',s,es]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,n),
	P=.. [Lemma,X].

lex(pn((Word^X)^X),Word):- lemma(Word,pn).

% IV With slots

lex(iv(X^P,[]),Word):-
	member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,iv),
	P=..[Lemma,X].

%TV with slots
lex(tv(X^Y^P,[]),Word):-
	member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,tv),
	P=..[Lemma,X,Y].

%Adjective
lex(adj((X^P)^X^and(P,Q)),Lemma):-
	lemma(Lemma,adj),
	Q=..[Lemma,X].

%Preposition
lex(p((Y^R)^Q^(X^P)^and(P,Q)),Lemma):-
	lemma(Lemma,p),
	R=..[Lemma,X,Y].

lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Word):-
		lemma(Word,dtforall).

lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Word):-
		lemma(Word,dtexists).

lex(dt((X^P)^(X^Q)^the(X,and(P,Q))),Word):-
		lemma(Word,dtthe).

%No - determinant
lex(dt((X^P)^(X^Q)^not(X,and(P,Q))),Word):-
		lemma(Word,dtnot).

lex(aux, Word):-
		lemma(Word,aux).
% (WHPR; λP.?x(person(x), P(x))) -> who
lex(whpr((X^P)^exists(X,and(person(X),P))), Word):-
    lemma(Word,whpr1).
% (WHPR; λP.?x(thing(x), P(x))) -> what
lex(whpr((X^P)^exists(X,and(thing(X),P))), Word):-
    lemma(Word,whpr2).

%Lex for PP complement
%(PV; λx.λy.rely(x,y), []) -> rely
lex(pv(X^Y^P,[]),Lemma) :- 
    lemma(Lemma,pv),
	P=..[Lemma,X,Y].
%(P; λP.P, []) -> on | of | to | at | ...
lex(vacp([]), Word) :-
    lemma(Word,vacp).
%(PP; λP.P(X), [x]) -> on | of | to | at | ...
lex(pp(X^_,[X]), Word) :-
    lemma(Word,vacp).

lex(rel, Word):-
		lemma(Word,rel).
lex(p(X^Y^Z),Word):-lemma(Word,p),Z =.. [Word,X,Y].

%DTV
lex(dtv(X^Y^Z^P,[]),Word):-
	member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),
	lemma(Lemma,dtv),
	P =.. [Lemma,X,Y,Z].
% ...

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------

% ...

% --------------forall------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

rule(s(Y),[np(X^Y),vp(X,[])]).

rule(vp(X^W),[tv(X^Y),np(Y^W)]).
rule(vp(X),[iv(X)]).

rule(np(Y),[dt(X^Y),n(X)]).
rule(np(X),[pn(X)]).

rule(n(Y),[adj(X^Y),n(X)]).

rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).

% New rules: Handled there exists
rule(np((X^B)^exists(X,and(Y,B))),[n(X^Y)]).

% Question rules: sym sem3
rule(vp(X^K,[]),[tv(X^Y,[]),np(Y^K)]).
rule(vp(X,WH),[iv(X,WH)]).
rule(s(Y,WH),[np(X^Y),vp(X,WH)]).

rule(vp(K,[WH]),[tv(Y,[WH]),np(Y^K)]).
rule(s(X,[WH]),[vp(X,[WH])]).

rule(q(Y),[whpr(X^Y),vp(X,[])]).
rule(ynq(Y),[aux, s(Y)]).
rule(ynq(Y),[aux, np(X^Y),vp(X,[])]).
rule(q(Z),[whpr((X^Y)^Z), inv_s(Y,[X])]).
rule(inv_s(Y,[WH]),[aux, np(X^Y),vp(X,[WH])]).

rule(n(X^and(Y,Z)),[n(X^Y),rc(X^Z,[])]).
rule(n(X^and(Y,Z)),[n(X^Y),rc(Z,[X])]).

%(RC; φ, []) -> REL (VP; φ, [])
rule(rc(X,[]),[rel,vp(X,[])]).
%(RC; φ, [x]) -> REL (S; φ, [x])
rule(rc(X,[Z]),[rel,s(X,[Z])]).

% (IV; λx.φ, [y]) -> (TV; λx.λy.φ, [ ])
rule(iv(X^P,[Y]),[tv(X^Y^P,[])]).
% (TV; λy.φ, [x]) -> (TV; λx.λy.φ, [ ])
rule(tv(Y^P,[X]),[tv(X^Y^P,[])]).

% (VP;X^Y) -> (PV;Y)(PP;X))
rule(vp(X^Y,[WH]),[pv(X^Z,[]),pp(Z^Y,[WH])]).

% DTV
rule(vp(X^A,[]),[dtv(X^Y^Z^W,[]),np((Y^B)^A),np((Z^W)^B)]).
rule((np(X)),[vacp([]),np(X)]).

% Piazza post
rule(pp(X^Y),[p(X^Z),np(Z^Y)]).


% ===========================================================
%  Modelchecker:
%  1. If input is a declarative, check if true
%  2. If input is a yes-no question, check if true
%  3. If input is a content question, find answer
% ===========================================================

% model(...,...)

% ===========================================================
%  Respond
%  For each input type, react appropriately.
% ===========================================================

% Declarative true in the model
respond(Evaluation) :- 
		Evaluation = [true_in_the_model], 
		write('That is correct'),!.

% Declarative false in the model
respond(Evaluation) :- 
		Evaluation = [not_true_in_the_model],  
		write('That is not correct'),!.

% Yes-No interrogative true in the model
respond(Evaluation) :- 
		Evaluation = [yes_to_question],			
		write('yes').

% Yes-No interrogative false in the model		
respond(Evaluation) :- 
		Evaluation = [no_to_question], 			
		write('no').

% wh-interrogative true in the model
% ...							

% wh-interrogative false in the model
% ...							

% ===========================================================
% Helper Functions
% ===========================================================
%check_tv(Word,Lemma):- member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,tv).
% parse([a,blue,box,contains,some,ham], X)
% parse([a,blue,box,contains,ham], X)
% parse([does,the,sandwich,contain,no,meat], X)
% parse([has,no,meat], X)
% parse([every, white, container, on, the, bottom, shelf, contains, a, banana], X)
% parse([the,white,box,in,the,freezer,contains,ham],X)
% parse([who,drank,the,almond,milk], X)
% parse([tom,ate,an,apple],X).
% parse([what,did,tom,eat],X).
% parse([what,does,the,green,box,contain],X).
% parse([what, does, the, green, box, on, the, top, shelf, contain],X).
% parse([every, blue, container, on, the, top, shelf, contains, a, sandwich, that, has, no, meat],X).
% parse([what, does, the, yellow, bowl, on, the, middle, shelf, contain],X).
% parse([who, drank, the, almond, milk],X).
% parse([who,put,every,yellow,box,on,the,white,bowl],X).
% 
% parse([the,white,box,that,the,freezer,contains,belongs,to,sue],X)
% parse([is,there,a,sandwich,contain,no,meat], X)
% Are there two watermelons in the fridge?
% Is there milk?
% parse([which,milk,did,sam,drink], X).
% 
% parse([is,there,an,egg,inside,the,blue,box], X)
% Is there a sandwich that does not contain meat?
% Is there an empty box of popsicles in the freezer?
% parse([are,there,two,eggs,inside,the,blue,box],X).


