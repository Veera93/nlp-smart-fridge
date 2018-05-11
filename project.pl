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

lemma(box,n).
lemma(ham,n).
lemma(freezer,n).
lemma(egg,n).
lemma(bowl,n).

lemma(tom,pn).
lemma(mia,pn).
lemma(sue,pn).

lemma(eat,tv).
lemma(contain,tv).
lemma(belong,tv).

lemma(in,p).
lemma(under,p).

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

% Questions
lemma(will,aux).
lemma(did,aux).

lemma(is,be).
lemma(was,be).
lemma(are,be).

lemma(who,whpr1).
lemma(what,whpr2).

lemma(and,coord).
lemma(but,coord).
lemma(or,coord).

lemma(that,rel).
lemma(what,rel).
lemma(who,rel).
lemma(which,rel).
lemma(to,rel).

 
% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------

% lex(n(X^man(X)),man).
lex(n(X^P),Word):-
	member(Suffix,['',s,es]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,n),
	P=.. [Lemma,X].

lex(pn((Word^X)^X),Word):- lemma(Word,pn).
% lex(iv(X^sneezed(X)),sneezed).

lex(iv(X^P),Word):-
	member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,iv),
	P=..[Lemma,X,[]].

%lex(tv(X^Y^saw(X,Y)),saw).
lex(tv(X^Y^P),Word):-
	member(Suffix,['',s,es,ed,ing]),atom_concat(Lemma,Suffix,Word),lemma(Lemma,tv),
	P=..[Lemma,X,Y,[]].

%lex(adj((X^P)^X^and(P,old(X))),old).


lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Word):-
		lemma(Word,dtforall).

lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Word):-
		lemma(Word,dtexists).

lex(dt((X^P)^(X^Q)^the(X,and(P,Q))),Word):-
		lemma(Word,dtthe).				
% ...

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------

% ...

% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

rule(s(Y),[np(X^Y),vp(X)]).

rule(vp(X^W),[tv(X^Y),np(Y^W)]).
rule(vp(X),[iv(X)]).

rule(np(Y),[dt(X^Y),n(X)]).
rule(np(X),[pn(X)]).

rule(n(Y),[adj(X^Y),n(X)]).

rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).

% New rules: need to handle there exists
rule(np(X),[n(X)]).

% ToDo: Add rule for ditransistive

% Question rules: sym sem3
rule(vp(X^K,[]),[tv(X^Y,[]),np(Y^K)]).
rule(vp(X,WH),[iv(X,WH)]).
rule(s(Y,WH),[np(X^Y),vp(X,WH)]).

rule(vp(K,[WH]),[tv(Y,[WH]),np(Y^K)]).
rule(s(X,[WH]),[vp(X,[WH])]).

rule(Y,[whpr(X^Y),vp(X,[])]).
rule(ynq(Y),[aux, np(X^Y),vp(X,[])]).
rule(Z,[whpr((X^Y)^Z), inv_s(Y,[X])]).
rule(inv_s(Y,[WH]),[aux, np(X^Y),vp(X,[WH])]).

rule(n(X^and(Y,Z)),[n(X^Y),rc(X^Z,[])]).
rule(n(X^and(Y,Z)),[n(X^Y),rc(Z,[X])]).
% ...


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