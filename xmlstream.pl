
:- module( xmlstream, [
        has_next/0,
        next/1
        ]).

peek_visible(Code) :-
        peek_code(Code0),
        peek_visible(Code0, Code).

peek_visible(-1, -1) :- !.

peek_visible(Code, Code) :- Code > 32.

peek_visible(_Code, NewCode) :-
        get_code(_Code),
        peek_code(Code),
        peek_visible(Code, NewCode).

get_visible(Code) :-
        get_code(Code0),
        get_visible(Code0, Code).

get_visible(Code, Code) :- Code > 32.

get_visible(_Code, NewCode) :-
        get_code(Code),
        get_visible(Code, NewCode).

has_next :-
        peek_visible(Code),
        Code =\= -1.

next_token(Token) :-
        peek_visible(Code),
        (Code == 60
        ->
                next_tag(Token)
        ;
                next_text(Token)
        ).

next_tag(tag(Codes)) :-
        get_code(Code),
        next_tag(Code, Codes).

next_tag(-1, _) :- fail, !.

next_tag(62, [62]).

next_tag(H, [H|T]) :-
        get_code(Code),
        next_tag(Code, T).

next_text(text(Codes)) :-
        peek_code(Code),
        next_text(Code, Codes).

next_text(-1, _) :- fail, !.

next_text(60, []).

next_text(H, [H|T]) :-
        get_code(H),
        peek_code(Code),
        next_text(Code, T).

next(Thing) :-
        next_token(Token),
        parse_token(Token, Thing).

parse_token(text(Codes), pcdata(Codes)). %TODO

parse_token(tag(Codes), Thing) :-
        p_tag(Thing, Codes, []).

p_tag(cdata(CData)) -->
        "<![CDATA[", chars(CData), "]]>".

p_tag(comment(Comment)) -->
        "<!--", chars(Comment), "-->".

p_tag(instructions(Instructions)) -->
        "<?", chars(Instructions),"?>".

p_tag(doctype(DocType)) -->
        "<!DOCTYPE ", chars(DocType), ">".

p_tag(end(Name)) -->
        "</", chars(Name), ">".

p_tag(singleton(Name)) -->
        "<", chars(Name), "/>".

p_tag(start(Name, [])) -->
        "<", chars(Name), ">".

chars( Chars, Plus, Minus ) :-
        append( Chars, Minus, Plus ).