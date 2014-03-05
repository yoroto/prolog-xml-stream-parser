
:- module( xmlstream, [
        start_parsing/1,
        end_parsing/0,
        next_start_before_end_of/3,
        next_start_or_singleton_before_end_of/3,
        next_singleton_before_end_of/3,
        next_text_element_before_end_of/3,
        next_end/1,
        next/1
        ]).

:- dynamic lt_got/0,
           element_stack/1,
           current_stream/1.

start_parsing(FileName) :-
        retractall(lt_got),
        retractall(element_stack(_)),
        retractall(current_stream(_)),

        catch(  open( FileName, read, Fd, [type(text),encoding('UTF-8')] ),
                Err,
                (Err = error(existence_error(source_sink, FileName ), _)
                ->
                        throw( inexistence( file( FileName), does_not_exist ) )
                ;
                        throw( error( file_failure( FileName, prolog_error( Err ) ) ) )
                ) 
             ),
        current_input(Current),
        assert(current_stream(Current)),
        assert(element_stack([])),
        set_input(Fd).

end_parsing :-
        current_input(Fd),
        (ground(Fd)
        ->
                (catch( close(Fd), _, true)
                ->
                        true
                ;
                        true
                )
        ;
                true
        ),
        current_stream(Current),
        set_input(Current).

next_start_before_end_of(StartName, EndName, Start) :-
        next(Tag), !,
        (Tag = start(StartName, _Attr)
        ->
                Start = Tag
        ;Tag = end(EndName)
        ->
                fail
        ;
                next_start_before_end_of(StartName, EndName, Start)
        ).

next_start_or_singleton_before_end_of(Name, EndName, Item) :-
        next(Tag), !,
        ((Tag = start(Name, _Attr)
         ;Tag = singleton(Name, _Attr)
         )
        ->
                Item = Tag
        ;Tag = end(EndName)
        ->
                fail
        ;
                next_start_or_singleton_before_end_of(Name, EndName, Item)
        ).

next_singleton_before_end_of(Name, EndName, Singleton) :-
        next(Tag), !,
        (Tag = singleton(Name, Attributes)
        ->
                Singleton = element(Name, Attributes, [])
        ;Tag = end(EndName)
        ->
                fail
        ;
                next_singleton_before_end_of(Name, EndName, Singleton)
        ).

next_text_element_before_end_of(Name, EndName, Element) :-
        next(Tag), !,
        (Tag = start(Name, Attributes)
        ->
                get_element_content(Name, Content),
                Element = element(Name, Attributes, Content)
        ;Tag = singleton(Name, Attributes)
        ->
                Element = element(Name, Attributes, [])
        ;Tag = end(EndName)
        ->
                fail
        ;
                next_text_element_before_end_of(Name, EndName, Element)
        ).

next_end(Name) :-
        next(Tag), !,
        (Tag = end(Name)
        ;next_end(Name)
        ).

get_element_content(Name, Content) :-
        next(Tag),
        get_element_content(Tag, Name, Content).

get_element_content(pcdata(Content), Name, [pcdata(Content)]) :-
        !, next(end(Name)).

get_element_content(cdata(Content), Name, [cdata(Content)]) :-
        !, next(end(Name)).

get_element_content(end(Name), Name, []).

add_to_stack(Item) :-
        retract(element_stack([H|T])),
        ((H = cdata; H = pcdata)
        ->
                fail
        ;
                assert(element_stack([Item|[H|T]]))
        ).

check_stack(start(Name, _Attrs)) :-
        add_to_stack(Name).

check_stack(cdata(_)) :-
        add_to_stack(cdata).

check_stack(pcdata(_)) :-
        add_to_stack(pcdata).

check_stack(end(Name)) :-
        retract(element_stack([H|T])),
        (H = Name
        ->
                assert(element_stack(T))
        ;(H = cdata; H = pcdata)
        ->
                T = [Name|TT],
                assert(element_stack(TT))
        ).

check_stack(_Other) :- true.

next_token(Token) :-
        (lt_got
        ->
                retract(lt_got),
                next_tag(Token)
        ;
                get_code(Code),
                next_token(Code, Token)
        ).

next_token(-1, _Token) :- !, fail.

next_token(60, Token) :-
        next_tag(Token).

next_token(Code, Token) :- 
        (Code =< 32
        ->
                get_code(NewCode),
                next_token(NewCode, Token)
        ;
                next_text(Code, Token)
        ).


next_tag(tag([60|Codes])) :-
        get_code(Code),
        next_tag(Code, Codes).

next_tag(-1, _) :- !, fail.

next_tag(62, [62]) :- !.

next_tag(H, [H|T]) :-
        get_code(Code),!,
        next_tag(Code, T).

next_text(Code, text([Code|Codes])) :-
        get_code(NewCode),
        next_text_inner(NewCode, Codes).

next_text_inner(-1, _) :- fail, !.

next_text_inner(60, []) :- assert(lt_got), !.

next_text_inner(H, [H|T]) :-
        get_code(Code),!,
        next_text_inner(Code, T).



next(Item) :-
        next_token(Token),
        parse_token(Token, Item),
        check_stack(Item).

parse_token(text(Codes), pcdata(PCData)) :-
        p_chars(PCData, Codes, []).

parse_token(tag(Codes), Thing) :-
        p_tag(Thing, Codes, []).

p_tag(cdata(CData)) -->
        "<![CDATA[", p_chars(CData), "]]>".

p_tag(comment(Comment)) -->
        "<!--", p_chars(Comment), "-->".

p_tag(instruction(Attributes)) -->
        "<?", p_name(xml), p_attributes(Attributes), p_spaces, "?>".

p_tag(doctype(DocType)) -->
        "<!DOCTYPE ", p_chars(DocType), ">".

p_tag(end(Name)) -->
        "</", p_name(Name), ">".

p_tag(singleton(Name, Attributes)) -->
        "<", p_name(Name), p_attributes(Attributes), p_spaces, "/>".

p_tag(start(Name, Attributes)) -->
        "<", p_name(Name), p_attributes(Attributes), p_spaces, ">".

p_attributes([]) --> [].

p_attributes([Name=Value|Attributes]) -->
        p_spaces, p_name(Name), "=""", p_chars(Value),"""",
        p_attributes(Attributes).

p_spaces( [], [] ).
p_spaces( [Char|Chars0], Chars1 ) :-
        ( Char =< 32 ->
                p_spaces( Chars0, Chars1 )
        ; otherwise ->
                Chars1 = [Char|Chars0]
        ).

p_codes(Codes, Plus, Minus) :-
        append(Codes, Minus, Plus).

p_chars( [] ) --> "".

p_chars( [Char|Chars] ) -->
        "&", p_escape(Char),
        p_chars( Chars ).

p_chars( [Char|Chars] ) -->
        [Char],
        p_chars( Chars ).

p_escape(38) --> "amp;".
p_escape(39) --> "apos;".
p_escape(62) --> "gt;".
p_escape(60) --> "lt;".
p_escape(34) --> "quot;".
p_escape(Code) -->
        "#",
        ("x",
                escape_hex(Code)
        ;p_chars(Codes), ";", {number_codes(Code, Codes)}
        ).

escape_hex( Code ) -->
        escape_hex1( 0, Code ).

escape_hex1( Current, Code ) -->
        hex_digit_char( Value ),
        !,
        {New is (Current << 4) + Value},
        escape_hex1( New, Code ).
escape_hex1( Code, Code ) --> ";".

hex_digit_char( 0 ) --> "0".
hex_digit_char( 1 ) --> "1".
hex_digit_char( 2 ) --> "2".
hex_digit_char( 3 ) --> "3".
hex_digit_char( 4 ) --> "4".
hex_digit_char( 5 ) --> "5".
hex_digit_char( 6 ) --> "6".
hex_digit_char( 7 ) --> "7".
hex_digit_char( 8 ) --> "8".
hex_digit_char( 9 ) --> "9".
hex_digit_char( 10 ) --> "A".
hex_digit_char( 11 ) --> "B".
hex_digit_char( 12 ) --> "C".
hex_digit_char( 13 ) --> "D".
hex_digit_char( 14 ) --> "E".
hex_digit_char( 15 ) --> "F".
hex_digit_char( 10 ) --> "a".
hex_digit_char( 11 ) --> "b".
hex_digit_char( 12 ) --> "c".
hex_digit_char( 13 ) --> "d".
hex_digit_char( 14 ) --> "e".
hex_digit_char( 15 ) --> "f".

p_name( Name ) -->
        nmtoken_chars( Chars ),
        {atom_codes(Name, Chars)}.

nmtoken_chars( [Char|Chars] ) -->
        [Char],
        {nmtoken_first( Char )},
        nmtoken_chars_tail( Chars ).

nmtoken_chars_tail( [Char|Chars] ) -->
        [Char],
        {nmtoken_char(Char)},
        !,
        nmtoken_chars_tail( Chars ).
nmtoken_chars_tail([]) --> "".

nmtoken_first( 0': ).
nmtoken_first( 0'_ ).
nmtoken_first( Char ) :-
        alphabet( Char ).

nmtoken_char( 0'a ).
nmtoken_char( 0'b ).
nmtoken_char( 0'c ).
nmtoken_char( 0'd ).
nmtoken_char( 0'e ).
nmtoken_char( 0'f ).
nmtoken_char( 0'g ).
nmtoken_char( 0'h ).
nmtoken_char( 0'i ).
nmtoken_char( 0'j ).
nmtoken_char( 0'k ).
nmtoken_char( 0'l ).
nmtoken_char( 0'm ).
nmtoken_char( 0'n ).
nmtoken_char( 0'o ).
nmtoken_char( 0'p ).
nmtoken_char( 0'q ).
nmtoken_char( 0'r ).
nmtoken_char( 0's ).
nmtoken_char( 0't ).
nmtoken_char( 0'u ).
nmtoken_char( 0'v ).
nmtoken_char( 0'w ).
nmtoken_char( 0'x ).
nmtoken_char( 0'y ).
nmtoken_char( 0'z ).
nmtoken_char( 0'A ).
nmtoken_char( 0'B ).
nmtoken_char( 0'C ).
nmtoken_char( 0'D ).
nmtoken_char( 0'E ).
nmtoken_char( 0'F ).
nmtoken_char( 0'G ).
nmtoken_char( 0'H ).
nmtoken_char( 0'I ).
nmtoken_char( 0'J ).
nmtoken_char( 0'K ).
nmtoken_char( 0'L ).
nmtoken_char( 0'M ).
nmtoken_char( 0'N ).
nmtoken_char( 0'O ).
nmtoken_char( 0'P ).
nmtoken_char( 0'Q ).
nmtoken_char( 0'R ).
nmtoken_char( 0'S ).
nmtoken_char( 0'T ).
nmtoken_char( 0'U ).
nmtoken_char( 0'V ).
nmtoken_char( 0'W ).
nmtoken_char( 0'X ).
nmtoken_char( 0'Y ).
nmtoken_char( 0'Z ).
nmtoken_char( 0'0 ).
nmtoken_char( 0'1 ).
nmtoken_char( 0'2 ).
nmtoken_char( 0'3 ).
nmtoken_char( 0'4 ).
nmtoken_char( 0'5 ).
nmtoken_char( 0'6 ).
nmtoken_char( 0'7 ).
nmtoken_char( 0'8 ).
nmtoken_char( 0'9 ).
nmtoken_char( 0'. ).
nmtoken_char( 0'- ).
nmtoken_char( 0'_ ).
nmtoken_char( 0': ).

alphabet( 0'a ).
alphabet( 0'b ).
alphabet( 0'c ).
alphabet( 0'd ).
alphabet( 0'e ).
alphabet( 0'f ).
alphabet( 0'g ).
alphabet( 0'h ).
alphabet( 0'i ).
alphabet( 0'j ).
alphabet( 0'k ).
alphabet( 0'l ).
alphabet( 0'm ).
alphabet( 0'n ).
alphabet( 0'o ).
alphabet( 0'p ).
alphabet( 0'q ).
alphabet( 0'r ).
alphabet( 0's ).
alphabet( 0't ).
alphabet( 0'u ).
alphabet( 0'v ).
alphabet( 0'w ).
alphabet( 0'x ).
alphabet( 0'y ).
alphabet( 0'z ).
alphabet( 0'A ).
alphabet( 0'B ).
alphabet( 0'C ).
alphabet( 0'D ).
alphabet( 0'E ).
alphabet( 0'F ).
alphabet( 0'G ).
alphabet( 0'H ).
alphabet( 0'I ).
alphabet( 0'J ).
alphabet( 0'K ).
alphabet( 0'L ).
alphabet( 0'M ).
alphabet( 0'N ).
alphabet( 0'O ).
alphabet( 0'P ).
alphabet( 0'Q ).
alphabet( 0'R ).
alphabet( 0'S ).
alphabet( 0'T ).
alphabet( 0'U ).
alphabet( 0'V ).
alphabet( 0'W ).
alphabet( 0'X ).
alphabet( 0'Y ).
alphabet( 0'Z ).