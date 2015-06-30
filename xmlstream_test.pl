:- use_module(xmlstream).

test1(FileName) :-
        current_input(Current),
        open(FileName, read, FD, [type(text),encoding('UTF-8')]),
        set_input(FD),
        print_token,
        set_input(Current),
        close(FD).

print_token :-
        (next(Token) ->
                print(Token),nl,
                print_token
        ;
                true
        ).

test_sbeo(FileName, Tests) :-
    start_parsing(FileName),
    test_sbeo_inner(Tests),
    end_parsing.

test_sbeo_inner([]).

test_sbeo_inner([(Start, End)|T]) :-
    next_start_before_end_of(Start, End, start(Start, Properties)),
    print(start(Start, Properties)),
    test_sbeo_inner(T). 