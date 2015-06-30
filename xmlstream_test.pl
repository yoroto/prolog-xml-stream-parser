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