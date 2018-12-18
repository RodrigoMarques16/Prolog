:-consult(parser).

%% text2poly(+Str, -Poly)
%
% Takes a polynom in text form and coverts it
% to a polynom made of terms.
%
text2poly(Str, Poly) :-
    split_string(Str, " ", "", List),
    poly(Poly, List, []), 
    !.

% polyplay
%
% Start the interactive demo
%
polyplay :- 
    prompt,
    readInput(Input),
    testInput(Input), 
    !.

prompt :- writeln('Type "help" or "commands" (without the quotes) for a list of commands').

% readInput(-List)
%
% Reads a string from user_input and converts
% it to a list of words
%
readInput(List) :-
    nl, write("> "), flush_output,
    read_line_to_codes(user_input, Codes), 
    string_codes(Input, Codes), 
    split_string(Input, " ", "", List).

% testInput(+Input)
%
% If input is an exit command, writes a farewell and the demo ends.
% If input is parseable, executes the given command
% If input is neither of the above, writes an error message
%
testInput(["exit"]) :- writeBye.
testInput(["quit"]) :- writeBye.
testInput(["bye"])  :- writeBye.

testInput(Input)  :- 
    phrase(parse(Result), Input, []),
    writeln(Result),
    polyplay.

testInput(_) :- 
    writeError, 
    polyplay.

% Write predicates
writeBye   :- writeln("See ya").
writeError :- writeln("Error!").

