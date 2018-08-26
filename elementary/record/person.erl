-module(person).

-export([ahmad/0]).
-export([new/3]).

-record(person, {name, age, phone}).

new(Name, Age, Phone) ->
    #person{name=Name, age=Age, phone=Phone}.

ahmad() ->
    #person{name="Ahmad"}.
