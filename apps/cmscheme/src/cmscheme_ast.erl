-module(cmscheme_ast).
-export([
         call/1,
         call/2,
         literal/1,
         def/2,
         def/3,
         sym/1,
         str/1,
         switch/2,
         match/2
        ]).

def(Name, Expr) -> 
    def(Name, [], Expr).

def(Name, Args, Expr) -> 
    #{ type => define,
       name => Name,
       args => Args,
       expression => Expr }.

call(Name) ->
    call(Name, []).

call(Name, Args) ->
    #{ type => call,
       name => Name,
       args => Args }.

literal(Name) ->
    #{ type => literal,
       value => Name }.

str(Name) ->
    #{ type => string,
       value => Name }.

sym(Name) ->
    #{ type => symbol,
       value => Name }.

switch(Test, Cases) ->
    #{ type => switch,
       test => Test,
       match => Cases }.

match(Value, Expr) ->
    #{ type => match,
       value => Value,
       expr => Expr }.
