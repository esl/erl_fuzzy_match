%%%-------------------------------------------------------------------
%%% Copyright (C) 2015, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Robby Raschke <robby.raschke@erlang-solutions.com>
%%% @doc
%%% Fuzzy Name Matching Library.
%%% Inspired by http://www.sportshacker.net/posts/fuzzy_string_matching.html
%%% @end
%%% Created: 9 Feb 2015 by Robby Raschke <erlang-solutions.com>
%%%-------------------------------------------------------------------

-module(erl_fuzzy_match).

-behaviour(gen_server).

-export([start_link/1, start_link/3, start_link/4, stop/1,
        translate/2, translate/3,
        matchers/1, dict/1, canonicals/1]).

% The matchers
-export([exact/2, abbreviation/2, levenshtein/2, tokens/2]).

% Export the gen_server callback functions.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).


-record(state, {
    fixed = false,  % Extend the map and canonicals (false) or not (true)
    lookup,         % Map from names to canonical names
    canonicals      % Set of all known canonical names
}).


% Starting and stopping

start_link(Name) ->
    start_link(Name, dict:new(), [], []).

start_link(Name, Dict, Canon) ->
    start_link(Name, Dict, Canon, []).

start_link(Name, Dict, Canon, Options) ->
    Fixed = case Options of
        [] -> false;
        [fixed] -> true
    end,
    % All canonicals map to themselves.
    All_Dict = lists:foldl(
        fun (S, Dct) -> dict:store(S, S, Dct) end,
        Dict,
        Canon
    ),
    % Final canonicals are all the values in the dictionary.
    All_Canon = dict:fold(
        fun (_K, V, Set) -> ordsets:add_element(V, Set) end,
        ordsets:new(),
        All_Dict
    ),
    State = #state{
        fixed = Fixed,
        lookup = All_Dict,
        canonicals = All_Canon
    },
    gen_server:start_link({local, Name}, ?MODULE, State, []).

-spec stop(atom() | pid()) -> 'ok'.
stop(Name) ->
    gen_server:cast(Name, stop).


% API

-spec translate(atom() | pid(), binary()) -> binary().
translate(Name, S) ->
    gen_server:call(Name, {translate, S}, infinity).

-spec translate(atom() | pid(), binary(), [fun()]) -> binary().
translate(Name, S, Matchers) ->
    gen_server:call(Name, {translate, S, Matchers}, infinity).

-spec matchers(atom() | pid()) -> [atom()].
matchers(Name) ->
    gen_server:call(Name, matchers, infinity).

-spec dict(atom() | pid()) -> dict:dict().
dict(Name) ->
    gen_server:call(Name, dict, infinity).

-spec canonicals(atom() | pid()) -> [binary()].
canonicals(Name) ->
    gen_server:call(Name, canonicals, infinity).


% Gen_server callbacks

init(#state{} = State) ->
    {ok, State}.

handle_call({translate, S}, _From, #state{} = State) ->
    {T, New_State} = match(S, run_matchers(), State),
    {reply, T, New_State};
handle_call({translate, S, Matchers}, _From, #state{} = State) ->
    {T, New_State} = match(S, Matchers, State),
    {reply, T, New_State};
handle_call(matchers, _From, #state{} = State) ->
    {reply, defined_matchers(), State};
handle_call(dict, _From, #state{lookup = Dict} = State) ->
    {reply, Dict, State};
handle_call(canonicals, _From, #state{canonicals = Canon} = State) ->
    {reply, Canon, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.


% API implementation details

-spec match(Subject :: binary(), Matchers :: [fun()], #state{}) -> {binary(), #state{}}.
match(S, Matchers, #state{lookup = Dict} = State) ->
    case dict:find(S, Dict) of
        {ok, T} ->
            {T, State};
        error ->
            match2(S, Matchers, State)
    end.

-spec match2(Subject :: binary(), Matchers :: [fun()], #state{}) -> {binary(), #state{}}.
match2(S, [], State) ->
    return(S, S, State);
match2(S, [Matcher | Rest], #state{canonicals = Canon} = State) ->
    case Matcher(S, Canon) of
        {ok, T} ->
            return(S, T, State);
        undefined ->
            match2(S, Rest, State)
    end.

-spec return(Subject :: binary(), Result :: binary(), #state{}) -> {binary(), #state{}}.
return(_, T, #state{fixed = true} = State) ->
    {T, State};
return(S, T, #state{lookup = Dict, canonicals = Canon} = State) ->
    New_Dict = dict:store(S, T, Dict),
    New_Canon = ordsets:add_element(T, Canon),
    {T, State#state{lookup = New_Dict, canonicals = New_Canon}}.


-spec defined_matchers() -> [fun()].
defined_matchers() -> [
    fun exact/2,
    fun abbreviation/2,
    fun levenshtein/2,
    fun tokens/2
].

% We do not need to actually run the exact matcher, as the builtin
% dictionary already takes care of that.
-spec run_matchers() -> [fun()].
run_matchers() -> [
    fun abbreviation/2,
    fun levenshtein/2,
    fun tokens/2
].


% The match algorithm implementations

-spec exact(Subject :: binary(), Canonicals :: [binary()]) -> 'undefined' | {'ok', binary()}.
exact(S, Canon) ->
    case lists:member(S, Canon) of
        true -> {ok, S};
        false -> undefined
    end.

-spec abbreviation(Subject :: binary(), Canonicals :: [binary()]) -> 'undefined' | {'ok', binary()}.
abbreviation(_S, []) ->
    undefined;
abbreviation(S, [Name | Rest]) ->
    case is_abbreviation(S, Name) orelse is_abbreviation(Name, S) of
        true ->
            {ok, Name};
        false ->
            abbreviation(S, Rest)
    end.

-spec levenshtein(Subject :: binary(), Canonicals :: [binary()]) -> 'undefined' | {'ok', binary()}.
levenshtein(_S, []) ->
    undefined;
levenshtein(S, [Name | Rest]) ->
    case levenshtein_distance(S, Name) =< 2 of
        true ->
            {ok, Name};
        false ->
            levenshtein(S, Rest)
    end.

-spec tokens(Subject :: binary(), Canonicals :: [binary()]) -> 'undefined' | {'ok', binary()}.
tokens(_S, []) ->
    undefined;
tokens(S, [Name | Rest]) ->
    case lists:any(
                fun ({A, B}) -> A == B orelse levenshtein_distance(A, B) =< 2 end,
                [ {A, B} || A <- split(S), B <- split(Name) ]
            ) of
        true -> 
            {ok, Name};
        false ->
            tokens(S, Rest)
    end.


% The match algorithm implementation details

% Abbreviations:
%    The first letter of the abbreviation must match the first letter of the text
%    The rest of the abbreviation (the abbrev minus the first letter) must be an abbreviation for:
%        the remaining words, or
%        the remaining text starting from any position in the first word.
% Taken from http://stackoverflow.com/questions/7331462/check-if-a-string-is-a-possible-abbrevation-for-a-name

-spec is_abbreviation(Subject :: binary(), Text :: binary()) -> boolean().
is_abbreviation(<<>>, _Text) ->
    true;
is_abbreviation(<<C:8, Abv/binary>> = _Abbrev, <<C:8, Txt/binary>> = Text) ->
    [<<_:8, Fst/binary>> | Rest] = split(Text),
    is_abbreviation(Abv, join(Rest, <<" ">>)) orelse
        lists:any(
            fun (S) -> is_abbreviation(Abv, S) end, 
            postfixes(Fst, Txt, [])
        );
is_abbreviation(_Abbrev, _Text) ->
    false.


% Levenshtein Distance:
% Taken from http://rosettacode.org/wiki/Levenshtein_distance#Erlang

-spec levenshtein_distance(Subject :: binary(),  Text :: binary()) -> integer().
levenshtein_distance(S, T) ->
    {L,_} = ld(S, T, dict:new()),
    L.

-spec ld(binary(), binary(), dict:dict()) -> {integer(), dict:dict()}.
ld(<<>> = S, T, Cache) ->
    {byte_size(T), dict:store({S,T}, byte_size(T), Cache)};
ld(S, <<>> = T, Cache) ->
    {byte_size(S), dict:store({S,T}, byte_size(S), Cache)};
ld(<<X:8, S/binary>>, <<X:8, T/binary>>, Cache) ->
    ld(S, T, Cache);
ld(<<_SH:8, ST/binary>> = S, <<_TH:8, TT/binary>> = T, Cache) ->
    case dict:is_key({S,T}, Cache) of
        true -> {dict:fetch({S,T}, Cache), Cache};
        false ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, dict:store({S,T}, L, C3)}
    end.


% Helper functions

ws() -> [<<" ">>, <<"\t">>, <<"\r\n">>, <<"\n">>, <<"\r">>, <<"\f">>].

split(B) ->
    lists:filter(
        fun (<<>>) -> false; (_) -> true end,
        binary:split(B, ws(), [global])
    ).

-spec join(Strings :: [binary()], Separator :: binary()) -> binary().
join([], _) ->
    <<>>;
join([S], _) when is_binary(S) ->
    S;
join([H | T], Sep) ->
    B = << <<Sep/binary, X/binary>> || X <- T >>,
    <<H/binary, B/binary>>.

-spec postfixes(Prefix :: binary(), Text :: binary(), Postfixes:: [binary()]) -> [binary()].
postfixes(<<>>, Text, Acc) ->
    lists:reverse([Text | Acc]);
postfixes(<<_:8, Fst/binary>>, <<_:8, Txt/binary>> = Text, Acc) ->
    postfixes(Fst, Txt, [Text | Acc]).

