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
%%% Test suite
%%% @end
%%% Created: 9 Feb 2015 by Robby Raschke <erlang-solutions.com>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% IMPORTANT! This file is in UTF-8 format!
%%%-------------------------------------------------------------------

-module(fuzzy_match_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


all() -> [unique, exact, abbrev, levenshtein, tokens, alternatives].


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(alternatives, Config) ->
    % For this test we want to record the mappings of "new" names to canonicals.
    {ok, _} = erl_fuzzy_match:start_link(fuzzy_test, dict:new(), names()),
    Config;
init_per_testcase(_TestCase, Config) ->
    % For the other tests, we run with a fixed set of canonicals and dictionary.
    {ok, _} = erl_fuzzy_match:start_link(fuzzy_test, dict:new(), names(), [fixed]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = erl_fuzzy_match:stop(fuzzy_test),
    ok.


unique(_Config) ->
    [<<"A Bilbao">>, <<"Almeria">>, <<"Almería">>, <<"Athletic Bilbao">>,
     <<"Athletic Club">>, <<"Atlético">>, <<"Atlético Madrid">>,
     <<"Barcelona">>, <<"Celta Vigo">>, <<"Celta de Vigo">>,
     <<"Elche">>, <<"Elche CF">>, <<"Espanyol">>, <<"FC Barcelona">>,
     <<"Getafe">>, <<"Getafe CF">>, <<"Granada">>,
     <<"Granada CF">>, <<"Levante">>, <<"Levante UD">>, <<"Malaga">>,
     <<"Málaga">>, <<"Málaga CF">>, <<"Osasuna">>, <<"RCD Espanyol">>,
     <<"Rayo Vallecano">>, <<"Real Betis">>, <<"Real Madrid">>,
     <<"Real Sociedad">>, <<"Real Valladolid">>, <<"Sevilla">>,
     <<"Sevilla FC">>, <<"UD Almería">>, <<"Valencia">>, <<"Valencia CF">>,
     <<"Valladolid">>, <<"Villarreal">>, <<"Villarreal CF">>]
        = queries().

exact(_Config) ->
    {_Matches, Unmatched} = match(
            [fun erl_fuzzy_match:exact/2]),
    [<<"A Bilbao">>, <<"Almería">>, <<"Athletic Club">>,
            <<"Atlético">>, <<"Atlético Madrid">>, <<"Celta de Vigo">>,
            <<"Elche CF">>, <<"FC Barcelona">>, <<"Getafe CF">>,
            <<"Granada CF">>, <<"Levante UD">>, <<"Málaga">>,
            <<"Málaga CF">>, <<"RCD Espanyol">>, <<"Sevilla">>,
            <<"UD Almería">>, <<"Valencia CF">>, <<"Valladolid">>,
            <<"Villarreal CF">>] = Unmatched.

abbrev(_Config) ->
    {_Matches, Unmatched} = match(
            [fun erl_fuzzy_match:exact/2, fun erl_fuzzy_match:abbreviation/2]),
    [<<"Almería">>, <<"Athletic Club">>,
            <<"Atlético">>, <<"Atlético Madrid">>,
            <<"FC Barcelona">>, <<"Málaga">>,
            <<"Málaga CF">>, <<"RCD Espanyol">>,
            <<"UD Almería">>, <<"Valladolid">>] = Unmatched.

levenshtein(_Config) ->
    {_Matches, Unmatched} = match(
            [fun erl_fuzzy_match:exact/2, fun erl_fuzzy_match:abbreviation/2,
             fun erl_fuzzy_match:levenshtein/2]),
    [<<"Athletic Club">>, <<"Atlético">>, <<"FC Barcelona">>,
            <<"Málaga CF">>, <<"RCD Espanyol">>,
            <<"UD Almería">>, <<"Valladolid">>] = Unmatched.

tokens(_Config) ->
    {_Matches, Unmatched} = match(
            [fun erl_fuzzy_match:exact/2, fun erl_fuzzy_match:abbreviation/2,
             fun erl_fuzzy_match:levenshtein/2, fun erl_fuzzy_match:tokens/2]),
    [] = Unmatched.

alternatives(_Config) ->
    {_, _} = match(
            [fun erl_fuzzy_match:exact/2, fun erl_fuzzy_match:abbreviation/2,
             fun erl_fuzzy_match:levenshtein/2, fun erl_fuzzy_match:tokens/2]),
    Dict = erl_fuzzy_match:dict(fuzzy_test),
    Groups = dict:fold(fun
        (S, S, Acc) ->
            Acc;
        (Key, Val, Acc) ->
            case orddict:is_key(Val, Acc) of
                false -> orddict:store(Val, [Key], Acc);
                true -> orddict:append(Val, Key, Acc)
            end
        end,
        orddict:new(),
        Dict
    ),
    Alternatives = orddict:fold(
        fun (Key, Val, Acc) ->
            Sorted = lists:sort(Val),
            Acc ++ [{Key, Sorted}]
        end,
        [],
        Groups
    ),
    [
        {<<"Almeria">>, [<<"Almería">>, <<"UD Almería">>]},
        {<<"Athletic Bilbao">>, [<<"A Bilbao">>, <<"Athletic Club">>]},
        {<<"Atletico Madrid">>, [<<"Atlético">>, <<"Atlético Madrid">>]},
        {<<"Barcelona">>, [<<"FC Barcelona">>]},
        {<<"Celta Vigo">>, [<<"Celta de Vigo">>]},
        {<<"Elche">>, [<<"Elche CF">>]},
        {<<"Espanyol">>, [<<"RCD Espanyol">>]},
        {<<"Getafe">>, [<<"Getafe CF">>]},
        {<<"Granada">>, [<<"Granada CF">>]},
        {<<"Levante">>, [<<"Levante UD">>]},
        {<<"Malaga">>, [<<"Málaga">>, <<"Málaga CF">>]},
        {<<"Real Valladolid">>, [<<"Valladolid">>]},
        {<<"Sevilla FC">>, [<<"Sevilla">>]},
        {<<"Valencia">>, [<<"Valencia CF">>]},
        {<<"Villarreal">>, [<<"Villarreal CF">>]}
    ] = Alternatives.


match(Matchers) ->
    {Matches, Unmatched} = lists:foldl(fun (Query, {M, U}) ->
        T = erl_fuzzy_match:translate(fuzzy_test, Query, Matchers),
        case lists:member(T, names()) of
            true -> {ordsets:add_element(Query, M), U};
            false -> {M, ordsets:add_element(Query, U)}
        end
    end, {[], []}, queries()),
    summarize(Matches, Unmatched),
    {Matches, Unmatched}.

summarize(Matches, Unmatched) ->
    ct:log("Matched: ~.2f", [100*length(Matches)/(length(Matches)+length(Unmatched))]),
    if Unmatched /= [] ->
        ct:log("Unmatched: ~p", [Unmatched]);
    true ->
        ct:log("No unmatched queries")
    end.

names() -> espn_names().
queries() -> ordsets:to_list(ordsets:from_list(bbc_names() ++ soccerway_names() ++ guardian_names() ++ eurosport_names())).

espn_names() -> [
    <<"Almeria">>, <<"Athletic Bilbao">>, <<"Atletico Madrid">>,
    <<"Barcelona">>, <<"Celta Vigo">>, <<"Elche">>, <<"Espanyol">>,
    <<"Getafe">>, <<"Granada">>, <<"Levante">>, <<"Malaga">>,
    <<"Osasuna">>, <<"Rayo Vallecano">>, <<"Real Betis">>,
    <<"Real Madrid">>, <<"Real Sociedad">>, <<"Real Valladolid">>,
    <<"Sevilla FC">>, <<"Valencia">>, <<"Villarreal">>
].

bbc_names() -> [
    <<"Almería">>, <<"Athletic Bilbao">>, <<"Atlético Madrid">>,
    <<"Barcelona">>, <<"Celta de Vigo">>, <<"Elche">>, <<"Espanyol">>,
    <<"Getafe">>, <<"Granada CF">>, <<"Levante">>, <<"Málaga">>,
    <<"Osasuna">>, <<"Rayo Vallecano">>, <<"Real Betis">>,
    <<"Real Madrid">>, <<"Real Sociedad">>, <<"Real Valladolid">>,
    <<"Sevilla">>, <<"Valencia CF">>, <<"Villarreal">>
].

soccerway_names() -> [
    <<"Almería">>, <<"Athletic Club">>, <<"Atlético Madrid">>,
    <<"Barcelona">>, <<"Celta de Vigo">>, <<"Elche">>, <<"Espanyol">>,
    <<"Getafe">>, <<"Granada">>, <<"Levante">>, <<"Málaga">>,
    <<"Osasuna">>, <<"Rayo Vallecano">>, <<"Real Betis">>,
    <<"Real Madrid">>, <<"Real Sociedad">>, <<"Real Valladolid">>,
    <<"Sevilla">>, <<"Valencia">>, <<"Villarreal">>
].

guardian_names() -> [
    <<"A Bilbao">>, <<"Almeria">>, <<"Atlético">>,
    <<"Barcelona">>, <<"Celta Vigo">>, <<"Elche">>, <<"Espanyol">>,
    <<"Getafe">>, <<"Granada">>, <<"Levante">>, <<"Malaga">>,
    <<"Osasuna">>, <<"Rayo Vallecano">>, <<"Real Betis">>,
    <<"Real Madrid">>, <<"Real Sociedad">>, <<"Sevilla">>,
    <<"Valencia">>, <<"Valladolid">>, <<"Villarreal">>
].

eurosport_names() -> [
    <<"Athletic Club">>, <<"Atlético Madrid">>, <<"Celta de Vigo">>,
    <<"Elche CF">>, <<"FC Barcelona">>, <<"Getafe CF">>, <<"Granada CF">>,
    <<"Levante UD">>, <<"Málaga CF">>, <<"Osasuna">>, <<"RCD Espanyol">>,
    <<"Rayo Vallecano">>, <<"Real Betis">>, <<"Real Madrid">>,
    <<"Real Sociedad">>, <<"Real Valladolid">>, <<"Sevilla FC">>,
    <<"UD Almería">>, <<"Valencia CF">>, <<"Villarreal CF">>
].
