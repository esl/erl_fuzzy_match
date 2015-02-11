# Erlang Fuzzy String Matcher

The Fuzzy Match for Erlang pulls together a handful of algorithms
to make fuzzy string matching available as a library to your Erlang
programs.

This little piece of code was inspired by the Python code presented in
http://www.sportshacker.net/posts/fuzzy_string_matching.html


## Overview

The fuzzy matcher runs as a node-local, named `gen_server` that
keeps a dictionary of translations. If a string is not found in the
lookup dictionary, it is sent through a list of string matching
algorithms (abbreviations, levenshtein, tokenized). A valid match
is added as a new entry in the dictionary. If no match was found,
the string is taken as a new entry in the dictionary, matching
itself.

An example, using English Premier league football team names ('14/'15
season):
```Erlang
1> Teams = [
<<"Arsenal">>,
<<"Aston Villa">>,
<<"Burnley">>,
<<"Chelsea">>,
<<"Crystal Palace">>,
<<"Everton">>,
<<"Hull City">>,
<<"Leicester City">>,
<<"Liverpool">>,
<<"Manchester City">>,
<<"Manchester United">>,
<<"Newcastle United">>,
<<"Queens Park Rangers">>,
<<"Southampton">>,
<<"Stoke City">>,
<<"Sunderland">>,
<<"Swansea City">>,
<<"Tottenham Hotspur">>,
<<"West Bromwich Albion">>,
<<"West Ham United">>
].
[<<"Arsenal">>,<<"Aston Villa">>,<<"Burnley">>,
 <<"Chelsea">>,<<"Crystal Palace">>,<<"Everton">>,
 <<"Hull City">>,<<"Leicester City">>,<<"Liverpool">>,
 <<"Manchester City">>,<<"Manchester United">>,
 <<"Newcastle United">>,<<"Queens Park Rangers">>,
 <<"Southampton">>,<<"Stoke City">>,<<"Sunderland">>,
 <<"Swansea City">>,<<"Tottenham Hotspur">>,
 <<"West Bromwich Albion">>,<<"West Ham United">>]
2> OddNames = [
{<<"Saints">>, <<"Southampton">>},
{<<"Spurs">>, <<"Tottenham Hotspur">>}
].
[{<<"Saints">>,<<"Southampton">>},
 {<<"Spurs">>,<<"Tottenham Hotspur">>}]
3> erl_fuzzy_match:start_link(premier, dict:from_list(OddNames), Teams).
{ok,<0.36.0>}
4> erl_fuzzy_match:translate(premier, <<"Chelsea">>).
<<"Chelsea">>
5> erl_fuzzy_match:translate(premier, <<"Manchester Utd">>).
<<"Manchester United">>
6> erl_fuzzy_match:translate(premier, <<"West Ham">>).
<<"West Ham United">>
7> erl_fuzzy_match:translate(premier, <<"Newcastle">>).
<<"Newcastle United">>
8> erl_fuzzy_match:translate(premier, <<"QPR">>).
<<"Queens Park Rangers">>
```

But some caution may still be required (!):
```Erlang
9> erl_fuzzy_match:translate(premier, <<"W.B.A">>).
<<"W.B.A">>
10> erl_fuzzy_match:translate(premier, <<"Bristol City">>).
<<"Hull City">>
```


## Starting And Stopping

A fuzzy matcher is started by one of the `start_link` functions:

```Erlang
start_link(Name)
start_link(Name, Dict, Canon)
```
where `Name` is an atom, `Dict` a mapping from UTF-8 binary strings
to UTF-8 binaries strings (
[`dict(binary(),binary())`](http://www.erlang.org/doc/man/dict.html)
), and `Canon` a list of UTF-8 binary strings.

Multiple fuzzy matchers may run at the same time. The name supplied
at startup allows selecting which fuzzy matcher to use when using
the other API calls.

A list of known canonical strings may be provided when starting the
fuzzy matcher, otherwise it is initially empty. This can be done
by either providing an empty dictionary and a list of canonical
UTF-8 binary strings, a pre-populated dictionary of known matches
(from UTF-8 binary strings to UTF-8 binary strings), or a mixture
of the two.

Under normal operations it is usually desirable to enter new,
unmatched strings into the list of canonical strings and into the
dictionary. If the extension of the initial set of canonical strings
and mappings is *not* desired, an option `[fixed]` may be provided
via the additional start function:
```Erlang
start_link(Name, Dict, Canon, [fixed])
```

A fuzzy matcher may be stopped either explicitly via the `stop` function
```Erlang
stop(Name)
```

A fuzzy matcher may also get started as part of a supervision tree,
in which case the normal supervision shutdown sequence will lead
to graceful termination of the fuzzy matcher.


## String Translation

A subject string may be fuzzily matched and translated into canonical
form using
```Erlang
translate(Name, S)
translate(Name, S, Matchers)
```
where `Name` is an atom (the given name of the fuzzy matcher when
it was started), `S` is a UTF-8 binary string to translate, and
`Matchers` is an optional list of functions to use as the fuzzy
matching algorithms.

A matcher function must obey the type
```Erlang
Matcher = fun(S :: binary(), [Canon :: binary()]) -> undefined | {ok, Name :: binary()}
```
that is, take the subject string as a UTF-8 binary and a list of
canonical UTF-8 binary strings returning either the atom `undefined`
if no match was found or `{ok, Canonical}` if `Canonical` was found
as a suitable match for the subject.

If a list of matcher functions is provided, they are tried in the
order provided in the list, if a matcher function returns 'undefined',
the next one on the list is tried.

The default list of match functions are:
- exact match (via the dictionary)
- abbreviation match (using [this](http://stackoverflow.com/questions/7331462/check-if-a-string-is-a-possible-abbrevation-for-a-name) algorithm)
- Levenshtein difference between subject and canonical string is less than or equal to two (2)
- all tokens (separated by whitespace) of subject and canonical matched according to exact match or levenshtein

If no matcher function finds a match, the `translate/2,3` functions
return the original subject string. Additionally, if the start
option `[fixed]` was *not* provided, the subject string is entered
into the list of canonical strings.


## Data Access

The list of matcher functions is returned by
```Erlang
matchers(Name)
```

The current lookup dictionary is returned by
```Erlang
dict(Name)
```

The current list of canonical strings is returned by
```Erlang
canonicals(Name)
```


## Possible Extensions

- Configurability of the Levenshtein distance threshold.
- Upfront configurability of the default matcher sequence.
- Upfront configurability of externally defined matchers.
