-module(native_ffi).
-export([to_list/1, to_string/1, to_int/1, to_float/1, is_tuple2/1, decode_tuple2/1]).

to_list(Val) when is_list(Val) -> {ok, Val};
to_list(_) -> {error, nil}.

to_string(Val) when is_binary(Val) -> {ok, Val};
to_string(Val) when is_atom(Val) -> {ok, atom_to_binary(Val, utf8)};
to_string(_) -> {error, nil}.

to_int(Val) when is_integer(Val) -> {ok, Val};
to_int(_) -> {error, nil}.

to_float(Val) when is_float(Val) -> {ok, Val};
to_float(_) -> {error, nil}.

is_tuple2({_, _}) -> true;
is_tuple2(_) -> false.

decode_tuple2({A, B}) -> {ok, {A, B}};
decode_tuple2(_) -> {error, nil}.
