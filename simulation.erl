-module(simulation).
-export([run/1]).

-import(parser, [read/1]).

run(Filename) ->
	Data = parser:read(Filename),
	io:format("Hello, world!~n").