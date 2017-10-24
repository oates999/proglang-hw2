-module(simulation).

-compile(export_all).

-import(parser, [read/1]).

run(Filename) ->
	Data = parser:read(Filename),
	Name = simulation:node(lists:nth(2, Data)),
	Name.

node(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE } = Dataset,
	NAME.