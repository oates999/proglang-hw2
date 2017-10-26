-module(simulation).
-compile(export_all).

-import(parser, [read/1]).

spawn_from_list([]) ->
	[];
spawn_from_list([H|T]) ->
	NewNode = start_node(H),
	[ NewNode | spawn_from_list(T)].

link_nodes_from_list(Nodes, N=1) ->
	lists:nth(N, Nodes) ! { self(), { "neighbors", lists:nth(length(Nodes), Nodes), lists:nth(N+1, Nodes) } },
	link_nodes_from_list(Nodes, 5);
link_nodes_from_list(Nodes, N=5) ->
	lists:nth(N, Nodes) ! { self(), { "neighbors", lists:nth(N-1, Nodes), lists:nth(1, Nodes) } }.

run(Filename) ->
	Data = parser:read(Filename),
	Nodes = spawn_from_list(Data),
	link_nodes_from_list(Nodes, 1),
	Nodes.

start_node(Dataset) ->
	spawn(simulation, node, [Dataset]).

node_looper() ->
	receive
		{From, { "neighbors", LeftNode, RightNode } } ->
			L_NODE = LeftNode,
			R_NODE = RightNode,
			io:format("node:~p~n  left:~p~n  right:~p~n", [self(), L_NODE, R_NODE])
	end,
	node_looper().

node(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE } = Dataset,
	io:format("node created!~n"),
	node_looper().