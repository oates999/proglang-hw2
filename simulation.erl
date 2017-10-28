-module(simulation).
-compile(export_all).

-import(parser, [read/1]).

spawn_from_list([]) ->
	[];
spawn_from_list([H|T]) ->
	NewNode = start_node(H),
	[ NewNode | spawn_from_list(T)].

link_nodes_from_list(Nodes, N) ->
	NodesLength = length(Nodes),
	case N of
		1 ->
			lists:nth(N, Nodes) ! { self(), { "neighbors", lists:nth(length(Nodes), Nodes), lists:nth(N+1, Nodes), length(Nodes) } };
		NodesLength ->
			lists:nth(N, Nodes) ! { self(), { "neighbors", lists:nth(N-1, Nodes), lists:nth(1, Nodes), length(Nodes) } };
		_ ->
			lists:nth(N, Nodes) ! { self(), { "neighbors", lists:nth(N-1, Nodes), lists:nth(N+1, Nodes), length(Nodes) } }
	end,
	case N of
		NodesLength ->
			io:format("neighbor linking complete.~n");
		_ ->
			link_nodes_from_list(Nodes, N+1)
	end.

run(Filename) ->
	Data = parser:read(Filename),
	Nodes = spawn_from_list(Data),
	link_nodes_from_list(Nodes, 1),
	lists:nth(1, Nodes) ! { self(), { "time_update", 0, 0, 0, 0 } },
	Nodes.

start_node(Dataset) ->
	spawn(simulation, node, [Dataset]).

node_looper(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE, SUP, L_N, R_N, HAS_LEAD, N} = Dataset,
	receive
		{ From, { "neighbors", LeftNode, RightNode, NumNodes } } ->
			SUPERVISOR = From,
			L_NODE = LeftNode,
			R_NODE = RightNode,
			NUM_NODES = NumNodes,
			io:format("node:~p~n  left:~p~n  right:~p~n", [self(), L_NODE, R_NODE]),
			node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, SUPERVISOR, L_NODE, R_NODE, HAS_LEAD, NUM_NODES });
		{ From, { "time_update", Time, LastElection, CurrentLeader, RevoltCounter } } ->
			if
				(ID =:= CurrentLeader) ->
					if
						RevoltCounter >= ((N+1) div 2) ->
							io:format("ID=~p was deposed at t=~p~n", [ID, Time]),
							SUP ! { self(), { "election" } },
							receive
								{ From, { "results", NextLeader } } ->
									NextLeader ! { self() , { "elected", Time+1, Time } }
							end;
						true ->
							L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter } }
					end;
				(ID =/= CurrentLeader) and (Time-LastElection >= TOLERANCE) and (Time-LastElection < TOLERANCE + N) ->
					io:format("ID=~p has revolted at t=~p... tolerance=~p~n", [ID, Time, TOLERANCE]),
					L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter+1 } };
				true ->
					L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter } }
			end
	end,
	node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, SUP, L_N, R_N, HAS_LEAD, N }).

node(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE } = Dataset,
	io:format("node created!~n"),
	node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, 0, 0, 0, false, 0 }).