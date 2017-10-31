% DISTRIBUTED VERSION
-module(simulation).
-compile(export_all).

-import(parser, [read/1]).

spawn_from_list([]) ->
	[];
spawn_from_list([H|T]) ->
	{ _, HOST, NAME, _, _ } = H,
	NewNode = start_node(H, list_to_atom(NAME++"@"++HOST)),
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
			io:format("");
		_ ->
			link_nodes_from_list(Nodes, N+1)
	end.

kill_nodes(Nodes, N) ->
	NodesLength = length(Nodes),
	exit(lists:nth(N, Nodes), normal),
	case N of
		NodesLength ->
			io:format("");
		_ ->
			kill_nodes(Nodes, N+1)
	end.

run_looper(Nodes) ->
	receive
		{ _, { "output", OutputString } } ->
			file:write_file("output.txt", OutputString, [append]),
			run_looper(Nodes);
		{ _, { "end_of_simulation" } } ->
			kill_nodes(Nodes, 1),
			file:write_file("output.txt", "End of simulation\n", [append])
	end,
	exit(normal).

run(Filename) ->
	Data = parser:read(Filename),
	Nodes = spawn_from_list(Data),
	link_nodes_from_list(Nodes, 1),
	lists:nth(1, Nodes) ! { self(), { "faux_election" } },
	file:delete("output.txt"),
	run_looper(Nodes).

start_node(Dataset, Name) ->
	spawn(Name, simulation, node, [Dataset]).

node_looper(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE, SUP, L_N, R_N, HAS_LEAD, N} = Dataset,
	receive
		{ From, { "neighbors", LeftNode, RightNode, NumNodes } } ->
			SUPERVISOR = From,
			L_NODE = LeftNode,
			R_NODE = RightNode,
			NUM_NODES = NumNodes,
			node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, SUPERVISOR, L_NODE, R_NODE, HAS_LEAD, NUM_NODES });
		{ From, { "time_update", Time, LastElection, CurrentLeader, RevoltCounter } } ->
			if
				(ID =:= CurrentLeader) ->
					if
						RevoltCounter >= ((N+1) div 2) ->
							OutputString = lists:concat(["ID=", ID, " was deposed at t=", Time, "\n"]),
							SUP ! { self(), { "output", OutputString } },
							io:format("ID=~p was deposed at t=~p~n", [ID, Time]),
							L_N ! { self(), { "revolution", -1, self() } },
							receive
								{ From, { "revolution", HighestPriority, NextLeader } } ->
									if
										(HighestPriority =:= -1) ->
											SUP ! { self(), { "end_of_simulation" } };
										true ->
											NextLeader ! { self(), { "elected", Time+1, Time } }
									end
							end;
						true ->
							L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter } }
					end;
				(ID =/= CurrentLeader) and (Time-LastElection > TOLERANCE) and (Time-LastElection =< TOLERANCE + N) ->
					OutputString = lists:concat(["ID=", ID, " revolted at t=", Time, "\n"]),
					SUP ! { self(), { "output", OutputString } },
					io:format("ID=~p revolted at t=~p~n", [ID, Time]),
					L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter+1 } };
				true ->
					L_N ! { self(), { "time_update", Time+1, LastElection, CurrentLeader, RevoltCounter } }
			end;
		{ _, { "elected", Time, LastElection } } ->
			OutputString = lists:concat(["ID=", ID, " became leader at t=", Time, "\n"]),
			SUP ! { self(), { "output", OutputString } },
			io:format("ID=~p became leader at t=~p~n", [ID, Time]),
			L_N ! { self(), { "time_update", Time+1, LastElection, ID, 0 } },
			node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, SUP, L_N, R_N, true, N });
		{ _, { "revolution", HighestPriority, NextLeader } } ->
			if
				((HAS_LEAD =:= false) and (HighestPriority < PRIORITY)) ->
					L_N ! { self(), { "revolution", PRIORITY, self() } };
				true ->
					L_N ! { self(), { "revolution", HighestPriority, NextLeader } }
			end;
		{ _, { "faux_election" } } ->
			L_N ! { self(), { "revolution", PRIORITY, self() } },
			receive
				{ _, { "revolution", _, NextLeader } } ->
					NextLeader ! { self(), { "elected", 0, 0 } }
			end
	end,
	node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, SUP, L_N, R_N, HAS_LEAD, N }).

node(Dataset) ->
	{ ID, HOST, NAME, PRIORITY, TOLERANCE } = Dataset,
	node_looper({ ID, HOST, NAME, PRIORITY, TOLERANCE, 0, 0, 0, false, 0 }).