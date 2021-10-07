%% This is the application resource file (.app file) for the 'base'
%% application.
{application, bully,
[{description, "Leader election using Bully algorithm" },
{vsn, "0.1.0" },
{modules, 
	  [bully,bully_sup,bully_server]},
{registered,[bully]},
{applications, [kernel,stdlib]},
{mod, {bully,[]}},
{start_phases, []},
{git_path,"https://github.com/joq62/bully_election.git"},
{env,[{connection_nodes,['controller@c0','controller@c2']}]}
]}.
