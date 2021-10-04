%% This is the application resource file (.app file) for the 'base'
%% application.
{application, bully,
[{description, "Mnesia based distributed dbase" },
{vsn, "0.1.0" },
{modules, 
	  [bully,bully_sup,bully_server]},
{registered,[bully]},
{applications, [kernel,stdlib]},
{mod, {bully,[]}},
{start_phases, []},
{git_path,"https://github.com/joq62/bully.git"},
{env,[{dbase_nodes,['bully@joq62-X550CA','bully@c0','bully@c2']}]}
]}.
