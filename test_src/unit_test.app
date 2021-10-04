%% This is the application resource file (.app file) for the 'base'
%% application.
{application, unit_test,
[{description, "unit_test  " },
{vsn, "1.0.0" },
{modules, 
	  [unit_test_app,unit_test_sup,unit_test]},
{registered,[unit_test]},
{applications, [kernel,stdlib]},
{mod, {unit_test_app,[]}},
{start_phases, []}
]}.
