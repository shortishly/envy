{application, 'envy', [
	{description, "wrapper prefixing os_env with application name"},
	{vsn, "0.8.0-3-gaef8eb3"},
	{modules, ['envy','envy_gen']},
	{registered, []},
	{applications, [kernel,stdlib,any]},
	{optional_applications, []},
	{env, []}
]}.