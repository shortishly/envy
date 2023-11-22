{application, 'envy', [
	{description, "wrapper prefixing os_env with application name"},
	{vsn, "0.9.0"},
	{id, "0.9.0-dirty"},
	{modules, ['envy','envy_gen']},
	{registered, []},
	{applications, [kernel,stdlib,any]},
	{optional_applications, []},
	{env, []}
]}.