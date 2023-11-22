{application, 'envy', [
	{description, "wrapper prefixing os_env with application name"},
	{vsn, "0.9.1"},
	{modules, ['envy','envy_gen']},
	{registered, []},
	{applications, [kernel,stdlib,any]},
	{optional_applications, []},
	{env, []}
]}.