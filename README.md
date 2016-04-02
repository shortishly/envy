# Envy #

[![Build Status](https://travis-ci.org/shortishly/envy.svg)](https://travis-ci.org/shortishly/envy)

An OTP application typically gets its configuration using the
operating system environment (`os:getenv/1`) and/or application
environment (`application:get_env/2`). Both are used to allow
configuration to be easily overridden by the OS environment, falling
back to defaults provided by the application environment.

The result of calling `os:getenv/1` is always a `string()` (or `false`
if not present in the environment). For example, running the following:

```shell
KERNEL_HTTP_PORT=8080 erl
```

```erlang
1> os:getenv("KERNEL_HTTP_PORT").
"8080"
```

Whereas using the application environment:

```shell
erl -kernel http_port 8080
```

```erlang
2> application:get_env(kernel, http_port).
{ok,8080}
```

Envy is a simple wrapper over
[get_env](https://github.com/uwiger/gproc/blob/master/doc/gproc.md#get_env-4)
which does a couple of things:

1. Convert the different types naturally returned by `os:getenv` and
`application:get_env` to the type that you are expecting.

2. Prefix any `os_env` with the name of the application, preventing
any environment clashes.

As an example, starting erlang with envy running:

```shell
cd envy
KERNEL_HTTP_PORT=8080 make shell
```

The following calls to `to_integer/3` demonstrate obtaining
configuration either from the OS or application environment:

```erlang
1> application:set_env(kernel, http_port, 1080).
2> envy:to_integer(kernel, http_port, [app_env, os_env, {default, 80}]).
1080
3> envy:to_integer(kernel, http_port, [os_env, app_env, {default, 80}]).
8080
```

When using the `os_env` strategy envy automatically prefixes the
application name to the environment variable lookup, so that
`KERNEL_HTTP_PORT` is used when `http_port` is the key.

Envy has helper wrappers for `to_integer/3`, `to_float/3`,
`to_atom/3`, `to_boolean/3`, `to_binary/3` and `to_list/3`.
