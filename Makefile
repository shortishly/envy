#-*- mode: makefile-gmake -*-
# Copyright (c) 2012-2022 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = envy
PROJECT_DESCRIPTION = wrapper prefixing os_env with application name
PROJECT_VERSION = ${shell git describe --tags}

DEPS += any

dep_any = git https://github.com/shortishly/any.git

dep_any_commit = 0.3.2


SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync

SHELL_DEPS = \
	sync

PLT_APPS = \
	compiler \
	crypto \
	syntax_tools

include erlang.mk

app:: rebar.config
