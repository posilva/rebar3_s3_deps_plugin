-module(rebar3_s3_deps_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, s3deps).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, ""},              % How to use the plugin
            {opts, []},                 % list of options understood by the plugin
            {short_desc, "Get dependencies from an S3 Bucket"},
            {desc, "Get dependencies from an S3 Bucket"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).