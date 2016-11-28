%%%============================================================================
%%% Copyright 2016 Aleksei Osin
%%%
%%%    Licensed under the Apache License, Version 2.0 (the "License");
%%%    you may not use this file except in compliance with the License.
%%%    You may obtain a copy of the License at
%%%
%%%        http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%    Unless required by applicable law or agreed to in writing, software
%%%    distributed under the License is distributed on an "AS IS" BASIS,
%%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%    See the License for the specific language governing permissions and
%%%    limitations under the License.
%%%============================================================================

%%% @author Aleksei Osin
%%% @copyright 2016 Aleksei Osin
%%% @doc Tracing hook for CT
%%%
%%% TODO: Add description
%%%
%%% see ttb details at http://erlang.org/documentation/doc-5.10.4/lib/observer-1.3.1.2/doc/html/ttb.html#p-2
%%%
%%% @end

-module(cth_tracer).


%% ct callbacks
-export([
    id/1,
    init/2,
    pre_init_per_testcase/3,
    post_end_per_testcase/4
]).


-record(state, {}).


%% @doc Provide a unique id for this CTH.
id(_Opts) ->
    ?MODULE.

%% @doc Initiate a common state. Called before any other callback function.
init(_Id, _Opts) ->
    {ok, #state{}}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC, Config, State) ->
    case get_trace_opts(Config) of
        [] ->
            ok;
        TraceOpts ->
            case get_trace_mecked(Config) of
                true ->
                    meck:new(meck, [passthrough]),
                    meck:expect(meck, expect, expect_3_fun(TraceOpts)),
                    meck:expect(meck, expect, expect_4_fun(TraceOpts));
                _ ->
                    ok
            end,
            Procs = get_procs(Config),
            Flags = get_flags(Config),
            start_tracer(Procs, Flags, TraceOpts)
    end,
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(_TC, Config, Return, State) ->
    case get_trace_opts(Config) of
        [] ->
            ok;
        _TraceOpts ->
            case get_trace_mecked(Config) of
                true ->
                    meck:unload(meck);
                _ ->
                    ok
            end,
            FormatOpts = get_format_opts(Config),
            ShowTrace = get_show_trace(Config),
            case {ShowTrace, Return} of
                {failed, {error, _}} ->
                    stop_tracer_and_format(FormatOpts);
                {all, _} ->
                    stop_tracer_and_format(FormatOpts);
                _ ->
                    stop_tracer()
            end
    end,
    {Return, State}.

get_show_trace(Config) ->
    get_config(show_trace, Config, failed).

get_procs(Config) ->
    get_config(procs, Config, all).

get_flags(Config) ->
    get_config(flags, Config, [call]).

get_trace_opts(Config) ->
    Modules = get_config(modules, Config, []),
    DefaultModTraceOpts = default_mod_trace_opts(Config),
    [merge_trace_opts(Item, DefaultModTraceOpts) || Item <- Modules].

default_mod_trace_opts(Config) ->
    get_config(trace_opts, Config, []) ++
        [{match_spec, [{'_', [], [{exception_trace}]}]}].

merge_trace_opts(Item, DefaultModTraceOpts) ->
    case Item of
        {Mod, ModTraceOpts} ->
            {Mod, ModTraceOpts ++ DefaultModTraceOpts};
        Mod ->
            {Mod, DefaultModTraceOpts}
    end.

get_format_opts(Config) ->
    get_config(format_opts, Config, default_format_opts(Config)).

get_trace_mecked(Config) ->
    get_config(trace_mecked, Config, false).

get_config(Key, Config, Default) ->
    TracerConfig = proplists:get_value(cth_tracer, Config, []),
    proplists:get_value(Key, TracerConfig, Default).

default_format_opts(_Config) ->
    [{handler, cth_tracer_puml_format:get_handler()}].

start_tracer(Procs, Flags, TraceOpts) ->
    ttb:tracer(),
    ttb:p(Procs, Flags),
    [set_tracing(Mod, ModTraceOpts) || {Mod, ModTraceOpts} <- TraceOpts].

stop_tracer_and_format(FormatOpts) ->
    ttb:stop({format, FormatOpts}).

stop_tracer() ->
    ttb:stop().

expect_3_fun(TraceOpts) ->
    fun(Mod, Func, Expect) ->
        call_expect_and_set_tracing([Mod, Func, Expect], TraceOpts)
    end.

expect_4_fun(TraceOpts) ->
    fun(Mod, Func, ArgsSpec, RetSpec) ->
        call_expect_and_set_tracing([Mod, Func, ArgsSpec, RetSpec], TraceOpts)
    end.

call_expect_and_set_tracing(ExpectArgs, TraceOpts) ->
    Result = apply(meck_meck_original, expect, ExpectArgs),
    Mod = hd(ExpectArgs),
    case lists:keyfind(Mod, 1, TraceOpts) of
        {Mod, ModTraceOpts} ->
            set_tracing(Mod, ModTraceOpts);
        false ->
            ok
    end,
    Result.

set_tracing(Mod, ModTraceOpts) ->
    MS = proplists:get_value(match_spec, ModTraceOpts),
    case proplists:is_defined(trace_locals, ModTraceOpts) of
        false ->
            ttb:tp(Mod, MS);
        true ->
            ttb:tpl(Mod, MS)
    end.
