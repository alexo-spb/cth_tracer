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
%%% @copyright 2017 Aleksei Osin
%%% @doc Tracer helper
%%%
%%% TODO: Add description
%%%
%%% see ttb details at http://erlang.org/documentation/doc-5.10.4/lib/observer-1.3.1.2/doc/html/ttb.html#p-2
%%%
%%% @end

-module(cth_tracer).

-record(state, {
    procs,
    flags,
    trace_opts,
    fetch_dir,
    format_opts
}).

%% api
-export([
    new/1,
    start/1,
    stop/1,
    attach_meck/1,
    detach_meck/1
]).

%% @doc Creates an opaque tracer handler.
new(Config) ->
    #state{
        procs = get_config(procs, Config),
        flags = get_config(flags, Config),
        trace_opts = get_config(trace_opts, Config),
        fetch_dir = get_config(fetch_dir, Config),
        format_opts = get_config(format_opts, Config)
    }.

%% @doc Starts the tracer.
start(State) ->
    #state{procs = Procs, flags = Flags, trace_opts = TraceOpts} = State,
    ttb:tracer(),
    ttb:p(Procs, Flags),
    [set_tracing(Mod, ModTraceOpts) || {Mod, ModTraceOpts} <- TraceOpts],
    ok.

%% @doc Stops the tracer.
stop(State) ->
    #state{fetch_dir = Dir, format_opts = FormatOpts} = State,
    ttb:stop([{fetch_dir, Dir}, {format, FormatOpts}]),
    ok.

%% @doc Attach to the meck API module.
attach_meck(State) ->
    case code:which(meck) of
        non_existing ->
            {error, not_available};
        _ ->
            #state{trace_opts = TraceOpts} = State,
            meck:new(meck, [passthrough]),
            meck:expect(meck, expect, make_expect_3_mock(TraceOpts)),
            meck:expect(meck, expect, make_expect_4_mock(TraceOpts))
    end.

%% @doc Detach from the meck API module.
detach_meck(_State) ->
    case code:which(meck) of
        non_existing ->
            {error, not_available};
        _ ->
            case whereis(meck_util:proc_name(meck)) of
                undefined ->
                    {error, not_attached};
                _ ->
                    meck:unload(meck)
            end
    end.

%% Local functions

get_config(fetch_dir, Config) ->
    proplists:get_value(fetch_dir, Config, "trace");
get_config(procs, Config) ->
    proplists:get_value(procs, Config, all);
get_config(flags, Config) ->
    proplists:get_value(flags, Config, [call]);
get_config(trace_opts, Config) ->
    TraceOpts = proplists:get_value(trace_opts, Config, default_trace_opts()),
    Modules = proplists:get_value(modules, Config, []),
    [merge_trace_opts(Item, TraceOpts) || Item <- Modules];
get_config(format_opts, Config) ->
    proplists:get_value(format_opts, Config, []).

default_trace_opts() ->
    [{match_spec, [{'_', [], [{exception_trace}]}]}].

merge_trace_opts({Mod, TraceOpts}, Default) ->
    {Mod, TraceOpts ++ Default};
merge_trace_opts(Mod, Default) ->
    {Mod, Default}.

make_expect_3_mock(TraceOpts) ->
    fun(Mod, Func, Expect) ->
        call_expect_and_set_tracing([Mod, Func, Expect], TraceOpts)
    end.

make_expect_4_mock(TraceOpts) ->
    fun(Mod, Func, ArgsSpec, RetSpec) ->
        call_expect_and_set_tracing([Mod, Func, ArgsSpec, RetSpec], TraceOpts)
    end.

call_expect_and_set_tracing(ExpectArgs, TraceOpts) ->
    Result = apply(meck_util:original_name(meck), expect, ExpectArgs),
    Mod = hd(ExpectArgs),
    case lists:keyfind(Mod, 1, TraceOpts) of
        {Mod, ModTraceOpts} ->
            set_tracing(Mod, ModTraceOpts);
        false ->
            ok
    end,
    Result.

set_tracing(Mod, TraceOpts) ->
    MS = proplists:get_value(match_spec, TraceOpts),
    case proplists:is_defined(trace_locals, TraceOpts) of
        false ->
            ttb:tp(Mod, MS);
        true ->
            ttb:tpl(Mod, MS)
    end.
