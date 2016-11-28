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
%%% @doc PlantUML trace formatter
%%%
%%% TODO: Add description
%%%
%%% @end

-module(cth_tracer_puml_format).


%% API
-export([get_handler/0]).


get_handler() ->
    {fun handler/4, undefined}.

handler(Fd, Trace, TraceInfo, undefined) ->
    io:fwrite(Fd, "@startuml~n", []),
    handler(Fd, Trace, TraceInfo, init_stacks());
handler(Fd, Trace, _TraceInfo, Stacks) ->
    case Trace of
        {trace_ts, {Pid, _, _}, call, MFA, _TS} ->
            format_call(Fd, Pid, MFA, Stacks);
        {trace_ts, {Pid, _, _}, return_from, MFA, Result, _TS} ->
            format_return(Fd, Pid, MFA, Result, Stacks);
        {trace_ts, {Pid, _, _}, exception_from, MFA, Exception, _TS} ->
            format_exception(Fd, Pid, MFA, Exception, Stacks);
        end_of_trace ->
            io:fwrite(Fd, "~n@enduml", []);
        _ ->
            Stacks
    end.

format_call(Fd, Pid, MFA, Stacks0) ->
    {Mod, Fun, Args} = MFA,
    Arity = length(Args),
    {From, Stacks1} = push_stack(Pid, MFA, Stacks0),
    io:fwrite(Fd, "~s -> ~s:~p ~s:~s/~p", [From, Mod, Pid, Mod, Fun, Arity]),
    io:fwrite(Fd, "'arguments: ~9000p", [Args]),
    Stacks1.

format_return(Fd, Pid, MFA, Result, Stacks0) ->
    {Mod, Fun, Arity} = MFA,
    {From, Stacks1} = pop_stack(Pid, MFA, Stacks0),
    io:fwrite(Fd, "~s <-- ~s:~p ~s:~s/~p", [From, Mod, Pid, Mod, Fun, Arity]),
    io:fwrite(Fd, "'result: ~9000p", [Result]),
    Stacks1.

format_exception(Fd, Pid, MFA, Exception, Stacks0) ->
    {Mod, Fun, Arity} = MFA,
    {From, Stacks1} = pop_stack(Pid, MFA, Stacks0),
    io:fwrite(Fd, "~s <-- ~s:~p ~s:~s/~p exception", [From, Mod, Pid, Mod, Fun, Arity]),
    io:fwrite(Fd, "'exception: ~9000p", [Exception]),
    Stacks1.

init_stacks() ->
    dict:new().

push_stack(Pid, Call, Stacks) ->
    case dict:find(Pid, Stacks) of
        {ok, Stack} ->
            {Mod, _, _} = hd(Stack),
            {Mod, dict:store(Pid, [Call | Stack], Stacks)};
        error ->
            {undefined, dict:store(Pid, [Call], Stacks)}
    end.

pop_stack(Pid, Return, Stacks) ->
    case dict:find(Pid, Stacks) of
        {ok, [Call | Stack]} ->
            case {matched(Call, Return), Stack} of
                {true, [{Mod, _, _} | _]} ->
                    {Mod, dict:store(Pid, Stack, Stacks)};
                {true, []} ->
                    {undefined, dict:erase(Pid, Stacks)};
                {false, [_ | _]} ->
                    pop_stack(Pid, Return, dict:store(Pid, Stack, Stacks));
                {false, []} ->
                    {undefined, dict:erase(Pid, Stacks)}
            end;
        error ->
            {undefined, Stacks}
    end.

matched({Mod, Fun, Args}, {Mod, Fun, Arity}) ->
    length(Args) == Arity;
matched(_, _) ->
    false.
