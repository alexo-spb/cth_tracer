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

-record(return, {value}).
-record(exception, {value}).

-record(call, {
    id,
    pid,
    from,
    mfa,
    result
}).

-record(state, {
    idx,
    stacks,
    uml,
    calls,
    plantuml,
    out
}).

%% API
-export([get_handler/1]).

get_handler(Config) ->
    {fun handle/4, init_state(Config)}.

%% Local functions

init_state(Config) ->
    #state{
        idx = 1,
        stacks = dict:new(),
        uml = start_uml(),
        calls = orddict:new(),
        plantuml = get_config(plantuml, Config),
        out = get_config(out, Config)
    }.

get_config(plantuml, Config) ->
    proplists:get_value(plantuml, Config, "plantuml.jar");
get_config(out, Config) ->
    proplists:get_value(out, Config, "trace.html").

handle(_Fd, Trace, _TraceInfo, State) ->
    case Trace of
        {trace_ts, {Pid, _, _}, call, MFA, _TS} ->
            handle_call(Pid, MFA, State);
        {trace_ts, {Pid, _, _}, return_from, MFA, Value, _TS} ->
            handle_return(Pid, MFA, Value, State);
        {trace_ts, {Pid, _, _}, exception_from, MFA, Value, _TS} ->
            handle_exception(Pid, MFA, Value, State);
        end_of_trace ->
            handle_end(State);
        _ ->
            State
    end.

handle_call(Pid, MFA, State) ->
    #state{idx = Idx, stacks = Stacks0, uml = Uml, calls = Calls} = State,
    {Call, Stacks1} = push_stack(Idx, Pid, MFA, Stacks0),
    State#state{
        idx = Idx + 1,
        stacks = Stacks1,
        uml = append_uml_call(Uml, Call),
        calls = store_call(Calls, Call)
    }.

handle_return(Pid, MFA, Value, State) ->
    #state{stacks = Stacks0, uml = Uml, calls = Calls} = State,
    case pop_stack(Pid, MFA, Stacks0) of
        {undefined, Stacks1} ->
            State#state{stacks = Stacks1};
        {Call, Stacks1} ->
            State#state{
                stacks = Stacks1,
                uml = append_uml_return(Uml, Call),
                calls = store_call(Calls, update_return(Call, Value))
            }
    end.

handle_exception(Pid, MFA, Value, State) ->
    #state{stacks = Stacks0, uml = Uml, calls = Calls} = State,
    case pop_stack(Pid, MFA, Stacks0) of
        {undefined, Stacks1} ->
            State#state{stacks = Stacks1};
        {Call, Stacks1} ->
            State#state{
                stacks = Stacks1,
                uml = append_uml_exception(Uml, Call),
                calls = store_call(Calls, update_exception(Call, Value))
            }
    end.

handle_end(State) ->
    #state{uml = Uml, calls = Calls, plantuml = PlantUml, out = Out} = State,
    SvgHtml = make_svg_html(end_uml(Uml), PlantUml),
    CallsTabHtml = make_calls_tab_html(Calls),
    write_html(Out, SvgHtml, CallsTabHtml).

start_uml() ->
    format("@startuml~n", []).

end_uml(Uml) ->
    Line = format("@enduml~n", []),
    append_uml(Uml, Line).

append_uml_call(Uml, Call) ->
    Line = format_uml_call(Call),
    append_uml(Uml, Line).

format_uml_call(#call{id = Id, pid = Pid, from = From, mfa = MFA}) ->
    {Mod, Fun, Args} = MFA, Arity = length(Args),
    Format = "~s -> ~s:[[#call~5..0B{details} ~5..0B]] ~p ~s:~s/~p~n",
    format(Format, [From, Mod, Id, Id, Pid, Mod, Fun, Arity]).

append_uml_return(Uml, Call) ->
    Line = format_uml_return(Call),
    append_uml(Uml, Line).

format_uml_return(#call{id = Id, pid = Pid, from = From, mfa = MFA}) ->
    {Mod, Fun, Args} = MFA, Arity = length(Args),
    Format = "~s <-- ~s:[[#call~5..0B{details} ~5..0B]] ~p ~s:~s/~p~n",
    format(Format, [From, Mod, Id, Id, Pid, Mod, Fun, Arity]).

append_uml_exception(Uml, Call) ->
    Line = format_uml_exception(Call),
    append_uml(Uml, Line).

format_uml_exception(#call{id = Id, pid = Pid, from = From, mfa = MFA}) ->
    {Mod, Fun, Args} = MFA, Arity = length(Args),
    Format = "~s <-[#red]- ~s:[[#call~5..0B{details} ~5..0B]] ~p ~s:~s/~p~n",
    format(Format, [From, Mod, Id, Id, Pid, Mod, Fun, Arity]).

append_uml(Uml, Line) ->
    <<Uml/binary, Line/binary>>.

format(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).

store_call(Calls, #call{id = Id} = Call) ->
    orddict:store(Id, Call, Calls).

update_return(Call, Value) ->
    Call#call{result = #return{value = Value}}.

update_exception(Call, Value) ->
    Call#call{result = #exception{value = Value}}.

push_stack(Idx, Pid, MFA, Stacks) ->
    case dict:find(Pid, Stacks) of
        {ok, Stack} ->
            #call{mfa = {Mod, _, _}} = hd(Stack),
            Call = #call{id = Idx, pid = Pid, from = Mod, mfa = MFA},
            {Call, dict:store(Pid, [Call | Stack], Stacks)};
        error ->
            Call = #call{id = Idx, pid = Pid, mfa = MFA},
            {Call, dict:store(Pid, [Call], Stacks)}
    end.

pop_stack(Pid, MFA, Stacks) ->
    case dict:find(Pid, Stacks) of
        {ok, [Call | Stack]} ->
            case {matched(Call, MFA), Stack} of
                {true, []} ->
                    {Call, dict:erase(Pid, Stacks)};
                {true, _} ->
                    {Call, dict:store(Pid, Stack, Stacks)};
                {false, []} ->
                    {undefined, dict:erase(Pid, Stacks)};
                {false, _} ->
                    pop_stack(Pid, MFA, dict:store(Pid, Stack, Stacks))

            end;
        error ->
            {undefined, Stacks}
    end.

matched(#call{mfa = {Mod, Fun, Args}}, MFA) ->
    {Mod, Fun, length(Args)} == MFA.

write_html(Out, SvgHtml, CallsTabHtml) ->
    Start = html_start(),
    End = html_end(),
    Html = <<Start/binary, SvgHtml/binary, CallsTabHtml/binary, End/binary>>,
    file:write_file(Out, Html).

html_start() ->
    <<"<!DOCTYPE html><html>"
      "<style>"
          ".calls-tab {border-collapse: collapse; width: 100%;}"
          ".calls-tab th {text-align: center; border-top: 1px solid black; border-bottom: 1px solid black; background: #DDDDDD;}"
          ".calls-tab tr:nth-child(odd) table {background: #E8E8E8;}"
          ".calls-tab tr table.exception {background: #FFCCCC;}"
          ".call-tab {border-collapse: collapse;}"
          ".call-tab td {vertical-align: top; font-family: Sans-serif; font-size: 13px;}"
          ".call-tab td:nth-child(2) {width: 100%;}"
      "</style>"
      "<body>">>.

html_end() ->
    <<"</body></html>">>.

make_svg_html(Uml, PlantUml) ->
    Out = run_puml(Uml, PlantUml),
    [_, SvgHtml] = binary:split(Out, <<"?>">>),
    SvgHtml.

run_puml(Uml, PlantUml) ->
    Command = command(Uml, PlantUml),
    Options = [stream, binary, eof],
    Port = open_port({spawn, Command}, Options),
    port_command(Port, Uml),
    get_puml_data(Port, []).

command(Uml, PlantUml) ->
    Size = byte_size(Uml),
    lists:concat(["head -c", Size, " | java -jar ", PlantUml, " -pipe -tsvg"]).

get_puml_data(Port, Acc) ->
    receive
        {Port, {data, Bin}} ->
            get_puml_data(Port, [Bin | Acc]);
        {Port, eof} ->
            port_close(Port),
            iolist_to_binary(lists:reverse(Acc))
    end.

make_calls_tab_html(Calls) ->
    Tags = [{id, "calls-tab"}, {class, "calls-tab"}],
    Head = make_calls_head(),
    Rows = make_calls_rows(Calls),
    Tab = {table, Tags, [Head | Rows]},
    iolist_to_binary(xmerl:export_simple_content([Tab], xmerl_xml)).

make_calls_head() ->
    Tags = [{colspan, "2"}, {class, "calls-tab-head"}],
    {th, Tags, ["Call Details"]}.

make_calls_rows(Calls) ->
    Fun = fun(_Id, Call, Acc) -> [make_calls_row(Call) | Acc] end,
    lists:reverse(orddict:fold(Fun, [], Calls)).

make_calls_row(Call) ->
    #call{id = Id, pid = Pid, from = From, mfa = MFA, result = Result} = Call,
    {Mod, Fun, Args} = MFA, Arity = length(Args),
    IdText = [format("~5..0B", [Id])],
    CallText = [format("~p ~s -> ~s:~s/~p", [Pid, From, Mod, Fun, Arity])],
    ArgsText = [format("~p", [Args])],
    ResultType = atom_to_list(type(Result)),
    ResultText = [format("~p", [value(Result)])],
    {tr, [{id, [<<"call">> | IdText]}], [
        {td, [
            {table, [{class, "call-tab " ++ ResultType}], [
                {tr, [{td, [{rowspan, "4"}], [IdText]}]},
                {tr, [{td, ["call"]}, {td, [CallText]}]},
                {tr, [{td, ["args"]}, {td, [ArgsText]}]},
                {tr, [{td, [ResultType]}, {td, [ResultText]}]}
            ]}
        ]}
    ]}.

type(Tuple) -> element(1, Tuple).

value(#return{value = Value}) -> Value;
value(#exception{value = Value}) -> Value.
