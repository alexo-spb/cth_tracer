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
    result,
    start_ts,
    end_ts
}).

-record(state, {
    idx,
    stacks,
    uml,
    calls,
    plantuml
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
        plantuml = get_config(plantuml, Config)
    }.

get_config(plantuml, Config) ->
    proplists:get_value(plantuml, Config, "plantuml.jar").

handle(Fd, Trace, _TraceInfo, State) ->
    case Trace of
        {trace_ts, {Pid, _, _}, call, MFA, TS} ->
            handle_call(Pid, MFA, TS, State);
        {trace_ts, {Pid, _, _}, return_from, MFA, Value, TS} ->
            handle_return(Pid, MFA, Value, TS, State);
        {trace_ts, {Pid, _, _}, exception_from, MFA, Value, TS} ->
            handle_exception(Pid, MFA, Value, TS, State);
        end_of_trace ->
            handle_end(Fd, State);
        _ ->
            State
    end.

handle_call(_Pid, {_, module_info, _}, _TS, State) ->
    State;
handle_call(Pid, MFA, TS, State) ->
    #state{idx = Idx, stacks = Stacks0, uml = Uml, calls = Calls} = State,
    {Call, Stacks1} = push_stack(Idx, Pid, MFA, TS, Stacks0),
    State#state{
        idx = Idx + 1,
        stacks = Stacks1,
        uml = append_uml_call(Uml, Call),
        calls = store_call(Calls, Call)
    }.

handle_return(_Pid, {_, module_info, _}, _Value, _TS, State) ->
    State;
handle_return(Pid, MFA, Value, TS, State) ->
    #state{stacks = Stacks0, uml = Uml, calls = Calls} = State,
    case pop_stack(Pid, MFA, Stacks0) of
        {undefined, Stacks1} ->
            State#state{stacks = Stacks1};
        {Call, Stacks1} ->
            State#state{
                stacks = Stacks1,
                uml = append_uml_return(Uml, Call),
                calls = store_call(Calls, update_return(Call, Value, TS))
            }
    end.

handle_exception(Pid, MFA, Value, TS, State) ->
    #state{stacks = Stacks0, uml = Uml, calls = Calls} = State,
    case pop_stack(Pid, MFA, Stacks0) of
        {undefined, Stacks1} ->
            State#state{stacks = Stacks1};
        {Call, Stacks1} ->
            State#state{
                stacks = Stacks1,
                uml = append_uml_exception(Uml, Call),
                calls = store_call(Calls, update_exception(Call, Value, TS))
            }
    end.

handle_end(Fd, State) ->
    #state{uml = Uml, calls = Calls, plantuml = PlantUml} = State,
    SvgHtml = make_svg_html(end_uml(Uml), PlantUml),
    CallsTabHtml = make_calls_tab_html(Calls),
    write_html(Fd, SvgHtml, CallsTabHtml).

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

update_return(Call, Value, TS) ->
    Call#call{result = #return{value = Value}, end_ts = TS}.

update_exception(Call, Value, TS) ->
    Call#call{result = #exception{value = Value}, end_ts = TS}.

push_stack(Idx, Pid, MFA, TS, Stacks) ->
    case dict:find(Pid, Stacks) of
        {ok, Stack} ->
            #call{mfa = {Mod, _, _}} = hd(Stack),
            Call = #call{id = Idx, pid = Pid, from = Mod, mfa = MFA, start_ts = TS},
            {Call, dict:store(Pid, [Call | Stack], Stacks)};
        error ->
            Call = #call{id = Idx, pid = Pid, mfa = MFA, start_ts = TS},
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

write_html(Fd, SvgHtml, CallsTabHtml) ->
    Start = html_start(),
    End = html_end(),
    Html = <<Start/binary, SvgHtml/binary, CallsTabHtml/binary, End/binary>>,
    file:write(Fd, Html).

html_start() ->
    <<"<!DOCTYPE html><html>"
      "<style>"
          ".calls-tab {border-collapse: collapse; width: 100%;} "
          ".calls-tab th {text-align: center; border-top: 1px solid black; border-bottom: 1px solid black; background: #DDDDDD;} "
          ".calls-tab tr:nth-child(8n-2) {background: #E8E8E8;} "
          ".calls-tab tr:nth-child(8n-1) {background: #E8E8E8;} "
          ".calls-tab tr:nth-child(8n+0) {background: #E8E8E8;} "
          ".calls-tab tr:nth-child(8n+1) {background: #E8E8E8;}"
          ".calls-tab tr td:first-child {width:1%;} "
          ".calls-tab td.exception {background: #FFCCCC;} "
          ".calls-tab td {vertical-align: top; font-family: Sans-serif; font-size: 13px;} "
          ".calls-tab tr:nth-child(4n-2) td:nth-child(1) {border-right: 1px solid #C0C0C0;} "
          ".calls-tab tr:nth-child(4n-2) td:nth-child(2) {border-right: 1px solid #C0C0C0;} "
          ".calls-tab tr:nth-child(4n-1) td:nth-child(1) {border-right: 1px solid #C0C0C0;} "
          ".calls-tab tr:nth-child(4n-0) td:nth-child(1) {border-right: 1px solid #C0C0C0;} "
          ".calls-tab tr:nth-child(4n+1) td:nth-child(1) {border-right: 1px solid #C0C0C0;}"
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
    Tags = [{colspan, "3"}, {class, "calls-tab-head"}],
    {th, Tags, ["Call Details"]}.

make_calls_rows(Calls) ->
    lists:reverse(orddict:fold(fun make_call_rows/3, [], Calls)).

make_call_rows(_Id, Call, Acc) ->
    #call{id = Id, pid = Pid, from = From, mfa = MFA, result = Result} = Call,
    {Mod, Fun, Args} = MFA, Arity = length(Args),
    IdText = [format("~5..0B", [Id])],
    CallText = [format("~p ~s -> ~s:~s/~p", [Pid, From, Mod, Fun, Arity])],
    {Ms, Us} = duration(Call),
    DurationText = [format("~p.~pms", [Ms, Us])],
    ArgsText = [format("~p", [Args])],
    ResultType = atom_to_list(type(Result)),
    ResultText = [format("~p", [value(Result)])],
    IdTags = [{id, [<<"call">> | IdText]}, {rowspan, "4"}, {class, ResultType}],
    R1 = {tr, [{td, IdTags, [IdText]}, {td, ["call"]}, {td, [CallText]}]},
    R2 = {tr, [{td, ["duration"]}, {td, [DurationText]}]},
    R3 = {tr, [{td, ["arguments"]}, {td, [ArgsText]}]},
    R4 = {tr, [{td, [ResultType]}, {td, [ResultText]}]},
    [R4, R3, R2, R1 | Acc].

duration(#call{start_ts = StartTS, end_ts = EndTS}) ->
    Diff = timer:now_diff(EndTS, StartTS),
    Us = Diff rem 1000,
    Ms = Diff div 1000,
    {Ms, Us}.

type(Tuple) -> element(1, Tuple).

value(#return{value = Value}) -> Value;
value(#exception{value = Value}) -> Value.
