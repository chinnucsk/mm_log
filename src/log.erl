%%  Copyright (C) 2011 - Molchanov Maxim,
%% @author Maxim Molchanov <elzor.job@gmail.com>

-module(log).
-compile(export_all).
-compile(nowarn_unused_vars).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    error/1,
    info/1,
    warning/1
    ]).

-record( state, {   
                    log_collector
                }).

%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop, infinity).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state, infinity).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks
init([]) ->
  State = #state{log_collector=global:whereis_name(log_collector)},
  {ok, State}.

error(Report)->
  gen_server:cast(?MODULE, {put, node(), self(), error, Report}).

info(Report)->
  gen_server:cast(?MODULE, {put, node(), self(), info, Report}).

warning(Report)->
  gen_server:cast(?MODULE, {put, node(), self(), warning, Report}).


handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast({put, Node, Pid, Type, Report}, State) ->
  try
    case Type of
        info->
          State#state.log_collector ! {self(),node(),info,Report};
        error->
          State#state.log_collector ! {self(),node(),error,Report};
        warning->
          State#state.log_collector ! {self(),node(),warning,Report};
        _Else->
          State#state.log_collector ! {self(),node(),error,Report}
      end,
    {noreply, State}
  catch
    _:_ ->
      io:format("Looking for log collector...~n"),
      NewState = State#state{log_collector=global:whereis_name(log_collector)},
      {noreply, NewState}
  end;

handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  say("terminate ~p, ~p", [_Reason, _State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).