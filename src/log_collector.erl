%%  Copyright (C) 2011 - Molchanov Maxim,
%% @author Maxim Molchanov <elzor.job@gmail.com>

-module(log_collector).
-compile(export_all).
-compile(nowarn_unused_vars).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    loop/0,
    put/4
    ]).

%% Server State
-record( state, {   cnt_err=0,
                    cnt_info=0,
                    cnt_warn=0,
                    connect_time=0
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
  State = #state{connect_time=utils:unix_timestamp()},
  Pid = spawn_link(?MODULE, loop,[]),
  yes = global:register_name(log_collector, Pid),
  {ok, State}.

put(Node, Pid, Type, Message)->
  gen_server:cast(?MODULE, {put, Node, Pid, Type, Message}).


loop()->
  receive
    {Pid, Node, Type, Message}->
      put(Node, Pid, Type, Message),
      loop()
  end.

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast({put, Node, Pid, Type, Message}, State) ->
      case Type of
        info->
          NewState = #state{cnt_info=State#state.cnt_info+1},
          case config:get(debug) of
            {ok, true}->
              error_logger:info_report({Node,Message});
            _Else->
              pass
          end;
        error->
          NewState = #state{cnt_info=State#state.cnt_err+1},
          error_logger:error_report({Node,Message});
        warning->
          NewState = #state{cnt_info=State#state.cnt_warn+1},
          error_logger:warning_report({Node,Message});
        _Else->
          NewState = #state{cnt_info=State#state.cnt_err+1},
          error_logger:error_report({Node,Message})
      end,
    {noreply, NewState};

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