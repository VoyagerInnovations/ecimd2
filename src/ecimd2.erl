%% @copyright 2017 Voyager Innovations Inc.
%% @doc CIMD2 Client
%%
%% Exposes a start_link/1 function that accepts login parameters
%% and makes connection to the Nokia SMSC. This starts an internal
%% gen_server and returns a reference for that particular process
%%
%% This is also the main module interface for this library. This will
%% also expose the functions for sending sms, retrieving delivery
%% receipts and registering callbacks
-module(ecimd2).

-export([
  start_link/1
]).

%% @doc Starts connection to the SMSC and logs in with the given
%% credentials

%% `Options' is a map that should contain the following keys:
%%
%% <dt><b>`name'</b></dt>
%% <dd>If provided, the internal gen_server will be registered with this name.
%%     see gen_server:start_link/4</dd>
%% <dt><b>`callback_mo'</b></dt>
%% <dd>Tuple of module and function atoms to be executed when a mobile
%%     originating message has been received</dd>
%% <dt><b>`callback_dr'</b></dt>
%% <dd>Tuple of module and function atoms to be executed when a delivery 
%%     receipt has been received</dd>
%% <dt><b>`host'</b></dt>
%% <dd>Hostname or IP address of the Nokia MC</dd>
%% <dt><b>`port'</b></dt>
%% <dd>Port of the Nokia MC</dd>
%% <dt><b>`username'</b></dt>
%% <dd>Username of the account to login</dd>
%% <dt><b>`password'</b></dt>
%% <dd>Password used to authenticate with the username</dd>
-spec start_link(Options) -> {ok, pid()} | ignore | {error, term()}
  when Options    :: #{ name        => ServerName ,
                        callback_mo => Callback   ,
                        callback_dr => Callback   ,
                        host        => iodata()   ,
                        port        => integer()  ,
                        username    => iodata()   ,
                        password    => iodata()  },
       Callback   :: {atom(), atom()},
       ServerName :: {local,  Name       :: atom()} |
                     {global, GlobalName :: term()} |
                     {via,    Module     :: atom(), ViaName :: term()}.
start_link(Options) ->
  Name = maps:get(name, Options, '__undefined__'),
  start_link(Name, ecimd2_worker, [Options]).
  
%% @private
start_link('__undefined__', Module, Args) ->
  gen_server:start_link(Module, Args, []); 
start_link(Name, Module, Args) ->
  gen_server:start_link(Name, Module, Args, []).
