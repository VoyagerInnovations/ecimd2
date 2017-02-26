%% @private Manages the states and TCP connection to the Nokia MC
-module(ecimd2_worker).

-behaviour(gen_server).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(conn_state, {
  host        :: iodata(),
  port        :: integer(),
  username    :: iodata(),
  password    :: iodata(),
  callback_mo :: {atom(), atom()},
  callback_dr :: {atom(), atom()}
}).

-record(state, {
  connected   = false :: boolean(),
  packet_num  = 1     :: integer(),
  from_list   = #{}   :: map(),
  callback_mo         :: {atom(), atom()},
  callback_dr         :: {atom(), atom()},
  socket              :: port()
}).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @private Entry point. Note that another record is used here compared
%% to the rest of the callbacks. This is to minimize footprint as these
%% fields are only used in the lazy initialization
%% ----------------------------------------------------------------------------
init([Opts]) ->
  Host       = maps:get(host,        Opts, <<"localhost">>),
  Port       = maps:get(port,        Opts, 7777),
  CallbackMO = maps:get(callback_mo, Opts, {esmpp_dummy_receiver, mo}),
  CallbackDR = maps:get(callback_dr, Opts, {esmpp_dummy_receiver, dr}),
  {ok, #conn_state{
    host        = Host,
    port        = Port,
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }, 0}.

%% ----------------------------------------------------------------------------
%% @private submit operation - not connected
%% ----------------------------------------------------------------------------
handle_call({submit, _Message}, _From, #state{connected=false} = State) ->
  {reply, {error, not_connected}, State};

%% ----------------------------------------------------------------------------
%% @private submit operation - connected
%% ----------------------------------------------------------------------------
handle_call({submit, Message}, From,
             #state{socket=Socket, packet_num=PNum, 
                    from_list=Clients} = State) ->
  NewPNum = increment(PNum),
  {pdu, Packet} = ecimd2_pdu:submit(NewPNum, Message),
  send(Socket, Packet),
  NewClients = maps:put(NewPNum, From, Clients),
  {noreply, State#state{
    from_list  = NewClients,
    packet_num = NewPNum
  }};

%% ----------------------------------------------------------------------------
%% @private Default handle_call callback
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% ----------------------------------------------------------------------------
%% @private login_response handler for successful login
%% ----------------------------------------------------------------------------
handle_cast({login_response, ok, _PNum, _Params}, State) ->
  {noreply, State#state{
    connected = true
  }};

%% ----------------------------------------------------------------------------
%% @private login_response handler for unsuccessful login
%% ----------------------------------------------------------------------------
handle_cast({login_response, Status, _PNum, _Params}, State) ->
  io:format(standard_error, "[login_response] ~p", [Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Default handle_cast callback
%% ----------------------------------------------------------------------------
handle_cast(_Message, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @private Lazy initialization. This sleeps the process for one second then
%% attempts to login to the Nokia MC
%% ----------------------------------------------------------------------------
handle_info(timeout, #conn_state{host=Host, port=Port,
                                 username=Username,
                                 password=Password,
                                 callback_mo=CallbackMO,
                                 callback_dr=CallbackDR}) ->
  timer:sleep(1000),
  {pdu,    Packet} = ecimd2_pdu:login(1, Username, Password),
  {socket, Socket} = get_socket(Host, Port),
  send(Socket, Packet),
  {noreply, #state{
    packet_num  = 1,
    socket      = Socket, 
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }};

%% ----------------------------------------------------------------------------
%% @private Network response callback
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, PDU}, State) ->
  gen_server:cast(self(), ecimd2_pdu:parse(PDU)),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Default handle_info handler
%% ----------------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @private Default terminate handler
%% ----------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%% ----------------------------------------------------------------------------
%% @private Default code_change handler
%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ----------------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------------

%% @private
get_socket(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  {socket, Socket}.

%% @private
send(Socket, Packet) ->
  ok = gen_tcp:send(Socket, Packet).

%% @private
increment(PNum) ->
  IntPNum = binary_to_integer(PNum),
  NewIntPNum = IntPNum + 2,
  NewPNum = integer_to_binary(NewIntPNum),
  binpad(NewPNum, 3).

%% @private
binpad(Bin, Length) when size(Bin) >= Length ->
  Bin;
binpad(Bin, Length) ->
  NewBin = <<"0", Bin/binary>>,
  binpad(NewBin, Length).
