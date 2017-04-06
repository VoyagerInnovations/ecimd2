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

-record(mo_part, {
  max_parts :: integer(),
  src_addr  :: iodata(),
  dst_addr  :: iodata(),
  messages  :: [iodata()]
}).

-record(conn_state, {
  host        :: iodata(),
  port        :: integer(),
  username    :: iodata(),
  password    :: iodata(),
  callback_mo :: {atom(), atom()},
  callback_dr :: {atom(), atom()},
  socket      :: port()
}).

-record(state, {
  connected   = false :: boolean(),
  packet_num  = 1     :: integer(),
  from_list   = #{}   :: map(),
  concat_list = #{}   :: map(),
  username    = <<>>  :: iodata(),
  password    = <<>>  :: iodata(),
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
  Username   = maps:get(username,    Opts, <<>>),
  Password   = maps:get(password,    Opts, <<>>),
  CallbackMO = maps:get(callback_mo, Opts, {ecimd2_dummy_receiver, mo}),
  CallbackDR = maps:get(callback_dr, Opts, {ecimd2_dummy_receiver, dr}),
  {ok, #conn_state{
    host        = Host,
    port        = Port,
    username    = Username,
    password    = Password,
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
%% @private is_connected callback
%% ----------------------------------------------------------------------------
handle_call(is_connected,  _From, #state{connected = Connected} = State) ->
  {reply, {status, Connected}, State};

%% ----------------------------------------------------------------------------
%% @private Default handle_call callback
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% ----------------------------------------------------------------------------
%% @private login handler
%% ----------------------------------------------------------------------------
handle_cast({login, Username, Password}, #state{socket=Socket} = State) ->
  PNum          = <<"001">>,
  {pdu, Packet} = ecimd2_pdu:login(PNum, Username, Password),
  send(Socket, Packet),
  {noreply, State#state{
    packet_num = PNum
  }};

%% ----------------------------------------------------------------------------
%% @private submit_response handler for successful submit
%% ----------------------------------------------------------------------------
handle_cast({submit_response, ok, PNum, Params}, 
                                        #state{from_list=Clients} = State) ->
  Client     = maps:get(PNum, Clients, '__undefined__'),
  DstAddress = maps:get(<<"021">>, Params, <<>>),
  Timestamp  = maps:get(<<"060">>, Params, <<"0">>),
  MessageId  = <<Timestamp/binary, DstAddress/binary>>,
  gen_server:reply(Client, {message_id, MessageId}),
  {noreply, State#state{
    from_list = maps:remove(PNum, Clients)
  }};

%% ----------------------------------------------------------------------------
%% @private submit_response handler for successful submit
%% ----------------------------------------------------------------------------
handle_cast({submit_response, Status, PNum, _Params}, 
                                        #state{from_list=Clients} = State) ->
  Client = maps:get(PNum, Clients, '__undefined__'),
  gen_server:reply(Client, {error, Status}),
  {noreply, State#state{
    from_list = maps:remove(PNum, Clients)
  }};

%% ----------------------------------------------------------------------------
%% @private login_response handler for successful login
%% ----------------------------------------------------------------------------
handle_cast({login_response, ok, _PNum, _Params}, State) ->
  keepalive(20000),
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
%% @private alive_response handler for successful alive
%% ----------------------------------------------------------------------------
handle_cast({alive_response, ok, _PNum, _Params}, State) ->
  keepalive(20000),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private alive_response handler for unsuccessful alive
%% ----------------------------------------------------------------------------
handle_cast({alive_response, Status, _PNum, _Params}, State) ->
  io:format(standard_error, "[alive_response] ~p", [Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_message_parse handler. For short MO messages
%% ----------------------------------------------------------------------------
handle_cast({deliver_concat, <<>>, SrcAddr, DstAddr, Message}, 
                            #state{callback_mo={Mod, Fun}} = State) ->
  spawn(Mod, Fun, [SrcAddr, DstAddr, Message]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_message_parse handler. For short MO messages
%% ----------------------------------------------------------------------------
handle_cast({deliver_concat, HexUDH, SrcAddr, DstAddr, Message}, 
                            #state{concat_list=ConcatMap} = State) ->
  <<_Misc:24, Ref:8, Parts:8, Part:8>> = from_hexstr(HexUDH),
  RefBin       = integer_to_binary(Ref),
  KeyBin       = <<SrcAddr/binary, ":", DstAddr/binary, ":", RefBin/binary>>,
  MOPart       = #mo_part{
    max_parts  = Parts,
    src_addr   = SrcAddr,
    dst_addr   = DstAddr,
    messages   = []
  },
  CurrMOPart   = maps:get(KeyBin, ConcatMap, MOPart),
  CurrPairs    = CurrMOPart#mo_part.messages,
  PartPair     = {Part, Message},
  NewMOPart    = CurrMOPart#mo_part{
    messages   = CurrPairs ++ [PartPair]
  },
  NewConcatMap = maps:put(KeyBin, NewMOPart, ConcatMap),
  gen_server:cast(self(), {chopped_mo, KeyBin, NewMOPart}),
  {noreply, State#state{
    concat_list = NewConcatMap
  }};

%% ----------------------------------------------------------------------------
%% @private Chopped MO event (complete)
%% ----------------------------------------------------------------------------
handle_cast({chopped_mo, Key, #mo_part{max_parts=MaxParts, src_addr=SrcAddr,
                                       dst_addr=DstAddr, messages=Messages}},
                              #state{concat_list=ConcatMap} = State)
                                when length(Messages) >= MaxParts ->
  %% Note to self: you can probably sort on the fly
  SortedMessages = lists:keysort(1, Messages),
  Message        = lists:foldl(fun({_Part, MsgPart}, Acc) ->
    <<Acc/binary, MsgPart/binary>>
  end, <<>>, SortedMessages),
  gen_server:cast(self(), {deliver_concat, <<>>, SrcAddr, DstAddr, Message}),
  NewConcatMap   = maps:remove(Key, ConcatMap),
  {noreply, State#state{
    concat_list = NewConcatMap
  }};

%% ----------------------------------------------------------------------------
%% @private Chopped MO event (incomplete)
%% ----------------------------------------------------------------------------
handle_cast({chopped_mo, _Key, _MOPart}, State) ->
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_message PDU
%% ----------------------------------------------------------------------------
handle_cast({deliver_message, ok, PNum, Params}, 
                           #state{socket=Socket} = State) ->
  Message       = extract_message(Params),
  UDH           = maps:get(<<"032">>, Params, <<>>),
  SrcAddr       = maps:get(<<"023">>, Params, <<>>),
  DstAddr       = maps:get(<<"021">>, Params, <<>>),
  {pdu, Packet} = ecimd2_pdu:deliver_message_response(PNum),
  send(Socket, Packet),
  gen_server:cast(self(), {deliver_concat, UDH, SrcAddr, DstAddr, Message}),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_message PDU
%% ----------------------------------------------------------------------------
handle_cast({deliver_message, Status, _PNum, _Params}, State) ->
  io:format(standard_error, "[deliver_message] Error: ~p", [Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_status_report PDU
%% ----------------------------------------------------------------------------
handle_cast({deliver_status_report, ok, PNum, Params}, 
                           #state{socket=Socket,
                                  callback_mo={Mod, Fun}} = State) ->
  Status        = maps:get(<<"061">>, Params, <<"0">>),
  Timestamp     = maps:get(<<"060">>, Params, <<"0">>),
  SrcAddr       = maps:get(<<"023">>, Params, <<>>),
  DstAddr       = maps:get(<<"021">>, Params, <<>>),
  MessageId     = <<Timestamp/binary, DstAddr/binary>>,
  {pdu, Packet} = ecimd2_pdu:deliver_status_report_response(PNum),
  send(Socket, Packet),
  spawn(Mod, Fun, [SrcAddr, DstAddr, MessageId, Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private deliver_status_report PDU
%% ----------------------------------------------------------------------------
handle_cast({deliver_status_report, Status, _PNum, _Params}, State) ->
  io:format(standard_error, "[deliver_status_report] Error: ~p", [Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private logout PDU
%% ----------------------------------------------------------------------------
handle_cast(logout, #state{socket=Socket, packet_num=PNum} = State) ->
  NewPNum       = increment(PNum),
  {pdu, Packet} = ecimd2_pdu:logout(NewPNum),
  send(Socket, Packet),
  {noreply, State#state{
    packet_num = NewPNum
  }};

%% ----------------------------------------------------------------------------
%% @private logout_response handler for successful logout
%% ----------------------------------------------------------------------------
handle_cast({logout_response, ok, _PNum, _Params}, State) ->
  {noreply, State#state{
    connected = false
  }};

%% ----------------------------------------------------------------------------
%% @private logout_response handler for unsuccessful logout
%% ----------------------------------------------------------------------------
handle_cast({logout_response, Status, _PNum, _Params}, State) ->
  io:format(standard_error, "[logout_response] ~p", [Status]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Greeting handler
%% ----------------------------------------------------------------------------
handle_cast({unknown_pdu, PDU}, #state{socket=Socket,
                                       username=Username,
                                       password=Password,
                                       callback_mo=CallbackMO,
                                       callback_dr=CallbackDR} = State) ->
  io:format("[greeting] ~s~n", [PDU]),
  gen_server:cast(self(), {login, Username, Password}),
  {noreply, State#state{
    socket      = Socket,
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }};

%% ----------------------------------------------------------------------------
%% @private handler unknown PDU
%% ----------------------------------------------------------------------------
handle_cast({unknown_pdu, PDU}, State) ->
  io:format(standard_error, "[unknown_pdu] ~p", [PDU]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Default handle_cast callback
%% ----------------------------------------------------------------------------
handle_cast(_Message, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @private Lazy initialization. This sleeps the process for one second then
%% attempts to connect to the MC
%% ----------------------------------------------------------------------------
handle_info(timeout, #conn_state{host=Host, port=Port,
                                 username=Username,
                                 password=Password,
                                 callback_mo=CallbackMO,
                                 callback_dr=CallbackDR}) ->
  timer:sleep(1000),
  {socket, Socket} = get_socket(Host, Port),
  {noreply, #state{
    socket      = Socket,
    username    = Username,
    password    = Password,
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }};

%% ----------------------------------------------------------------------------
%% @private alive handler on disconnected state
%% ----------------------------------------------------------------------------
handle_info(alive, #state{connected = false} = State) ->
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private alive packet sending callback 
%% ----------------------------------------------------------------------------
handle_info(alive, #state{socket=Socket, packet_num=PNum} = State) ->
  NewPNum       = increment(PNum),
  {pdu, Packet} = ecimd2_pdu:alive(NewPNum),
  send(Socket, Packet),
  {noreply, State#state{
    packet_num = NewPNum
  }};

%% ----------------------------------------------------------------------------
%% @private Network response callback
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, PDU}, State) ->
  gen_server:cast(self(), ecimd2_pdu:parse(PDU)),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Network disconnection
%% ----------------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
  exit(disconnected),
  {noreply, State#state{
    connected  = false,
    packet_num = 1
  }};

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
keepalive(Milliseconds) ->
  erlang:send_after(Milliseconds, self(), alive).

%% @private
get_socket(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  {socket, Socket}.

%% @private
send(Socket, Packet) ->
  ok = gen_tcp:send(Socket, Packet).

%% @private
increment(<<"255">>) -> <<"001">>;
increment(<<"254">>) -> <<"000">>;
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

%% @private
extract_message(#{<<"030">> := <<"8">>, <<"034">> := HexStr}) ->
  Message = from_hexstr(HexStr),
  unicode:characters_to_binary(Message, utf16, utf8);
extract_message(#{<<"033">> := Message}) ->
  Message.

%% @private
from_hexstr(HexStr) ->
  from_hexstr(binary_to_list(HexStr), []).
from_hexstr([], Acc) ->
  list_to_binary(lists:reverse(Acc));
from_hexstr([B1, B2 | Tail], Acc) ->
  {ok, [Char], []} = io_lib:fread("~16u", [B1, B2]),
  from_hexstr(Tail, [Char | Acc]);
from_hexstr([B | Tail], Acc) ->
  {ok, [Char], []} = io_lib:fread("~16u", lists:flatten([B, "0"])),
  from_hexstr(Tail, [Char | Acc]).
