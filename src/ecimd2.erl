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
  start_link/1,
  send_sms/6,
  logout/1,
  is_connected/1
]).

%% @doc Starts connection to the SMSC and logs in with the given
%% credentials
%%
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

%% @doc Sends a message to the specified MSISDN
%%
%% `Options' is an optional map that may contain the following keys:
%%
%% <dt><b>`cancellable'</b></dt>
%% <dd>Determines if the message can be cancelled or not</dd>
%% <dt><b>`tariff_class'</b></dt>
%% <dd>Tariff code to be used for the message. This is usually MC specific</dd>
%% <dt><b>`service_desc'</b></dt>
%% <dd>Service description to be used for the message. 
%%     This is usually MC specific</dd>
%% <dt><b>`status_report'</b></dt>
%% <dd>Flag of the cases when the status report should be returned.
%%     See <a href="#status_report">status report flags</a></dd>
%% <dt><b>`priority'</b></dt>
%% <dd>Priorty of the message (0-9). Lower value means higher priority</dd>
%%
%% <div id="status_report">Status Report Flag values:</div>
%% <dd>
%%   <p>`1   - Temporary error'</p>
%%   <p>`2   - Validity period expired'</p>
%%   <p>`4   - Delivery failed'</p>
%%   <p>`8   - Delivery successful'</p>
%%   <p>`16  - Message cancelled'</p>
%%   <p>`32  - Message deleted by operator'</p>
%%   <p>`64  - First temporary result'</p>
%%   <p>`128 - Reserved'</p>
%% </dd>
-spec send_sms(pid(), iodata(), iodata(), iodata(), iodata(), Options) -> [{message_id, iodata()}] | {error, atom()}
  when Options :: #{ cancellable   => boolean()  ,
                     tariff_class  => term()     ,
                     service_desc  => term()     ,
                     status_report => integer()  ,
                     priority      => integer() }.
send_sms(C, AccessCode, Sender, Destination, Message, Options) ->
  DataCoding     = get_data_coding(Message),
  EncodedMessage = encode(ecimd2_format:ensure_binary(Message), DataCoding),
  MessageMap     = #{
    dst_address  => Destination,
    src_address  => AccessCode,
    src_alpha    => Sender,
    data_coding  => DataCoding,
    message      => EncodedMessage
  },
  submit_msg(C, maps:merge(Options, MessageMap), EncodedMessage, DataCoding).
  
%% @doc Logout the session
-spec logout(pid()) -> ok.
logout(C) ->
  gen_server:cast(C, logout).

%% @doc Check if connected to server
-spec is_connected(pid()) -> boolean().
is_connected(C) ->
  gen_server:call(C, is_connected).

%% ----------------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------------

%% @private
submit_msg(C, Map, Message, DataCoding)
                       when DataCoding =:= 0, size(Message) =< 160 ->
  [gen_server:call(C, {submit, Map})];
submit_msg(C, Map, Message, DataCoding)
                       when DataCoding =:= 8, size(Message) =< 140 ->
  NewMap  = maps:remove(message, Map),
  HexMsg  = to_hexstr(Message),
  NewMap2 = maps:put(message_bin, HexMsg, NewMap),
  [gen_server:call(C, {submit, NewMap2})];
submit_msg(C, Map, Message, DataCoding) when DataCoding =:= 0 ->
  Ref = random:uniform(255),
  Size = size(Message),
  Parts = ceil(Size / 153),
  submit_msg(C, Map, Message, <<>>, Ref, 1, Parts, 153, message, []);
submit_msg(C, Map, Message, DataCoding) when DataCoding =:= 8 ->
  Ref = random:uniform(255),
  Size = size(Message),
  Parts = ceil(Size / 134),
  submit_msg(C, Map, Message, <<>>, Ref, 1, Parts, 134, message_bin, []).

%% @private
submit_msg(_C, _Map, <<>>, _Tail, _Ref, _Part, _Parts, _Limit, _Key, Acc) ->
  Acc;
submit_msg(C, Map, Str, _Tail, Ref, Part, Parts, Limit, _Key, Acc)
                                             when size(Str) > Limit ->
  {BinPart, BinTail} = chop(Str, Limit),
  submit_msg(C, Map, BinPart, BinTail, Ref, Part, Parts, Limit, _Key, Acc);
submit_msg(C, Map, Str, Tail, Ref, Part, Parts, Limit, Key, Acc) ->
  UDH     = <<5, 0, 3, Ref, Parts, Part>>,
  HexUDH  = to_hexstr(UDH),
  NewMap  = maps:put(udh, HexUDH, Map),
  NewMap2 = maps:put(Key, Str, NewMap),
  NewAcc  = Acc ++ [gen_server:call(C, {submit, NewMap2})],
  submit_msg(C, NewMap2, Tail, <<>>, Ref, Part + 1, Parts, Limit, Key, NewAcc).

%% @private
chop(Str, 153 = Limit) ->
  <<TmpPart:Limit/binary, TmpTail/binary>> = Str,
  {TmpPart, TmpTail};
chop(Str, Limit) ->
  <<TmpPart:Limit/binary, TmpTail/binary>> = Str,
  Utf8 = unicode:characters_to_binary(TmpPart, utf16),
  analyze_unicode(Utf8, Str, Limit, TmpPart, TmpTail).

%% @private
analyze_unicode({incomplete, _U1, _U2}, Str, Limit, _TmpPart, _TmpTail) ->
  chop(Str, Limit - 2);
analyze_unicode({error, _U1, _U2}, Str, Limit, _TmpPart, _TmpTail) ->
  chop(Str, Limit - 2);
analyze_unicode(_Ok, _Str, _Limit, TmpPart, TmpTail) ->
  {TmpPart, TmpTail}.
  
%% @private
start_link('__undefined__', Module, Args) ->
  gen_server:start_link(Module, Args, []); 
start_link(Name, Module, Args) ->
  gen_server:start_link(Name, Module, Args, []).

%% @private
get_data_coding(Message) ->
  BinMessage = ecimd2_format:ensure_binary(Message), 
  is_basic_latin([N || <<N:1/binary>> <= BinMessage]).

%% @private
is_basic_latin([]) -> 0;
is_basic_latin([Char | Rest]) ->
  case binary:decode_unsigned(Char) of
    Val when Val < 128, Val =/= 96 ->
      is_basic_latin(Rest);
    _Val ->
      8
  end.

%% @private
encode(Message, 0) ->
  gsm0338:from_utf8(Message);
encode(Message, 8) ->
  unicode:characters_to_binary(Message, utf8, utf16).

%% @private
to_hexstr(Bin) ->
  Hex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]),
  ecimd2_format:ensure_binary(Hex).

%% @private
ceil(X) when X < 0 ->
  trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true  -> T;
    false -> T + 1
  end.
