# ecimd2 - Erlang/OTP CIMD2 Client

[![Build Status](https://travis-ci.org/VoyagerInnovations/ecimd2.svg?branch=master)](https://travis-ci.org/VoyagerInnovations/ecimd2) [![Hex.pm](https://img.shields.io/hexpm/v/ecimd2.svg)](https://hex.pm/packages/ecimd2)

**ecimd2** connects erlang applications to Nokia SMSCs via the CIMD2 protocol.

## Supported Operations
* login
* submit
* deliver\_message
* deliver\_status\_report
* alive

Other commands will be supported in the future versions.

## OTP Version

**Required**: OTP 18 and later

## Setup

**ecimd2** can be added as a dependency via [hex.pm](https://hex.pm/packages/ecimd2)

```erlang
{deps, [
  {ecimd2, "0.0.7"}
]}. 
```

Then include **ecimd2** in your application's `.app.src` file

```erlang
{applications, [
  kernel,
  stdlib,
  ecimd2
]}.
```

## Usage

Calling `ecimd2:start_link/1` will start a connection to the SMSC. The following options are available inside a map as a parameter:

* `name` - If provided, the internal `gen_server` will be registered with this name. see `gen_server:start_link/4`
* `callback_mo` - Module and function tuple to be executed when a mobile originating message has been received
* `callback_dr` - Module and function tuple to be executed when a delivery receipt has been received
* `host` - Hostname or IP address of the Nokia MC
* `port` - Port of the Nokia MC
* `username` - Username of the account to login
* `password` - Password used to authenticate with the username

```erlang
{ok, C} = ecimd2:start_link(#{
   host => "127.0.0.1",
   port => 16001,
   username => "cimd2client",
   password => "password",
   callback_dr => {mymodule, myfunction}
}).
```

### Sending SMS

SMS messages are sent by calling `ecimd2:send_sms/6`. The function parameters are as follows:

* `Connection` - Process identifier of the `ecimd2` `gen_server` returned by `ecimd2:start_link/1`
* `AccessCode` - Access code assigned to the remote SMSC. This will serve as the originating address if there's no `Sender` defined
* `Sender` - Number or alphanumeric mask to be set as the sender of the message
* `Destination` - MSISDN that will be receiving the message
* `Message` - UTF-8 encoded message
* `Options` - An optional map with the following keys:
    * `cancellable` - Determines if the message can be cancelled or not
    * `tariff_class` - Tariff code to be used for the message. This is usually MC specific
    * `service_desc` - Service description to be used for the message. This is usually MC specific
    * `status_report` - Flag of the cases when the status report should be returned
        * `0`  - No status reports
        * `1`  - Temporary error
        * `2`  - Validity period expired
        * `4`  - Delivery failed
        * `8`  - Delivery successful
        * `16` - Message cancelled
        * `32` - Message deleted by operator
        * `64` - First temporary result
    * `priority` - Priorty of the message (0-9). Lower value means higher priority

```erlang
Ids = ecimd2:send_sms(C, <<"12345">>, <<"TestSender">>, <<"+639473371390">>, <<"Hello">>).
```

#### Return Type

The `send_sms` function will return a list of message id tuples:

```erlang
[{message_id, MessageId}]
```

`MessageId` is a (binary) string that was associated to the submitted message in the SMSC. Since CIMD2 lacks a message identifier in it's protocol, the `MessageId` returned in the function is a combination of timestamp and destination address from the `submit` operation response parameters.

#### UCS2/UTF-16 Support

Messages that are outside the standard GSM 03.38 character set are automatically detected and encoded with UTF-16. This includes emojis.

#### Long Messages

Long messages are automatically concatenated when they exceed the standard 140 byte limit. 

