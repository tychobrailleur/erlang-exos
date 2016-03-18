
## Install

## Execution

To start the server:

```
erl -sname seb

(seb@pyjama)1> lib_chan:start_server().
lib_chan starting:"/home/sebastien/.erlang_config/lib_chan.conf"
ConfigData=[{port,9994},
            {service,yafs,password,"password",mfa,yafs,start_yafs,notUsed}]
true

```


Second session:

```
erl -sname seb2

(seb2@pyjama)1> {ok, Pid} = lib_chan:connect("localhost", 9994, yafs, "password", "").
{ok,<0.41.0>}
(seb2@pyjama)2> lib_chan:rpc(Pid, { self(), list_dir }).
{<7317.56.0>,
 {ok,["any_apply.beam","lib_chan.erl","lib_chan_auth.erl",
      "lib_chan_cs.erl","kvs.beam","lib_md5.erl",
      "lib_chan_test.erl","lib_chan_mm.erl","lib_chan_cs.beam",
      "lib_chan_test.beam","README.md","io_widget.erl",
      "lib_chan_mm.beam","kvs.erl","io_widget.beam",
      "lib_chan_auth.beam","any_apply.erl","Makefile",
      "erl_crash.dump","yafs.erl","lib_md5.beam","yafs.beam",
      "lib_chan.beam"]}}
(seb2@pyjama)3> lib_chan:rpc(Pid, { self(), { get_file, "lib_chan.erl" } }).
{<7317.56.0>,
 {ok,<<"%% ---\n%%  Excerpted from \"Programming Erlang\",\n%%  published by The Pragmatic Bookshelf.\n%%  Copyri"...>>}}
(seb2@pyjama)4>



```
