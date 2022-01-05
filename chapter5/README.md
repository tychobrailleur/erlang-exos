To run this:

```
$ ./rebar get-deps
$ ./rebar compile
$ erl -pa ebin/ -pa deps/jsx/ebin/
Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V7.2  (abort with ^G)
1> json_config:test().
tests_worked
```
