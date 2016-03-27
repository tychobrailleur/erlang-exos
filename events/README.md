Attempt at writing Reminder app as described here:
http://learnyousomeerlang.com/designing-a-concurrent-application

without looking at reference implementation.

To execute:

```
erl -make
erl -pa ebin/
```

And run:

```
Pid = client:test().
```


Can then cancel long event:

```
client:cancel(Pid, "Test2").
```
