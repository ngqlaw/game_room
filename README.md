game_room
=====

An game room process application

Build
-----

    $ rebar3 compile

Usage
-----
使用的时候需要回调模块Handler。
回调模块是gen_server类型的进程模块，必要的回调函数有:
``` 
-callback (start_link([Id::integer(), Type::term()]) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
```

API模块是game_room，以下是主要的函数说明:
启动房间进程服务，Type指定大类
```
-spec (start_server(Type::term(), Handler::atom()) -> boolean()).
```

新增房间，使用之前的指定大类Type，SubType指定房间的类型
```
-spec (new(Type::term(), SubType::term()) -> undefined | {RoomId::integer(), Pid::pid()} | term()).
```

获取符合条件的房间列表
例如：查询所有有人的房间 Cond :: fun(#game_room{id = Id, num = Num}) when Num > 0 -> Id end.
```
-spec (select(Type::term(), Cond::function()) -> list()).
-spec (select_n(Type::term(), Continuation::term()) -> none | {list(), term()}).
-spec (select_n(Type::term(), Cond::function(), N::integer()) -> none | {list(), term()}).
```

新增索引
```
-spec (insert(Type::term(), Info::tuple()) -> boolean()).
```

移除房间
```
-spec (delete(Type::term(), Key::integer()) -> boolean()).
```

更新房间人数
```
-spec (incr(Type::term(), Key::integer(), Incr::integer()) -> undefined | integer()).
```

获取房间进程信息
```
-spec (get_pid(Type::term(), Key::integer()) -> undefined | {pid(), integer()}).
```
