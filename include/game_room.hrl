
-record (game_room_type, {
    type = 0 :: term(),                                 %% 房间大类型
    pid = undefined :: undefined | pid(),               %% 监督进程
    handle_sup_pid = undefined :: undefined | pid(),    %% 房间监督进程
    manager_pid = undefined :: undefined | pid(),       %% 房间管理进程
    table = undefined :: term()                         %% 房间索引
}).

-record (game_room, {
    id = 0 :: integer(),                    %% 房间id
    pid = undefined :: undefined | pid(),   %% 房间进程
    type = 0 :: term()                      %% 房间类型
}).