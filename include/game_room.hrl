
-record (game_room_type, {
  type = 0 :: term(),         %% 房间类型
  pid = undefined :: pid()    %% 房间管理进程
}).

-record (game_room, {
  id = 0 :: integer(),        %% 房间id
  num = 0 :: integer(),       %% 房间人数
  pid = undefined :: pid(),   %% 房间进程
  type = 0 :: term()          %% 房间类型
}).