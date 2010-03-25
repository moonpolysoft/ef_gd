-module(gd).
-author('cliff@moonpolysoft.com').

-export([read/2, size/1, resize/3, resize/2, crop/3, intelligent_resize/2, padded_resize/2, crop_resize/2, blob/2, close/1]).

-define(SIZE, $s).
-define(READ, $r).
-define(RESIZE, $e).
-define(CROP, $c).
-define(BLOB, $b).
-define(JPG, $j).
-define(GIF, $g).
-define(PNG, $p).

read(Bin, Fmt) ->
  case load_driver() of
    ok ->
      P = open_port({spawn, 'gd_drv'}, [binary]),
      port_command(P, [?READ, format(Fmt), Bin]),
      Result = receive
        {P, {data, Data}} -> binary_to_term(Data)
      end,
      case Result of 
        ok -> {ok, {gd, P}};
        Err -> Err
      end;
    {error, Err} ->
      Msg = erl_ddll:format_error(Err),
      {error, Msg}
  end.
  
size({gd, P}) ->
  port_command(P, [?SIZE]),
  receive
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
resize({gd, P}, Width, Height) ->
  port_command(P, [?RESIZE, term_to_binary({Width, Height})]),
  receive
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
resize(Gd, Width) ->
  Height = height_from_width(Gd, Width),
  resize(Gd, Width, Height).
  
crop({gd, P}, Width, Height) ->
  port_command(P, [?CROP, term_to_binary({Width, Height})]),
  receive
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
padded_resize(Gd, Size) ->
  {W, H} = gd:size(Gd),
  {RW, RH} = if
    W > H ->
      {Size, trunc(Size / (W / H))};
    true ->
      {trunc(Size / (H / W)), Size}
  end,
  % ?debugFmt("rw ~p rh ~p~n", [RW, RH]),
  ok = resize(Gd, RW, RH),
  crop(Gd, Size, Size).

crop_resize(Gd, Size) ->
  {W, H} = gd:size(Gd),
  ok = if
    W > H ->
      crop(Gd, H, H);
    true ->
      crop(Gd, W, W)
  end,
  resize(Gd, Size, Size).

intelligent_resize(Gd, Size) ->
  {W, H} = gd:size(Gd),
  Aspect = if
    W > H -> H / W;
    true -> W / H
  end,
  if
    Aspect > 0.9 -> crop_resize(Gd, Size);
    true -> padded_resize(Gd, Size)
  end.

blob({gd, P}, Quality) when is_integer(Quality) ->
  port_command(P, [?BLOB, term_to_binary(Quality)]),
  receive
    {P, ok, Blob} -> {ok, Blob};
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
close({gd, P}) ->
  unlink(P),
  port_close(P).

%%====================================================================
%% Internal functions
%%====================================================================
load_driver() ->
  Dir = filename:join([filename:dirname(code:which(gd)), "..", "priv"]),
  erl_ddll:load(Dir, "gd_drv").

height_from_width(Gd, Width) ->
  {W, H} = gd:size(Gd),
  Ratio = W / H,
  trunc(Width / Ratio).

format("image/png") -> ?PNG;
format("image/jpeg") -> ?JPG;
format("image/pjpeg") -> ?JPG;
format("image/jpg") -> ?JPG;
format("image/gif") -> ?GIF.
