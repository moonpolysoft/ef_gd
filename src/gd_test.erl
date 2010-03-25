-module(gd_test).

-export([test/0]).

test() ->
  {ok, Bin} = file:read_file("priv/riak_logo.jpg"),
  {ok, Gd} = gd:read(Bin, "image/jpg"),
  gd:resize(Gd, 700),
  {ok, Bin2} = gd:blob(Gd, 100),
  Port = open_port({spawn, "open -f -a preview"}, [binary, stream, use_stdio, out]),
  port_command(Port, Bin2),
  unlink(Port),
  port_close(Port).
  