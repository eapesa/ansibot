-module(util_io).

-export([
  find_map/3
]).

find_map(_, _, []) -> false;
find_map(MapKey, MapValue, [ H | T ]) ->
  Get = maps:get(MapKey, H, false),
  case Get of
    false -> find_map(MapKey, MapValue, T);
    MapValue -> H;
    _ -> find_map(MapKey, MapValue, T)
  end.