%% date utilities
-module(dateutils).

-export([
	now/0,
	ts_to_dt/1,
	dt_format/1]).

%% @doc Returns seconds sincen epoch
-spec now() -> non_neg_integer().
now() ->
    {MegaSecs, Secs, _Microsecs} = os:timestamp(),
    integer_to_list(MegaSecs*1000000 + Secs).

%% Converts timestamp (seconds since epoch)
%% to Date Time.
%% Returns {{Y, M, D}, {h, m, s}}
ts_to_dt(Seconds) when is_number(Seconds) ->
   BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   GregorianSeconds = BaseDate + Seconds,
   calendar:gregorian_seconds_to_datetime(GregorianSeconds);
ts_to_dt(Seconds) when is_list(Seconds) ->
    {Sec, _} = string:to_integer(Seconds),
    ts_to_dt(Sec).

%% Format a DateTime tuple to a string
%% YYYY/MM/DD HH:MM:SS
dt_format(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    list_to_binary(io_lib:format("~4.10.0B/~2.10.0B/~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).
