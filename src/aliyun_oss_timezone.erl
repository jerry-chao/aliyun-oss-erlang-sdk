-module(aliyun_oss_timezone).

-export([
    set_timezone/1,
    get_timezone/0,
    get_local_time/0,
    get_local_time_with_offset/1
]).

%% @doc Set the timezone for the application
-spec set_timezone(string()) -> ok.
set_timezone(Zone) ->
    % Set TZ environment variable
    os:putenv("TZ", Zone),
    % Force timezone update
    calendar:local_time(),
    ok.

%% @doc Get current timezone
-spec get_timezone() -> string().
get_timezone() ->
    case os:getenv("TZ") of
        false -> "UTC";
        Zone -> Zone
    end.

%% @doc Get current local time
-spec get_local_time() -> calendar:datetime().
get_local_time() ->
    calendar:local_time().

%% @doc Get local time with offset
-spec get_local_time_with_offset(integer()) -> calendar:datetime().
get_local_time_with_offset(OffsetSeconds) ->
    LocalTime = calendar:local_time(),
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(LocalTime) + OffsetSeconds
    ). 