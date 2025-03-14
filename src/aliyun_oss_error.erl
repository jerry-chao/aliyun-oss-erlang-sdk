%%%-------------------------------------------------------------------
%%% @doc Aliyun OSS error handling
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss_error).

-export([
    parse_error/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Parse error response from OSS
-spec parse_error(Response :: map()) -> map().
parse_error(#{body := Body} = Response) ->
    % In a real implementation, this would parse the XML error response
    % For simplicity, we're just returning the response with a generic error
    Response#{
        error_code => extract_error_code(Body),
        error_message => extract_error_message(Body),
        request_id => extract_request_id(Body)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Extract error code from XML response
-spec extract_error_code(Body :: binary()) -> binary().
extract_error_code(_Body) ->
    % In a real implementation, this would extract the error code from the XML
    <<"UnknownError">>.

%% @private Extract error message from XML response
-spec extract_error_message(Body :: binary()) -> binary().
extract_error_message(_Body) ->
    % In a real implementation, this would extract the error message from the XML
    <<"An unknown error occurred">>.

%% @private Extract request ID from XML response
-spec extract_request_id(Body :: binary()) -> binary().
extract_request_id(_Body) ->
    % In a real implementation, this would extract the request ID from the XML
    <<"00000000-0000-0000-0000-000000000000">>. 