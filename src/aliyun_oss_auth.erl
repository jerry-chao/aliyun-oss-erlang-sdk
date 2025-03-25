%%%-------------------------------------------------------------------
%%% @doc Aliyun OSS authentication module
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss_auth).

-export([
    new/2,
    new/3,
    sign_request/3
]).

-record(auth, {
    access_key_id :: binary(),
    access_key_secret :: binary(),
    security_token = undefined :: binary() | undefined
}).

-type auth() :: #auth{}.
-export_type([auth/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Create a new auth object with access key ID and secret
-spec new(AccessKeyId :: binary() | string(), AccessKeySecret :: binary() | string()) -> auth().
new(AccessKeyId, AccessKeySecret) ->
    #auth{
        access_key_id = to_binary(AccessKeyId),
        access_key_secret = to_binary(AccessKeySecret)
    }.

%% @doc Create a new auth object with access key ID, secret and security token
-spec new(AccessKeyId :: binary() | string(), AccessKeySecret :: binary() | string(), SecurityToken :: binary() | string()) -> auth().
new(AccessKeyId, AccessKeySecret, SecurityToken) ->
    #auth{
        access_key_id = to_binary(AccessKeyId),
        access_key_secret = to_binary(AccessKeySecret),
        security_token = to_binary(SecurityToken)
    }.

%% @doc Sign an HTTP request with the auth credentials
-spec sign_request(Auth :: auth(), Method :: atom(), Headers :: [{binary(), binary()}]) -> [{binary(), binary()}].
sign_request(Auth, Method, Headers) ->
    % Check if Date header exists, if not, use current time
    Date = case proplists:get_value(<<"Date">>, Headers) of
        undefined ->
            % Get current time in local timezone
            Now = aliyun_oss_timezone:get_local_time(),
            DateStr = httpd_util:rfc1123_date(Now),
            DateBin = list_to_binary(DateStr),
            [{<<"Date">>, DateBin} | Headers];
        _ ->
            Headers
    end,
    
    % Add security token if present
    HeadersWithToken = case Auth#auth.security_token of
        undefined ->
            Date;
        Token ->
            [{<<"x-oss-security-token">>, Token} | Date]
    end,
    
    % Create string to sign
    StringToSign = create_string_to_sign(Method, HeadersWithToken),
    
    % Sign the string using crypto module
    Signature = base64:encode(
        crypto:mac(hmac, sha, Auth#auth.access_key_secret, StringToSign)
    ),
    
    % Add Authorization header
    AuthHeader = <<"OSS ", (Auth#auth.access_key_id)/binary, ":", Signature/binary>>,
    [{<<"Authorization">>, AuthHeader} | HeadersWithToken].

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Convert string to binary if needed
-spec to_binary(Value :: binary() | string()) -> binary().
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value).

%% @private Create the string to sign for the request
-spec create_string_to_sign(Method :: atom(), Headers :: [{binary(), binary()}]) -> binary().
create_string_to_sign(Method, Headers) ->
    MethodStr = string:uppercase(atom_to_list(Method)),
    
    % Extract relevant headers
    ContentMD5 = proplists:get_value(<<"Content-MD5">>, Headers, <<"">>),
    ContentType = proplists:get_value(<<"Content-Type">>, Headers, <<"">>),
    Date = proplists:get_value(<<"Date">>, Headers, <<"">>),
    
    % Get canonicalized OSS headers
    CanonicalizedOSSHeaders = canonicalize_oss_headers(Headers),
    
    % Get canonicalized resource
    CanonicalizedResource = canonicalize_resource(Headers),
    
    % Combine all parts with proper newlines
    StringToSign = <<
        (list_to_binary(MethodStr))/binary, "\n",
        ContentMD5/binary, "\n",
        ContentType/binary, "\n",
        Date/binary, "\n",
        CanonicalizedOSSHeaders/binary,
        CanonicalizedResource/binary
    >>,
    
    StringToSign.

%% @private Canonicalize OSS headers
-spec canonicalize_oss_headers(Headers :: [{binary(), binary()}]) -> binary().
canonicalize_oss_headers(Headers) ->
    % Filter headers that start with x-oss-
    OSSHeaders = lists:filter(
        fun({Key, _}) ->
            binary:match(Key, <<"x-oss-">>) =:= {0, 6}
        end,
        Headers
    ),
    
    % Sort headers by key
    SortedHeaders = lists:sort(OSSHeaders),
    
    % Format headers
    FormattedHeaders = lists:map(
        fun({Key, Value}) ->
            LowerKey = string:lowercase(binary_to_list(Key)),
            <<(list_to_binary(LowerKey))/binary, ":", Value/binary, "\n">>
        end,
        SortedHeaders
    ),
    
    % Combine all headers
    list_to_binary(FormattedHeaders).

%% @private Canonicalize resource
-spec canonicalize_resource(Headers :: [{binary(), binary()}]) -> binary().
canonicalize_resource(Headers) ->
    % Get bucket and object from headers
    Bucket = proplists:get_value(<<"x-oss-bucket">>, Headers),
    Object = proplists:get_value(<<"x-oss-object">>, Headers),
    
    % Build canonicalized resource
    case {Bucket, Object} of
        {undefined, _} ->
            <<"/">>;
        {Bucket, undefined} ->
            <<"/", Bucket/binary>>;
        {Bucket, Object} ->
            <<"/", Bucket/binary, "/", Object/binary>>
    end. 