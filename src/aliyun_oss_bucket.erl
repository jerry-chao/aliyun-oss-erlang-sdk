%%%-------------------------------------------------------------------
%%% @doc Aliyun OSS bucket operations
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss_bucket).

-export([
    new/3,
    new/4,
    put_object/3,
    get_object/2,
    delete_object/2,
    list_objects/1,
    list_objects/2
]).

-record(bucket, {
    auth :: aliyun_oss_auth:auth(),
    endpoint :: binary(),
    bucket_name :: binary(),
    region :: binary(),
    host :: binary()
}).

-type bucket() :: #bucket{}.
-export_type([bucket/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Create a new bucket object
-spec new(Auth :: aliyun_oss_auth:auth(), Endpoint :: binary() | string(), BucketName :: binary() | string()) -> bucket().
new(Auth, Endpoint, BucketName) ->
    new(Auth, Endpoint, BucketName, <<"oss-cn-hangzhou">>).

%% @doc Create a new bucket object with region
-spec new(Auth :: aliyun_oss_auth:auth(), Endpoint :: binary() | string(), BucketName :: binary() | string(), Region :: binary() | string()) -> bucket().
new(Auth, Endpoint, BucketName, Region) ->
    EndpointBin = to_binary(Endpoint),
    BucketNameBin = to_binary(BucketName),
    RegionBin = to_binary(Region),
    Host = <<BucketNameBin/binary, ".", (strip_protocol(EndpointBin))/binary>>,
    
    #bucket{
        auth = Auth,
        endpoint = EndpointBin,
        bucket_name = BucketNameBin,
        region = RegionBin,
        host = Host
    }.

%% @doc Put an object into the bucket
-spec put_object(Bucket :: bucket(), Key :: binary() | string(), Content :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
put_object(Bucket, Key, Content) ->
    % Convert key to binary if needed
    KeyBin = case is_binary(Key) of
        true -> Key;
        false -> list_to_binary(Key)
    end,
    
    % Calculate content MD5
    ContentMD5 = base64:encode(crypto:hash(md5, Content)),
    
    % Create headers with required fields
    Headers = [
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Content-MD5">>, ContentMD5},
        {<<"x-oss-bucket">>, Bucket#bucket.bucket_name},
        {<<"x-oss-object">>, KeyBin}
    ],
    
    % Sign request
    SignedHeaders = aliyun_oss_auth:sign_request(Bucket#bucket.auth, put, Headers),
    
    % Make the request
    Url = make_object_url(Bucket, KeyBin),
    case hackney:request(put, Url, SignedHeaders, Content, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, _Body} = hackney:body(ClientRef),
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders
            }};
        {ok, StatusCode, ResponseHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body
            }};
        Error ->
            Error
    end.

%% @doc Get an object from the bucket
-spec get_object(Bucket :: bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
get_object(Bucket, Key) ->
    KeyBin = to_binary(Key),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, Bucket#bucket.host}
    ],
    
    % Sign the request
    SignedHeaders = aliyun_oss_auth:sign_request(Bucket#bucket.auth, get, Headers0),
    
    % Make the request
    Url = make_object_url(Bucket, KeyBin),
    case hackney:request(get, Url, SignedHeaders, <<>>, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body
            }};
        {ok, StatusCode, ResponseHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body
            }};
        Error ->
            Error
    end.

%% @doc Delete an object from the bucket
-spec delete_object(Bucket :: bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
delete_object(Bucket, Key) ->
    KeyBin = to_binary(Key),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, Bucket#bucket.host}
    ],
    
    % Sign the request
    SignedHeaders = aliyun_oss_auth:sign_request(Bucket#bucket.auth, delete, Headers0),
    
    % Make the request
    Url = make_object_url(Bucket, KeyBin),
    case hackney:request(delete, Url, SignedHeaders, <<>>, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, _Body} = hackney:body(ClientRef),
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders
            }};
        {ok, StatusCode, ResponseHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body
            }};
        Error ->
            Error
    end.

%% @doc List objects in the bucket
-spec list_objects(Bucket :: bucket()) -> 
    {ok, map()} | {error, term()}.
list_objects(Bucket) ->
    list_objects(Bucket, []).

%% @doc List objects in the bucket with options
-spec list_objects(Bucket :: bucket(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
list_objects(Bucket, Options) ->
    % Prepare headers
    Headers0 = [
        {<<"Host">>, Bucket#bucket.host}
    ],
    
    % Sign the request
    SignedHeaders = aliyun_oss_auth:sign_request(Bucket#bucket.auth, get, Headers0),
    
    % Prepare query parameters
    QueryParams = build_list_objects_query(Options),
    
    % Make the request
    Url = <<(make_bucket_url(Bucket))/binary, QueryParams/binary>>,
    case hackney:request(get, Url, SignedHeaders, <<>>, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, Body} = hackney:body(ClientRef),
            % Parse XML response
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body,
                objects => parse_list_objects_response(Body)
            }};
        {ok, StatusCode, ResponseHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body
            }};
        Error ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Convert string to binary if needed
-spec to_binary(Value :: binary() | string()) -> binary().
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value).

%% @private Strip protocol from endpoint
-spec strip_protocol(Endpoint :: binary()) -> binary().
strip_protocol(Endpoint) ->
    case binary:match(Endpoint, <<"://">>) of
        {Pos, Len} ->
            binary:part(Endpoint, Pos + Len, byte_size(Endpoint) - Pos - Len);
        nomatch ->
            Endpoint
    end.

%% @private Get content type based on file extension
-spec get_content_type(Key :: binary(), Options :: list()) -> binary().
get_content_type(Key, Options) ->
    case proplists:get_value(content_type, Options) of
        undefined ->
            % Try to guess from file extension
            case filename:extension(binary_to_list(Key)) of
                ".jpg" -> <<"image/jpeg">>;
                ".jpeg" -> <<"image/jpeg">>;
                ".png" -> <<"image/png">>;
                ".gif" -> <<"image/gif">>;
                ".html" -> <<"text/html">>;
                ".txt" -> <<"text/plain">>;
                ".json" -> <<"application/json">>;
                ".xml" -> <<"application/xml">>;
                ".pdf" -> <<"application/pdf">>;
                _ -> <<"application/octet-stream">>
            end;
        ContentType ->
            to_binary(ContentType)
    end.

%% @private Add custom headers from options
-spec add_custom_headers(Headers :: list(), Options :: list()) -> list().
add_custom_headers(Headers, Options) ->
    % Add metadata headers
    MetaHeaders = case proplists:get_value(meta, Options) of
        undefined ->
            [];
        Meta when is_list(Meta) ->
            lists:map(
                fun({Key, Value}) ->
                    {<<"x-oss-meta-", (to_binary(Key))/binary>>, to_binary(Value)}
                end,
                Meta
            )
    end,
    
    % Add other custom headers
    CustomHeaders = lists:filtermap(
        fun
            ({acl, Value}) ->
                {true, {<<"x-oss-acl">>, to_binary(Value)}};
            ({storage_class, Value}) ->
                {true, {<<"x-oss-storage-class">>, to_binary(Value)}};
            (_) ->
                false
        end,
        Options
    ),
    
    Headers ++ MetaHeaders ++ CustomHeaders.

%% @private Make URL for object operations
-spec make_object_url(Bucket :: bucket(), Key :: binary()) -> binary().
make_object_url(Bucket, Key) ->
    Protocol = case binary:match(Bucket#bucket.endpoint, <<"https">>) of
        {0, 5} -> <<"https">>;
        _ -> <<"http">>
    end,
    <<Protocol/binary, "://", (Bucket#bucket.host)/binary, "/", Key/binary>>.

%% @private Make URL for bucket operations
-spec make_bucket_url(Bucket :: bucket()) -> binary().
make_bucket_url(Bucket) ->
    Protocol = case binary:match(Bucket#bucket.endpoint, <<"https">>) of
        {0, 5} -> <<"https">>;
        _ -> <<"http">>
    end,
    <<Protocol/binary, "://", (Bucket#bucket.host)/binary, "/">>.

%% @private Build query parameters for list_objects
-spec build_list_objects_query(Options :: list()) -> string().
build_list_objects_query(Options) ->
    QueryParams = lists:filtermap(
        fun
            ({prefix, Value}) ->
                {true, {"prefix", binary_to_list(to_binary(Value))}};
            ({marker, Value}) ->
                {true, {"marker", binary_to_list(to_binary(Value))}};
            ({max_keys, Value}) when is_integer(Value) ->
                {true, {"max-keys", integer_to_list(Value)}};
            ({delimiter, Value}) ->
                {true, {"delimiter", binary_to_list(to_binary(Value))}};
            (_) ->
                false
        end,
        Options
    ),
    
    case QueryParams of
        [] -> "";
        _ -> "?" ++ hackney_url:qs(QueryParams)
    end.

%% @private Parse list objects XML response
-spec parse_list_objects_response(Body :: binary()) -> list().
parse_list_objects_response(_Body) ->
    % In a real implementation, this would parse the XML response
    % For simplicity, we're just returning an empty list
    []. 