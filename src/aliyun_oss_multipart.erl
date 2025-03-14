%%%-------------------------------------------------------------------
%%% @doc Aliyun OSS multipart upload operations
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss_multipart).

-export([
    init_multipart_upload/2,
    init_multipart_upload/3,
    upload_part/5,
    complete_multipart_upload/3,
    abort_multipart_upload/3,
    list_multipart_uploads/1,
    list_multipart_uploads/2
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize a multipart upload
-spec init_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
init_multipart_upload(Bucket, Key) ->
    init_multipart_upload(Bucket, Key, []).

%% @doc Initialize a multipart upload with options
-spec init_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
init_multipart_upload(Bucket, Key, Options) ->
    KeyBin = to_binary(Key),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, get_bucket_host(Bucket)},
        {<<"Content-Type">>, get_content_type(KeyBin, Options)}
    ],
    
    % Add custom headers from options
    Headers1 = add_custom_headers(Headers0, Options),
    
    % Sign the request
    SignedHeaders = sign_request(Bucket, post, Headers1),
    
    % Make the request
    Url = make_object_url(Bucket, KeyBin) ++ "?uploads",
    case hackney:request(post, Url, SignedHeaders, <<>>, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, Body} = hackney:body(ClientRef),
            % Parse XML response to get upload ID
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body,
                upload_id => extract_upload_id(Body)
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

%% @doc Upload a part in a multipart upload
-spec upload_part(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), 
                 UploadId :: binary() | string(), PartNumber :: integer(), Content :: binary()) -> 
    {ok, map()} | {error, term()}.
upload_part(Bucket, Key, UploadId, PartNumber, Content) when is_integer(PartNumber), PartNumber >= 1, PartNumber =< 10000 ->
    KeyBin = to_binary(Key),
    UploadIdBin = to_binary(UploadId),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, get_bucket_host(Bucket)},
        {<<"Content-Length">>, integer_to_binary(byte_size(Content))}
    ],
    
    % Sign the request
    SignedHeaders = sign_request(Bucket, put, Headers0),
    
    % Make the request
    QueryParams = "?partNumber=" ++ integer_to_list(PartNumber) ++ "&uploadId=" ++ binary_to_list(UploadIdBin),
    Url = make_object_url(Bucket, KeyBin) ++ QueryParams,
    
    case hackney:request(put, Url, SignedHeaders, Content, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, _Body} = hackney:body(ClientRef),
            % Extract ETag from response headers
            ETag = proplists:get_value(<<"ETag">>, ResponseHeaders, <<"">>),
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                part_number => PartNumber,
                etag => ETag
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

%% @doc Complete a multipart upload
-spec complete_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(),
                              UploadInfo :: map()) -> 
    {ok, map()} | {error, term()}.
complete_multipart_upload(Bucket, Key, #{upload_id := UploadId, parts := Parts}) ->
    KeyBin = to_binary(Key),
    UploadIdBin = to_binary(UploadId),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, get_bucket_host(Bucket)},
        {<<"Content-Type">>, <<"application/xml">>}
    ],
    
    % Create XML body with part information
    Body = create_complete_multipart_body(Parts),
    Headers1 = [{<<"Content-Length">>, integer_to_binary(byte_size(Body))} | Headers0],
    
    % Sign the request
    SignedHeaders = sign_request(Bucket, post, Headers1),
    
    % Make the request
    QueryParams = "?uploadId=" ++ binary_to_list(UploadIdBin),
    Url = make_object_url(Bucket, KeyBin) ++ QueryParams,
    
    case hackney:request(post, Url, SignedHeaders, Body, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => ResponseBody
            }};
        {ok, StatusCode, ResponseHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {error, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => ResponseBody
            }};
        Error ->
            Error
    end.

%% @doc Abort a multipart upload
-spec abort_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(),
                           UploadId :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
abort_multipart_upload(Bucket, Key, UploadId) ->
    KeyBin = to_binary(Key),
    UploadIdBin = to_binary(UploadId),
    
    % Prepare headers
    Headers0 = [
        {<<"Host">>, get_bucket_host(Bucket)}
    ],
    
    % Sign the request
    SignedHeaders = sign_request(Bucket, delete, Headers0),
    
    % Make the request
    QueryParams = "?uploadId=" ++ binary_to_list(UploadIdBin),
    Url = make_object_url(Bucket, KeyBin) ++ QueryParams,
    
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

%% @doc List multipart uploads in a bucket
-spec list_multipart_uploads(Bucket :: aliyun_oss_bucket:bucket()) -> 
    {ok, map()} | {error, term()}.
list_multipart_uploads(Bucket) ->
    list_multipart_uploads(Bucket, []).

%% @doc List multipart uploads in a bucket with options
-spec list_multipart_uploads(Bucket :: aliyun_oss_bucket:bucket(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
list_multipart_uploads(Bucket, Options) ->
    % Prepare headers
    Headers0 = [
        {<<"Host">>, get_bucket_host(Bucket)}
    ],
    
    % Sign the request
    SignedHeaders = sign_request(Bucket, get, Headers0),
    
    % Prepare query parameters
    QueryParams = build_list_uploads_query(Options),
    
    % Make the request
    Url = make_bucket_url(Bucket) ++ "?uploads" ++ QueryParams,
    
    case hackney:request(get, Url, SignedHeaders, <<>>, []) of
        {ok, StatusCode, ResponseHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, Body} = hackney:body(ClientRef),
            % Parse XML response
            {ok, #{
                status_code => StatusCode,
                headers => ResponseHeaders,
                body => Body,
                uploads => parse_list_uploads_response(Body)
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

%% @private Get bucket host
-spec get_bucket_host(Bucket :: aliyun_oss_bucket:bucket()) -> binary().
get_bucket_host(Bucket) ->
    % This is a simplified implementation
    % In a real implementation, we would access the host from the bucket record
    <<"bucket.oss-cn-hangzhou.aliyuncs.com">>.

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

%% @private Sign the request
-spec sign_request(Bucket :: aliyun_oss_bucket:bucket(), Method :: atom(), Headers :: list()) -> list().
sign_request(Bucket, Method, Headers) ->
    % This is a simplified implementation
    % In a real implementation, we would call aliyun_oss_auth:sign_request
    % with the auth object from the bucket
    Headers.

%% @private Make URL for object operations
-spec make_object_url(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary()) -> binary().
make_object_url(Bucket, Key) ->
    % This is a simplified implementation
    % In a real implementation, we would construct the URL based on the bucket's endpoint and name
    <<"http://bucket.oss-cn-hangzhou.aliyuncs.com/", Key/binary>>.

%% @private Make URL for bucket operations
-spec make_bucket_url(Bucket :: aliyun_oss_bucket:bucket()) -> binary().
make_bucket_url(Bucket) ->
    % This is a simplified implementation
    % In a real implementation, we would construct the URL based on the bucket's endpoint and name
    <<"http://bucket.oss-cn-hangzhou.aliyuncs.com">>.

%% @private Extract upload ID from XML response
-spec extract_upload_id(Body :: binary()) -> binary().
extract_upload_id(_Body) ->
    % In a real implementation, this would parse the XML to extract the upload ID
    <<"upload-id-placeholder">>.

%% @private Create XML body for completing multipart upload
-spec create_complete_multipart_body(Parts :: list()) -> binary().
create_complete_multipart_body(Parts) ->
    % Sort parts by part number
    SortedParts = lists:sort(
        fun(#{part_number := A}, #{part_number := B}) -> A =< B end,
        Parts
    ),
    
    % Create XML elements for each part
    PartElements = lists:map(
        fun(#{part_number := PartNumber, etag := ETag}) ->
            <<"<Part>",
              "<PartNumber>", (integer_to_binary(PartNumber))/binary, "</PartNumber>",
              "<ETag>", ETag/binary, "</ETag>",
              "</Part>">>
        end,
        SortedParts
    ),
    
    % Combine all parts into a complete XML document
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<CompleteMultipartUpload>",
      (list_to_binary(PartElements))/binary,
      "</CompleteMultipartUpload>">>.

%% @private Build query parameters for list_multipart_uploads
-spec build_list_uploads_query(Options :: list()) -> string().
build_list_uploads_query(Options) ->
    QueryParams = lists:filtermap(
        fun
            ({prefix, Value}) ->
                {true, {"prefix", binary_to_list(to_binary(Value))}};
            ({key_marker, Value}) ->
                {true, {"key-marker", binary_to_list(to_binary(Value))}};
            ({upload_id_marker, Value}) ->
                {true, {"upload-id-marker", binary_to_list(to_binary(Value))}};
            ({max_uploads, Value}) when is_integer(Value) ->
                {true, {"max-uploads", integer_to_list(Value)}};
            ({delimiter, Value}) ->
                {true, {"delimiter", binary_to_list(to_binary(Value))}};
            (_) ->
                false
        end,
        Options
    ),
    
    case QueryParams of
        [] -> "";
        _ -> "&" ++ hackney_url:qs(QueryParams)
    end.

%% @private Parse list uploads XML response
-spec parse_list_uploads_response(Body :: binary()) -> list().
parse_list_uploads_response(_Body) ->
    % In a real implementation, this would parse the XML response
    % For simplicity, we're just returning an empty list
    []. 