%%%-------------------------------------------------------------------
%%% @doc Aliyun OSS main API module
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss).

-export([
    % Auth operations
    auth/2,
    auth/3,
    
    % Bucket operations
    bucket/4,
    
    % Object operations
    put_object/3,
    put_object/4,
    get_object/2,
    delete_object/2,
    list_objects/1,
    list_objects/2,
    
    % Multipart upload operations
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

%% @doc Create a new auth object with access key ID and secret
-spec auth(AccessKeyId :: binary() | string(), AccessKeySecret :: binary() | string()) -> 
    aliyun_oss_auth:auth().
auth(AccessKeyId, AccessKeySecret) ->
    aliyun_oss_auth:new(AccessKeyId, AccessKeySecret).

%% @doc Create a new auth object with access key ID, secret and security token
-spec auth(AccessKeyId :: binary() | string(), AccessKeySecret :: binary() | string(), SecurityToken :: binary() | string()) -> 
    aliyun_oss_auth:auth().
auth(AccessKeyId, AccessKeySecret, SecurityToken) ->
    aliyun_oss_auth:new(AccessKeyId, AccessKeySecret, SecurityToken).

%% @doc Create a new bucket object
-spec bucket(Auth :: aliyun_oss_auth:auth(), Endpoint :: binary() | string(), BucketName :: binary() | string(), Region :: binary() | string()) -> 
    aliyun_oss_bucket:bucket().
bucket(Auth, Endpoint, BucketName, Region) ->
    aliyun_oss_bucket:new(Auth, Endpoint, BucketName, Region).

%% @doc Put an object into the bucket
-spec put_object(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), Content :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
put_object(Bucket, Key, Content) ->
    aliyun_oss_bucket:put_object(Bucket, Key, Content).

%% @doc Put an object into the bucket with options
-spec put_object(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), Content :: binary() | string(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
put_object(Bucket, Key, Content, Options) ->
    aliyun_oss_bucket:put_object(Bucket, Key, Content, Options).

%% @doc Get an object from the bucket
-spec get_object(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
get_object(Bucket, Key) ->
    aliyun_oss_bucket:get_object(Bucket, Key).

%% @doc Delete an object from the bucket
-spec delete_object(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
delete_object(Bucket, Key) ->
    aliyun_oss_bucket:delete_object(Bucket, Key).

%% @doc List objects in the bucket
-spec list_objects(Bucket :: aliyun_oss_bucket:bucket()) -> 
    {ok, map()} | {error, term()}.
list_objects(Bucket) ->
    aliyun_oss_bucket:list_objects(Bucket).

%% @doc List objects in the bucket with options
-spec list_objects(Bucket :: aliyun_oss_bucket:bucket(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
list_objects(Bucket, Options) ->
    aliyun_oss_bucket:list_objects(Bucket, Options).

%% @doc Initialize a multipart upload
-spec init_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
init_multipart_upload(Bucket, Key) ->
    aliyun_oss_multipart:init_multipart_upload(Bucket, Key).

%% @doc Initialize a multipart upload with options
-spec init_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
init_multipart_upload(Bucket, Key, Options) ->
    aliyun_oss_multipart:init_multipart_upload(Bucket, Key, Options).

%% @doc Upload a part in a multipart upload
-spec upload_part(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(), 
                 UploadId :: binary() | string(), PartNumber :: integer(), Content :: binary()) -> 
    {ok, map()} | {error, term()}.
upload_part(Bucket, Key, UploadId, PartNumber, Content) ->
    aliyun_oss_multipart:upload_part(Bucket, Key, UploadId, PartNumber, Content).

%% @doc Complete a multipart upload
-spec complete_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(),
                              UploadInfo :: map()) -> 
    {ok, map()} | {error, term()}.
complete_multipart_upload(Bucket, Key, UploadInfo) ->
    aliyun_oss_multipart:complete_multipart_upload(Bucket, Key, UploadInfo).

%% @doc Abort a multipart upload
-spec abort_multipart_upload(Bucket :: aliyun_oss_bucket:bucket(), Key :: binary() | string(),
                           UploadId :: binary() | string()) -> 
    {ok, map()} | {error, term()}.
abort_multipart_upload(Bucket, Key, UploadId) ->
    aliyun_oss_multipart:abort_multipart_upload(Bucket, Key, UploadId).

%% @doc List multipart uploads in a bucket
-spec list_multipart_uploads(Bucket :: aliyun_oss_bucket:bucket()) -> 
    {ok, map()} | {error, term()}.
list_multipart_uploads(Bucket) ->
    aliyun_oss_multipart:list_multipart_uploads(Bucket).

%% @doc List multipart uploads in a bucket with options
-spec list_multipart_uploads(Bucket :: aliyun_oss_bucket:bucket(), Options :: list()) -> 
    {ok, map()} | {error, term()}.
list_multipart_uploads(Bucket, Options) ->
    aliyun_oss_multipart:list_multipart_uploads(Bucket, Options). 