%%%-------------------------------------------------------------------
%%% @doc Multipart upload example for Aliyun OSS Erlang SDK
%%% @end
%%%-------------------------------------------------------------------
-module(multipart_upload).

-export([
    run/0
]).

%% @doc Run the example
run() ->
    % Set your credentials and endpoint
    AccessKeyId = os:getenv("OSS_ACCESS_KEY_ID"),
    AccessKeySecret = os:getenv("OSS_ACCESS_KEY_SECRET"),
    Endpoint = os:getenv("OSS_ENDPOINT"),
    BucketName = os:getenv("OSS_BUCKET_NAME"),
    
    % Create auth and bucket objects
    Auth = aliyun_oss:auth(AccessKeyId, AccessKeySecret),
    Bucket = aliyun_oss:bucket(Auth, Endpoint, BucketName),
    
    % Define a key for our object
    Key = "large_file.txt",
    
    % Create some sample content (in a real scenario, this would be a large file)
    Part1 = binary:copy(<<"Part 1 of the multipart upload. ">>, 100),
    Part2 = binary:copy(<<"Part 2 of the multipart upload. ">>, 100),
    Part3 = binary:copy(<<"Part 3 of the multipart upload. ">>, 100),
    
    % Initialize multipart upload
    io:format("Initializing multipart upload for ~s...~n", [Key]),
    case aliyun_oss:init_multipart_upload(Bucket, Key) of
        {ok, #{upload_id := UploadId} = Result1} ->
            io:format("Multipart upload initialized: ~p~n", [Result1]),
            
            % Upload parts
            io:format("Uploading part 1...~n"),
            case aliyun_oss:upload_part(Bucket, Key, UploadId, 1, Part1) of
                {ok, #{etag := ETag1} = Result2} ->
                    io:format("Part 1 uploaded: ~p~n", [Result2]),
                    
                    io:format("Uploading part 2...~n"),
                    case aliyun_oss:upload_part(Bucket, Key, UploadId, 2, Part2) of
                        {ok, #{etag := ETag2} = Result3} ->
                            io:format("Part 2 uploaded: ~p~n", [Result3]),
                            
                            io:format("Uploading part 3...~n"),
                            case aliyun_oss:upload_part(Bucket, Key, UploadId, 3, Part3) of
                                {ok, #{etag := ETag3} = Result4} ->
                                    io:format("Part 3 uploaded: ~p~n", [Result4]),
                                    
                                    % Complete multipart upload
                                    Parts = [
                                        #{part_number => 1, etag => ETag1},
                                        #{part_number => 2, etag => ETag2},
                                        #{part_number => 3, etag => ETag3}
                                    ],
                                    UploadInfo = #{upload_id => UploadId, parts => Parts},
                                    
                                    io:format("Completing multipart upload...~n"),
                                    case aliyun_oss:complete_multipart_upload(Bucket, Key, UploadInfo) of
                                        {ok, Result5} ->
                                            io:format("Multipart upload completed: ~p~n", [Result5]),
                                            
                                            % Get the object to verify
                                            io:format("Getting object ~s...~n", [Key]),
                                            case aliyun_oss:get_object(Bucket, Key) of
                                                {ok, #{body := Body} = Result6} ->
                                                    io:format("Get successful: ~p~n", [Result6]),
                                                    io:format("Content length: ~p bytes~n", [byte_size(Body)]),
                                                    
                                                    % Delete the object
                                                    io:format("Deleting object ~s...~n", [Key]),
                                                    case aliyun_oss:delete_object(Bucket, Key) of
                                                        {ok, Result7} ->
                                                            io:format("Delete successful: ~p~n", [Result7]);
                                                        {error, Error7} ->
                                                            io:format("Delete failed: ~p~n", [Error7])
                                                    end;
                                                {error, Error6} ->
                                                    io:format("Get failed: ~p~n", [Error6])
                                            end;
                                        {error, Error5} ->
                                            io:format("Complete multipart upload failed: ~p~n", [Error5])
                                    end;
                                {error, Error4} ->
                                    io:format("Part 3 upload failed: ~p~n", [Error4]),
                                    abort_upload(Bucket, Key, UploadId)
                            end;
                        {error, Error3} ->
                            io:format("Part 2 upload failed: ~p~n", [Error3]),
                            abort_upload(Bucket, Key, UploadId)
                    end;
                {error, Error2} ->
                    io:format("Part 1 upload failed: ~p~n", [Error2]),
                    abort_upload(Bucket, Key, UploadId)
            end;
        {error, Error1} ->
            io:format("Initialize multipart upload failed: ~p~n", [Error1])
    end,
    
    ok.

%% @private Abort a multipart upload
abort_upload(Bucket, Key, UploadId) ->
    io:format("Aborting multipart upload...~n"),
    case aliyun_oss:abort_multipart_upload(Bucket, Key, UploadId) of
        {ok, Result} ->
            io:format("Multipart upload aborted: ~p~n", [Result]);
        {error, Error} ->
            io:format("Abort multipart upload failed: ~p~n", [Error])
    end. 