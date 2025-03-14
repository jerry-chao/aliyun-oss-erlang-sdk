# Aliyun OSS SDK for Erlang

This is an Erlang SDK for Alibaba Cloud Object Storage Service (OSS). It provides a simple and easy-to-use interface for interacting with Aliyun OSS.

## Features

- Authentication with AccessKey ID and Secret
- Basic bucket operations
- Object operations (put, get, delete, list)
- Multipart upload support
- Custom metadata and headers

## Requirements

- Erlang/OTP 21 or later
- rebar3

## Dependencies

- hackney: HTTP client
- jsx: JSON parser
- base64url: Base64 URL encoding
- erlsha2: SHA-2 hash functions
- uuid: UUID generation

## Installation

Add the SDK to your `rebar.config`:

```erlang
{deps, [
    {aliyun_oss, {git, "https://github.com/user/aliyun-oss-erlang-sdk.git", {tag, "0.1.0"}}}
]}.
```

## Usage

### Basic Operations

```erlang
% Create auth and bucket objects
Auth = aliyun_oss:auth(AccessKeyId, AccessKeySecret),
Bucket = aliyun_oss:bucket(Auth, Endpoint, BucketName),

% Put an object
{ok, _} = aliyun_oss:put_object(Bucket, "example.txt", "Hello, OSS!"),

% Get an object
{ok, #{body := Body}} = aliyun_oss:get_object(Bucket, "example.txt"),

% Delete an object
{ok, _} = aliyun_oss:delete_object(Bucket, "example.txt"),

% List objects
{ok, #{objects := Objects}} = aliyun_oss:list_objects(Bucket).
```

### Multipart Upload

```erlang
% Initialize multipart upload
{ok, #{upload_id := UploadId}} = aliyun_oss:init_multipart_upload(Bucket, "large_file.txt"),

% Upload parts
{ok, #{etag := ETag1}} = aliyun_oss:upload_part(Bucket, "large_file.txt", UploadId, 1, Part1),
{ok, #{etag := ETag2}} = aliyun_oss:upload_part(Bucket, "large_file.txt", UploadId, 2, Part2),

% Complete multipart upload
Parts = [
    #{part_number => 1, etag => ETag1},
    #{part_number => 2, etag => ETag2}
],
UploadInfo = #{upload_id => UploadId, parts => Parts},
{ok, _} = aliyun_oss:complete_multipart_upload(Bucket, "large_file.txt", UploadInfo).
```

## Examples

Check the `examples` directory for more detailed examples:

- `basic_operations.erl`: Basic object operations
- `multipart_upload.erl`: Multipart upload example

## Building and Testing

```bash
$ rebar3 compile
$ rebar3 eunit
$ rebar3 ct
```

## License

MIT

## Acknowledgements

This SDK is inspired by the [Aliyun OSS SDK for Python](https://github.com/aliyun/aliyun-oss-python-sdk). 