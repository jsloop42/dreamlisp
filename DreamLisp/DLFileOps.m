//
//  DLFileOps.m
//  DreamLisp
//
//  Created by Jaseem V V on 08/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLFileOps.h"

NSString * const READ_ERROR = @"READ_ERROR";
NSString * const READ_ERROR_MSG = @"Error reading file.";

@implementation DLFileResult {
    NSUInteger _index;
    NSString *_content;
}

@synthesize index = _index;
@synthesize content = _content;

- (void)dealloc {
    [super dealloc];
}

-(void)setIndex:(NSUInteger)index {
    _index = index;
}

-(void)setContent:(NSString * _Nonnull)content {
    _content = content;
}

@end

/*!
 A class to work with file IO. Use an instance for working with a file in a specific mode, for example, only for reading, and another only for appending.
 */
@implementation DLFileOps {
    NSString *_path;
    NSURL *_filePath;
    /*! Holds the file content which is used for files which are opened for reading line by line or in appending mode. This is an autoreleased object. */
    NSData *_fileData;
    NSFileHandle *_fileHandle;
    NSData *_delim;
    NSUInteger _start;
    NSUInteger _offset;
    NSUInteger _buff;
    NSFileManager *_fm;
    NSFileHandle *_readFileHandle;
    NSFileHandle *_writeFileHandle;
    NSFileHandle *_appendFileHandle;
    dispatch_queue_t _serialQueue;
    enum DLFileMode _fileMode;
    BOOL _isFileClosed;
}

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)dealloc {
    if (!_isFileClosed) [self closeFile];
    [super dealloc];
}

- (void)bootstrap {
    _filePath = [NSURL fileURLWithPath:@""];
    _delim = [@"\n" dataUsingEncoding:NSUTF8StringEncoding];
    _start = 0;
    _offset = 0;
    _buff = 0;
    _fm = [NSFileManager defaultManager];
    _serialQueue = dispatch_queue_create("dl-fileops-queue", DISPATCH_QUEUE_SERIAL);
}

- (BOOL)isFileExists:(NSString *)path {
    return [_fm fileExistsAtPath:path];
}

- (void)createFileIfNotExist:(NSString *)path {
    _path = path;
    if (![self isFileExists:_path]) [_fm createFileAtPath:_path contents:nil attributes:nil];
}

- (BOOL)isDirectoryExists:(NSString *)path {
    BOOL isDir = YES;
    return [_fm fileExistsAtPath:path isDirectory:&isDir];
}

- (void)createDirectoryWithIntermediate:(NSString *)path {
    NSError *err = nil;
    BOOL ret = NO;
    ret = [_fm createDirectoryAtPath:path withIntermediateDirectories:YES attributes:nil error:&err];
    if (!ret || err) [[[[DLError alloc] initWithUserInfo:[err userInfo]] autorelease] throw];
}

/*!
 Opens the given file for reading.
 @throws NSException If the file is not found or if read fails, an exception is thrown.
 */
- (void)openFileForReading:(NSString *)path {
    _fileMode = DLFileModeRead;
    _path = path;
    _filePath = [NSURL fileURLWithPath:_path];
    _fileData = [NSData dataWithContentsOfURL:_filePath];
    if (!_fileData) @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    NSError *err = nil;
    _readFileHandle = [NSFileHandle fileHandleForReadingFromURL:_filePath error:&err];
    if (!_readFileHandle && err) {
        @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    }
    _isFileClosed = NO;
    _buff = [_fileData length];
}

- (void)openFileForAppending:(NSString *)path {
    _fileMode = DLFileModeAppend;
    _path = path;
    _filePath = [NSURL fileURLWithPath:_path];
    _fileData = [NSData dataWithContentsOfURL:_filePath];
    if (!_fileData) @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    NSError *err = nil;
    _appendFileHandle = [NSFileHandle fileHandleForUpdatingURL:_filePath error:&err];
    if (!_appendFileHandle && err) {
        @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    }
    _isFileClosed = NO;
    _buff = [_fileData length];
}

- (void)openFileForWriting:(NSString *)path {
    _fileMode = DLFileModeWrite;
    NSURL *fileURL = [NSURL fileURLWithPath:path];
    NSError *err = nil;
    _writeFileHandle = [NSFileHandle fileHandleForWritingToURL:fileURL error:&err];
    if (!_writeFileHandle && err) {
        @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    }
    _isFileClosed = NO;
}

- (void)closeFile {
    if (_readFileHandle && _isFileClosed) {
        [_readFileHandle closeFile];
        _isFileClosed = YES;
    }
    if (_appendFileHandle && _isFileClosed) {
        [_appendFileHandle closeFile];
        _isFileClosed = YES;
    }
    if (_writeFileHandle && _isFileClosed) {
        [_writeFileHandle closeFile];
        _isFileClosed = YES;
    }
}

/**
  Load file from the given paths.
  @param locations An array containing path string
  @param isConcurrent Should the loading be done concurrently. Concurrent option does not present loading order.
  @param isLookup Is the file loading lookup based, in which case, the process will stop after the first successful load.
  @return contents An array containing @c FileResult objects.
 */
- (NSMutableArray<DLFileResult *> *)loadFileFromPath:(NSMutableArray *)locations isConcurrent:(BOOL)isConcurrent isLookup:(BOOL)isLookup {
    NSMutableArray<DLFileResult *> *contents = [[NSMutableArray new] autorelease];
    NSUInteger len = [locations count];
    NSUInteger i = 0;
    NSString *path = nil;
    for (i = 0; i < len; i++) {
        path = locations[i];
        if ([_fm fileExistsAtPath:path]) {
            DLFileResult *result = [[DLFileResult new] autorelease];
            [result setIndex:i];
            [result setContent:[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil]];
            if (isLookup && [contents count] == 0) {
                [contents addObject:result];
                break;
            } else if (!isLookup) {
                [contents addObject:result];
            }
        }
    }
    return contents;
}

/** Returns the current working directory. */
- (NSString *)currentDirectoryPath {
    return [_fm currentDirectoryPath];
}

/** Return the path from where the executable is placed. */
- (NSBundle *)mainBundle {
    return [NSBundle bundleForClass:[self class]];
}

/** Returns the main bundle resource path. */
- (NSString *)resourcePath {
    return [[self mainBundle] resourcePath];
}

/** Checks if the there are contents left for reading. */
- (BOOL)hasNext {
    return _start < _buff;
}

/** Reads a line of string. */
- (NSString *)readLine {
    NSData *line = nil;
    NSFileHandle *fileHandle = _fileMode == DLFileModeRead ? _readFileHandle : _appendFileHandle;
    NSRange range = [_fileData rangeOfData:_delim options:0 range:NSMakeRange(_start, _buff - _start)];
    if (range.location != NSNotFound) {
        _offset = range.location - _start;
        line = [fileHandle readDataOfLength:_offset];
        _start += _offset + range.length;
        [fileHandle seekToFileOffset:_start];
    } else {
        _offset = _buff - _start;
        line = [fileHandle readDataOfLength:_offset];
        [fileHandle seekToFileOffset:_buff];
        _start = _buff;
    }
    return [[[NSString alloc] initWithData:line encoding:NSUTF8StringEncoding] autorelease];
}

- (void)append:(NSString *)string {
    if (_appendFileHandle) {
        [_appendFileHandle seekToEndOfFile];
        NSData *data = [[NSData alloc] initWithBytes:[string cStringUsingEncoding:NSUTF8StringEncoding] length:[string count]];
        if (data) {
            [_appendFileHandle writeData:data];
            [data release];
            data = nil;
        }
    } else {
        @throw [[[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil] autorelease];
    }
}

/** Writes the given string to the file. */
- (void)write:(NSString *)string {
    [_writeFileHandle writeData:[NSData dataWithBytes:[string UTF8String] length:[string length]]];
}

/** Deletes the file at the given path. */
- (BOOL)delete:(NSString *)path {
    NSError *err = nil;
    BOOL ret = [_fm removeItemAtPath:path error:&err];
    if (!ret && err) [DLLog error:err.description];
    return ret;
}

#pragma mark - FileIOServiceDelegate

- (NSString *)readFile:(NSString *)path {
    DLFileResult *res = [[self loadFileFromPath:[[@[path] mutableCopy] autorelease] isConcurrent:NO isLookup:NO] first];
    return [res content];
}

@end
