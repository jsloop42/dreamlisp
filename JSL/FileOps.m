//
//  FileOps.m
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "FileOps.h"

NSString * const READ_ERROR = @"Error reading file.";

@implementation FileOps {
    NSString *_path;
    NSURL *_filePath;
    NSData *_fileData;
    NSFileHandle *_fileHandle;
    NSFileHandle *_appendHandle;
    NSData *_delim;
    NSUInteger _start;
    NSUInteger _offset;
    NSUInteger _buff;
    NSFileManager *_fm;
    dispatch_queue_t serialQueue;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _filePath = [NSURL fileURLWithPath:@""];
    _fileData = [NSData new];
    _fileHandle = [NSFileHandle new];
    _delim = [@"\n" dataUsingEncoding:NSUTF8StringEncoding];
    _start = 0;
    _offset = 0;
    _buff = 0;
    _fm = [NSFileManager defaultManager];
    serialQueue = dispatch_queue_create("jsl-fileops-queue", DISPATCH_QUEUE_SERIAL);
}

- (void)createFileIfNotExist:(NSString *)path {
    _path = path;
    if (![_fm fileExistsAtPath:_path]) {
        [_fm createFileAtPath:_path contents:nil attributes:nil];
    }
}

- (void)openFile:(NSString *)path {
    _path = path;
    _filePath = [NSURL fileURLWithPath:_path];
    _fileData = [NSData dataWithContentsOfURL:_filePath];
    if (!_fileData) {
        @throw [[NSException alloc] initWithName:@"READ_ERROR" reason:READ_ERROR userInfo:nil];
    }
    NSError *err = nil;
    _fileHandle = [NSFileHandle fileHandleForReadingFromURL:_filePath error:&err];
    if (!_fileHandle && err) {
        @throw [[NSException alloc] initWithName:@"READ_ERROR" reason:READ_ERROR userInfo:nil];
    }
    _buff = [_fileData length];
}

- (void)closeFile {
    [_fileHandle closeFile];
    if (_appendHandle) {
        [_appendHandle closeFile];
    }
}

- (BOOL)hashNext {
    return _start < _buff;
}

- (NSString *)readLine {
    NSData *line = nil;
    NSRange range = [_fileData rangeOfData:_delim options:0 range:NSMakeRange(_start, _buff - _start)];
    if (range.location != NSNotFound) {
        _offset = range.location - _start;
        line = [_fileHandle readDataOfLength:_offset];
        _start += _offset + range.length;
        [_fileHandle seekToFileOffset:_start];
    } else {
        _offset = _buff - _start;
        line = [_fileHandle readDataOfLength:_offset];
        [_fileHandle seekToFileOffset:_buff];
        _start = _buff;
    }
    return [[NSString alloc] initWithData:line encoding:NSUTF8StringEncoding];
}

- (void)append:(NSString *)string completion:(void  (^ _Nullable)(void))callback {
    dispatch_async(self->serialQueue, ^{
        if (!self->_appendHandle && self->_path) {
            self->_appendHandle = [NSFileHandle fileHandleForUpdatingAtPath:self->_path];
        }
        if (self->_appendHandle) {
            [self->_appendHandle seekToEndOfFile];
            NSData *data = [string dataUsingEncoding:NSUTF8StringEncoding];
            if (data) {
                [self->_appendHandle writeData:data];
            }
        } else {
            @throw [[NSException alloc] initWithName:@"READ_ERROR" reason:READ_ERROR userInfo:nil];
        }
        if (callback) {
            callback();
        }
    });
}

- (BOOL)delete:(NSString *)path {
    NSError *err = nil;
    BOOL ret = [_fm removeItemAtPath:path error:&err];
    if (!ret && err) {
        NSLog(@"Error: %@", err.description);
    }
    return ret;
}

@end
