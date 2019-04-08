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
    NSData *line = [NSData new];
    NSRange range = [_fileData rangeOfData:_delim options:0 range:NSMakeRange(_start, _buff)];
    if (range.location != NSNotFound) {
        _offset = range.location;
        line = [_fileHandle readDataOfLength:_offset - _start];
        [_fileHandle seekToFileOffset:_offset];
    } else {
        _offset += _offset;
        line = [_fileHandle readDataOfLength:_offset - _start];
        [_fileHandle seekToFileOffset:_offset];
        _start = _buff;
    }
    return [[NSString alloc] initWithData:line encoding:NSUTF8StringEncoding];
}

- (void)append:(NSString *)string {
    if (!_appendHandle && _path) {
        _appendHandle = [NSFileHandle fileHandleForUpdatingAtPath:_path];
    }
    if (_appendHandle) {
        [_appendHandle seekToEndOfFile];
        NSData *data = [[string stringByAppendingString:@"\n"] dataUsingEncoding:NSUTF8StringEncoding];
        if (data) {
            [_appendHandle writeData:data];
        }
    } else {
        @throw [[NSException alloc] initWithName:@"READ_ERROR" reason:READ_ERROR userInfo:nil];
    }
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
