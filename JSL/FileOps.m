//
//  FileOps.m
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "FileOps.h"

NSString * const READ_ERROR = @"READ_ERROR";
NSString * const READ_ERROR_MSG = @"Error reading file.";

@implementation FileResult {
    NSUInteger _index;
    NSString *_content;
}

@synthesize index = _index;
@synthesize content = _content;

-(void)setIndex:(NSUInteger)index {
    _index = index;
}

-(void)setContent:(NSString * _Nonnull)content {
    _content = content;
}

@end

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
    dispatch_queue_t _serialQueue;
}

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)dealloc {
    [self closeFile];
    _appendHandle = nil;
    _fileHandle = nil;
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
    _serialQueue = dispatch_queue_create("jsl-fileops-queue", DISPATCH_QUEUE_SERIAL);
}

- (void)createFileIfNotExist:(NSString *)path {
    _path = path;
    if (![_fm fileExistsAtPath:_path]) [_fm createFileAtPath:_path contents:nil attributes:nil];
}

- (BOOL)isDirectoryExists:(NSString *)path {
    BOOL isDir = YES;
    return [_fm fileExistsAtPath:path isDirectory:&isDir];
}

- (void)createDirectoryWithIntermediate:(NSString *)path {
    NSError *err = nil;
    BOOL ret = NO;
    ret = [_fm createDirectoryAtPath:path withIntermediateDirectories:YES attributes:nil error:&err];
    if (!ret || err) [[[JSError alloc] initWithUserInfo:[err userInfo]] throw];
}

/** Opens the given file setting handlers for reading the file. */
- (void)openFile:(NSString *)path {
    _path = path;
    _filePath = [NSURL fileURLWithPath:_path];
    _fileData = [NSData dataWithContentsOfURL:_filePath];
    if (!_fileData) @throw [[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil];
    NSError *err = nil;
    _fileHandle = [NSFileHandle fileHandleForReadingFromURL:_filePath error:&err];
    if (!_fileHandle && err) @throw [[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil];
    _buff = [_fileData length];
}

- (void)closeFile {
    if (_fileHandle) [_fileHandle closeFile];
    if (_appendHandle) [_appendHandle closeFile];
}

/**
  Load file from the given paths.
  @param locations An array containing path string
  @param isConcurrent Should the loading be done concurrently. Concurrent option does not present loading order.
  @param isLookup Is the file loading lookup based, in which case, the process will stop after the first successful load.
  @return contents An array containing @c FileResult objects.
 */
- (NSMutableArray<FileResult *> *)loadFileFromPath:(NSMutableArray *)locations isConcurrent:(BOOL)isConcurrent isLookup:(BOOL)isLookup {
    NSMutableArray<FileResult *> *contents = [NSMutableArray new];
    NSLock *lock = [NSLock new];
    NSEnumerationOptions opt = isConcurrent ? NSEnumerationConcurrent : 0;
    FileOps * __weak weakSelf = self;
    [locations enumerateObjectsWithOptions:opt usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        FileOps *this = weakSelf;
        NSString *path = locations[idx];
        if ([this->_fm fileExistsAtPath:path]) {
            FileResult *result = [FileResult new];
            [result setIndex:idx];
            [result setContent:[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil]];
            if (isConcurrent) [lock lock];
            if (isLookup && [contents count] == 0) {
                [contents addObject:result];
                *stop = YES;
            } else if (!isLookup) {
                [contents addObject:result];
            }
            if (isConcurrent) [lock unlock];
        }
    }];
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

/** Appends the given string to currently opened file asynchronously and invokes the given callback function when done. */
- (void)append:(NSString *)string completion:(void  (^ _Nullable)(void))callback {
    FileOps * __weak weakSelf = self;
    dispatch_async(self->_serialQueue, ^{
        FileOps *this = weakSelf;
        if (this) {
            if (!this->_appendHandle && this->_path) {
                this->_appendHandle = [NSFileHandle fileHandleForUpdatingAtPath:this->_path];
            }
            if (this->_appendHandle) {
                [this->_appendHandle seekToEndOfFile];
                NSData *data = [string dataUsingEncoding:NSUTF8StringEncoding];
                if (data) {
                    [this->_appendHandle writeData:data];
                }
            } else {
                @throw [[NSException alloc] initWithName:READ_ERROR reason:READ_ERROR_MSG userInfo:nil];
            }
            if (callback) callback();
        }
    });
}

/** Deletes the file at the given path. */
- (BOOL)delete:(NSString *)path {
    NSError *err = nil;
    BOOL ret = [_fm removeItemAtPath:path error:&err];
    if (!ret && err) error(@"Error: %@", err.description);
    return ret;
}

@end
