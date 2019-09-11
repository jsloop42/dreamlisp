//
//  FileOps.h
//  DreamLisp
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Utils.h"
#import "Logger.h"
#import "FileIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

typedef NS_ENUM(NSUInteger, DLFileMode) {
    DLFileModeRead,
    DLFileModeWrite,
    DLFileModeAppend
};

@interface FileResult : NSObject
@property (nonatomic, readonly) NSUInteger index;
@property (nonatomic, readonly, assign) NSString *content;
@end

@interface FileOps : NSObject
- (instancetype)init;
- (BOOL)isDirectoryExists:(NSString *)path;
- (BOOL)isFileExists:(NSString *)path;
- (void)createDirectoryWithIntermediate:(NSString *)path;
- (void)createFileIfNotExist:(NSString *)path;
- (void)openFileForReading:(NSString *)path;
- (void)openFileForAppending:(NSString *)path;
- (void)openFileForWriting:(NSString *)path;
- (void)closeFile;
- (NSMutableArray<FileResult *> *)loadFileFromPath:(NSMutableArray *)locations isConcurrent:(BOOL)isConcurrent isLookup:(BOOL)isLookup;
- (NSString *)currentDirectoryPath;
- (NSBundle *)mainBundle;
- (NSString *)resourcePath;
- (BOOL)hasNext;
- (NSString *)readLine;
- (void)append:(NSString *)string;
- (void)write:(NSString *)string;
- (BOOL)delete:(NSString *)path;
@end

@interface FileOps () <FileIOServiceDelegate>
@end

NS_ASSUME_NONNULL_END
