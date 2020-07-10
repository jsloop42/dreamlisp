//
//  DLFileOps.h
//  DreamLisp
//
//  Created by Jaseem V V on 08/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLLogger.h"
#import "DLError.h"
#import "DLFileIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

typedef NS_ENUM(NSUInteger, DLFileMode) {
    DLFileModeRead,
    DLFileModeWrite,
    DLFileModeAppend
};

@interface DLFileResult : NSObject
@property (nonatomic, readonly) NSUInteger index;
@property (nonatomic, readonly, assign) NSString *content;
@end

@interface DLFileOps : NSObject
@property (nonatomic, readwrite, retain) NSString *path;
@property (nonatomic, readwrite, retain) NSFileManager *fm;
- (instancetype)init;
- (BOOL)isDirectoryExists:(NSString *)path;
- (BOOL)isFileExists:(NSString *)path;
- (NSString *)currentDirectoryPath;
- (NSString *)projectRoot;
- (NSBundle *)mainBundle;
- (NSString *)resourcePath;
- (NSInteger)fileSize;
- (NSURL *)applicationSupportDirectory;
- (BOOL)copyFile:(NSURL *)sourceURL toURL:(NSURL *)destinationURL;
- (void)createDirectoryWithIntermediate:(NSString *)path;
- (void)createFileIfNotExist:(NSString *)path;
- (void)openFileForReading:(NSString *)path;
- (void)openFileForAppending:(NSString *)path;
- (void)openFileForWriting:(NSString *)path;
- (void)closeFile;
- (NSMutableArray<DLFileResult *> *)loadFileFromPath:(NSMutableArray *)locations isConcurrent:(BOOL)isConcurrent isLookup:(BOOL)isLookup;
- (BOOL)hasNext;
- (NSString *)readLine;
- (void)append:(NSString *)string;
- (void)writeString:(NSString *)string;
- (void)write:(NSData *)data;
- (BOOL)delete:(NSString *)path;
@end

@interface DLFileOps () <DLFileIOServiceDelegate>
@end

NS_ASSUME_NONNULL_END
