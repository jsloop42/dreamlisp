//
//  DLFileOps.h
//  DreamLisp
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLUtils.h"
#import "DLLogger.h"
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
- (instancetype)init;
- (BOOL)isDirectoryExists:(NSString *)path;
- (BOOL)isFileExists:(NSString *)path;
- (void)createDirectoryWithIntermediate:(NSString *)path;
- (void)createFileIfNotExist:(NSString *)path;
- (void)openFileForReading:(NSString *)path;
- (void)openFileForAppending:(NSString *)path;
- (void)openFileForWriting:(NSString *)path;
- (void)closeFile;
- (NSMutableArray<DLFileResult *> *)loadFileFromPath:(NSMutableArray *)locations isConcurrent:(BOOL)isConcurrent isLookup:(BOOL)isLookup;
- (NSString *)currentDirectoryPath;
- (NSBundle *)mainBundle;
- (NSString *)resourcePath;
- (BOOL)hasNext;
- (NSString *)readLine;
- (void)append:(NSString *)string;
- (void)write:(NSString *)string;
- (BOOL)delete:(NSString *)path;
@end

@interface DLFileOps () <DLFileIOServiceDelegate>
@end

NS_ASSUME_NONNULL_END
