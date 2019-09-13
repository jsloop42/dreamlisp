//
//  DreamLisp.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLConst.h"
#import "DLTypes.h"
#import "DLDelegate.h"
#import "DLReader.h"
#import "DLPrinter.h"
#import "DLCore.h"
#import "DLFileOps.h"
#import "DLIOService.h"
#import <objc/objc-api.h>

NS_ASSUME_NONNULL_BEGIN

@class DLReader;

@interface DreamLisp: NSObject <DLDelegate>
@property (nonatomic, readwrite, retain) DLReader *reader;
@property (nonatomic, readwrite, retain) DLEnv *globalEnv;
@property (nonatomic, readwrite, retain) DLEnv *env;
@property (nonatomic, readwrite) BOOL isREPL;
@property (nonatomic, readwrite, retain) NSString *prompt;
- (instancetype)init;
- (instancetype)initWithoutREPL;
- (void)bootstrap;
- (void)loadDLModuleLibs;
- (void)printVersion;
- (NSString * _Nullable)rep:(NSString *)string;
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
