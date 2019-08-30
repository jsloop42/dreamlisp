//
//  DreamLisp.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Const.h"
#import "Types.h"
#import "DLDelegate.h"
#import "Reader.h"
#import "Printer.h"
#import "Core.h"
#import "FileOps.h"
#import "IOService.h"
#import <objc/objc-api.h>

NS_ASSUME_NONNULL_BEGIN

@interface DreamLisp: NSObject <DLDelegate>
@property (nonatomic, readwrite) Reader *reader;
@property (nonatomic, readwrite) Env *globalEnv;
@property (nonatomic, readwrite) Env *env;
@property (nonatomic, readwrite) BOOL isREPL;
@property (nonatomic, readwrite) NSString *prompt;
- (instancetype)init;
- (instancetype)initWithoutREPL;
- (void)bootstrap;
- (void)loadCoreLib;
- (void)printVersion;
- (NSString * _Nullable)rep:(NSString *)string;
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
