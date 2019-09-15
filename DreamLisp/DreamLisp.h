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
#import "DLLogger.h"
#import <objc/objc-api.h>

NS_ASSUME_NONNULL_BEGIN

@class DLReader;

@interface DreamLisp: NSObject <DLDelegate>
@property (nonatomic, readwrite, retain) DLReader *reader;
@property (nonatomic, readwrite, retain) DLEnv *globalEnv;
@property (nonatomic, readwrite, retain) DLEnv *env;
@property (nonatomic, readwrite) BOOL isREPL;
@property (nonatomic, readwrite, retain) NSString *prompt;
@property (nonatomic, readwrite) BOOL isDebug;
@property (nonatomic, readwrite) BOOL isVerbose;
- (instancetype)init;
- (instancetype)initWithoutREPL;
- (void)bootstrap;
- (void)setIsDebug:(BOOL)isDebug;
- (void)setIsVerbose:(BOOL)isVerbose;
- (void)loadDLModuleLibs;
- (void)printVersion;
- (NSString * _Nullable)rep:(NSString *)string;
- (id<DLDataProtocol>)evalAST:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
- (id<DLDataProtocol>)quasiquote:(id<DLDataProtocol>)ast;
- (BOOL)isMacroCall:(id<DLDataProtocol>)ast env:(DLEnv *)env;
- (id<DLDataProtocol>)macroExpand:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
- (DLList *)toDoForm:(NSMutableArray *)arr;
- (DLMethod *)parseMethodForm:(id<DLDataProtocol>)ast object:(id<DLProxyProtocol>)object withEnv:(DLEnv *)env;
- (id<DLDataProtocol>)eval:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
