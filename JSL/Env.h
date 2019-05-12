//
//  Env.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSSymbol.h"
#import "JSList.h"
#import "JSError.h"
#import "SymbolTable.h"
#import "ModuleTable.h"
#import <objc/NSObjCRuntime.h>

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;
@class JSList;
@class SymbolTable;
@class ModuleTable;

@interface Env : NSObject
@property (nonatomic, readwrite) Env *outer;
@property (nonatomic, readwrite) NSMapTable<JSSymbol *, id<JSDataProtocol>> *table;
@property (nonatomic, readwrite) ModuleTable *coreModule;
@property (nonatomic, readwrite) ModuleTable *module;
@property (nonatomic, readwrite) BOOL isModule;
- (instancetype)init;
- (instancetype)initWithCoreModule:(ModuleTable *)core;
- (instancetype)initWithEnv:(Env *)env;
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (void)setObject:(id<JSDataProtocol>)obj forSymbol:(JSSymbol *)key;
- (Env *)findEnvForKey:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key;
- (_Nullable id<JSDataProtocol>)objectForSymbolFromCore:(JSSymbol *)key;
@end

NS_ASSUME_NONNULL_END
