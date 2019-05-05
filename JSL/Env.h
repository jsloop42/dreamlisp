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
#import <objc/NSObjCRuntime.h>

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;
@class JSList;
@class SymbolTable;

@interface Env : NSObject
@property (nonatomic, readwrite) Env *outer;
@property (nonatomic, readwrite) NSMapTable<JSSymbol *, id<JSDataProtocol>> *table;
@property (nonatomic, readwrite) SymbolTable *symbolTable;
+ (instancetype)new NS_UNAVAILABLE;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithTable:(SymbolTable *)table;
- (instancetype)initWithEnv:(Env *)env;
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (void)setObject:(id<JSDataProtocol>)value forSymbol:(JSSymbol *)key;
- (Env *)findEnvForKey:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key;
@end

NS_ASSUME_NONNULL_END
