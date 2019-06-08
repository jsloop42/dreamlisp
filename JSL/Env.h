//
//  Env.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "JSError.h"
#import "Logger.h"
#import "ModuleTable.h"
#import <objc/NSObjCRuntime.h>

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;
@class JSList;
@class JSVector;
@class JSHashMap;
@class JSAtom;
@class SymbolTable;
@class ModuleTable;

@interface Env : NSObject
@property (nonatomic, readwrite) Env *outer;
@property (nonatomic, readwrite) ModuleTable *exportTable;
@property (nonatomic, readwrite) ModuleTable *importTable;
@property (nonatomic, readwrite) ModuleTable *internalTable;
@property (nonatomic, readwrite) NSString *moduleName;
@property (nonatomic, readwrite) BOOL isUserDefined;
@property (nonatomic, readwrite) BOOL isExportAll;
+ (void)initialize;
+ (void)setEnv:(Env *)env forModuleName:(NSString *)moduleName;
+ (Env *)forModuleName:(NSString *)moduleName;
+ (void)removeModule:(NSString *)moduleName;
+ (NSMapTable<NSString *, Env *> *)modules;
- (instancetype)init;
- (instancetype)initWithModuleName:(NSString *)name isUserDefined:(BOOL)isUserDefined;
- (instancetype)initWithEnv:(Env *)env;
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (void)setObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key;
- (void)updateObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key isThrow:(BOOL)isThrow;
- (void)setModuleName:(NSString *)name;
- (NSString *)moduleName;

@end

NS_ASSUME_NONNULL_END
