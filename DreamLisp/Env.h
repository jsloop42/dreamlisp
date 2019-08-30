//
//  Env.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "DLError.h"
#import "Logger.h"
#import "ModuleTable.h"
#import "SymbolTable.h"
#import <objc/NSObjCRuntime.h>

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLList;
@class DLVector;
@class DLHashMap;
@class DLAtom;
@class SymbolTable;
@class ModuleTable;

@interface Env : NSObject
@property (nonatomic, readwrite) Env *outer;
@property (nonatomic, readwrite) ModuleTable *exportTable;
@property (nonatomic, readwrite) ModuleTable *importTable;
@property (nonatomic, readwrite) ModuleTable *internalTable;
@property (nonatomic, readwrite) SymbolTable *symbolTable;
@property (nonatomic, readwrite) NSString *moduleName;
@property (nonatomic, readwrite) NSString *moduleDescription;
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
- (id<DLDataProtocol>)resolveFault:(id<DLDataProtocol>)object forKey:(DLSymbol *)key inEnv:(Env *)env;
- (void)setObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key;
- (void)updateImportedObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key;
- (void)updateExportedObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key;
- (id<DLDataProtocol> _Nullable)objectForKey:(DLSymbol *)key;
- (id<DLDataProtocol> _Nullable)objectForKey:(DLSymbol *)key isThrow:(BOOL)isThrow;
- (void)setModuleName:(NSString *)name;
- (NSString *)moduleName;
- (NSArray *)exportedFunctions;
- (NSArray *)importedFunctions;
- (NSArray *)internalFunctions;
@end

NS_ASSUME_NONNULL_END
