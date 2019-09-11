//
//  DLEnv.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"
#import "DLError.h"
#import "DLLogger.h"
#import "DLModuleTable.h"
#import "DLSymbolTable.h"
#import <objc/NSObjCRuntime.h>

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLList;
@class DLVector;
@class DLHashMap;
@class DLAtom;
@class DLSymbolTable;
@class DLModuleTable;

@interface DLEnv : NSObject<NSSecureCoding>
@property (nonatomic, readwrite, retain) DLEnv *outer;
@property (nonatomic, readwrite, retain) DLModuleTable *exportTable;
@property (nonatomic, readwrite, retain) DLModuleTable *importTable;
@property (nonatomic, readwrite, retain) DLModuleTable *internalTable;
@property (nonatomic, readwrite, retain) DLSymbolTable *symbolTable;
@property (nonatomic, readwrite, retain) NSString *moduleName;
@property (nonatomic, readwrite, retain) NSString *moduleDescription;
@property (nonatomic, readwrite) BOOL isUserDefined;
@property (nonatomic, readwrite) BOOL isExportAll;
+ (void)initialize;
+ (void)setEnv:(DLEnv *)env forModuleName:(NSString *)moduleName;
+ (DLEnv *)envForModuleName:(NSString *)moduleName;
+ (void)removeModule:(NSString *)moduleName;
+ (NSMapTable<NSString *, DLEnv *> *)modules;
- (instancetype)init;
- (instancetype)initWithModuleName:(NSString *)name isUserDefined:(BOOL)isUserDefined;
- (instancetype)initWithEnv:(DLEnv *)env;
- (instancetype)initWithEnv:(DLEnv *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (id<DLDataProtocol>)resolveFault:(id<DLDataProtocol>)object forKey:(DLSymbol *)key inEnv:(DLEnv *)env;
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
