//
//  JSSymbol.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSFunction.h"
#import "JSList.h"
#import "NSString+JSDataProtocol.h"
#import "NSMutableArray+JSList.h"
#import "JSError.h"
#import "State.h"
#import "SymbolTable.h"
#import "SymbolTableKey.h"

NS_ASSUME_NONNULL_BEGIN

@class JSFunction;
@class JSList;
@class SymbolTableKey;

@interface JSSymbol: NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSInteger arity;
@property (nonatomic, readwrite) NSInteger initialArity;
@property (nonatomic, readwrite) NSString *initialValue;
@property (nonatomic, readwrite) BOOL isFunction;
@property (nonatomic, readonly) BOOL hasNArity;
@property (nonatomic, readwrite) NSString *fnName;
@property (nonatomic, readwrite) NSString *initialModuleName;
@property (nonatomic, readwrite) BOOL isQualified;
@property (nonatomic, readwrite) BOOL isModule;
@property (nonatomic, readwrite) BOOL isFault;
@property (nonatomic, readwrite) BOOL isCore;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isSymbol:(id)object;
+ (BOOL)isSymbol:(id)object withName:(NSString *)name;
+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object;
+ (void)updateProperties:(JSSymbol *)symbol list:(JSList *)list;
// Should not instantiate with empty name
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName;
- (instancetype)initWithFunction:(JSFunction *)func name:(NSString *)name moduleName:(NSString *)moduleName;
- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(JSSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string moduleName:(NSString *)moduleName;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol;
- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name;
- (JSSymbol *)gensym;
- (JSSymbol *)autoGensym;
- (BOOL)isGensym;
- (NSString *)name;
- (JSSymbol *)toNArity;
- (JSSymbol *)resetArity;
- (void)updateArity;
- (void)resetModuleName;
- (NSString *)string;
- (void)copyProperties:(JSSymbol *)symbol;
- (BOOL)isEqualToName:(NSString *)name;
- (BOOL)isEqual:(id)sym;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
