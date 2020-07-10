//
//  DLSymbol.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLFunction.h"
#import "DLList.h"
#import "NSString+DLCat.h"
#import "NSMutableArray+DLCat.h"
#import "DLError.h"
#import "DLState.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLFunction;
@class DLList;

@interface DLSymbol: NSObject <DLDataProtocol>
@property (nonatomic, readwrite) NSInteger arity;
@property (nonatomic, readwrite) NSInteger initialArity;
@property (nonatomic, readwrite) BOOL isFunction;
@property (nonatomic, readonly) BOOL hasNArity;
@property (nonatomic, readwrite, retain) NSString *fnName;
@property (nonatomic, readwrite, retain) NSString *initialModuleName;
@property (nonatomic, readwrite) BOOL isQualified;
@property (nonatomic, readwrite) BOOL isModule;
@property (nonatomic, readwrite) BOOL isFault;
@property (nonatomic, readwrite) BOOL isCore;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isSymbol:(id)object;
+ (BOOL)isSymbol:(id)object withName:(NSString *)name;
+ (DLSymbol *)dataToSymbol:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLSymbol *)dataToSymbol:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
+ (DLSymbol *)processName:(NSString *)name;
+ (DLSymbol *)symbolWithArityCheck:(DLSymbol *)symbol withObject:(id)object;
+ (NSComparisonResult)compareSymbol:(DLSymbol *)aSymbol withSymbol:(DLSymbol *)bSymbol;
// Should not instantiate with empty name
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName;
- (instancetype)initWithFunction:(DLFunction *)func name:(NSString *)name moduleName:(NSString *)moduleName;
- (instancetype)initWithArity:(NSInteger)arity symbol:(DLSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(DLSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string moduleName:(NSString *)moduleName;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string moduleName:(NSString *)moduleName;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta symbol:(DLSymbol *)symbol;
- (instancetype)initWithMeta:(_Nullable id<DLDataProtocol>)meta name:(NSString *)name;
- (DLSymbol *)toNArity;
- (DLSymbol *)resetArity;
- (void)updateArity;
- (void)resetModuleName;
- (NSString *)string;
- (void)copyMeta:(id<DLDataProtocol>)object;
- (void)copyProperties:(DLSymbol *)symbol;
- (BOOL)isEqualToName:(NSString *)name;
@end

NS_ASSUME_NONNULL_END
