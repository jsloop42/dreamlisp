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
#import "NSString+JSDataProtocol.h"
#import "NSMutableArray+JSList.h"
#import "JSError.h"
#import "State.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSSymbol: NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSInteger arity;
@property (nonatomic, readwrite) NSInteger initialArity;
@property (nonatomic, readwrite) NSString *initialValue;
@property (nonatomic, readwrite) BOOL isFunction;
@property (nonatomic, readonly) BOOL hasNArity;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isSymbol:(id)object;
+ (BOOL)isSymbol:(id)object withName:(NSString *)name;
+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object;
// Should not instantiate with empty name
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(JSSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string;
- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol;
- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name;
- (JSSymbol *)gensym;
- (JSSymbol *)autoGensym;
- (BOOL)isGensym;
- (NSString *)name;
- (JSSymbol *)toNArity;
- (JSSymbol *)resetArity;
- (NSString *)string;
- (void)copyProperties:(JSSymbol *)symbol;
- (BOOL)isEqualToName:(NSString *)name;
- (BOOL)isEqual:(id)sym;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END