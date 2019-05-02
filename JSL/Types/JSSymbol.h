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
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSSymbol: NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSInteger arity;
@property (nonatomic, readwrite) NSInteger initialArity;
@property (nonatomic, readwrite) BOOL isFunction;
@property (nonatomic, readwrite) BOOL hasNArity;
+ (BOOL)isSymbol:(id)object;
+ (BOOL)isSymbol:(id)object withName:(NSString *)name;
+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol;
- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol;
- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name;
- (NSString *)name;
- (JSSymbol *)toNArity;
- (JSSymbol *)resetArity;
- (NSString *)string;
- (BOOL)isEqual:(id)sym;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
