//
//  TypeUtils.h
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSError.h"
#import "NSArray+JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

typedef NS_ENUM(NSUInteger, ArityPredicate) {
    ArityPredicateEq,
    ArityPredicateGreaterThan,
    ArityPredicateGreaterThanOrEq,
    ArityPredicateLessThan,
    ArityPredicateLessThanOrEq,
    ArityPredicateMax,
    ArityPredicateMin,
    ArityPredicateMultiple,
    ArityPredicateOdd
};

@interface TypeUtils: NSObject
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block;
+ (void)checkArity:(NSMutableArray *)data arity:(NSInteger)arity;
+ (void)checkArityCount:(NSUInteger)count arity:(NSInteger)arity;
+ (void)checkArity:(NSMutableArray *)xs arity:(NSInteger)arity fnName:(NSString *)fnName;
+ (void)checkArityCount:(NSInteger)count arity:(NSInteger)arity fnName:(NSString *)fnName;
+ (void)checkIndexBounds:(NSMutableArray *)xs index:(NSInteger)index;
+ (void)checkIndexBoundsCount:(NSInteger)count index:(NSInteger)index;
+ (void)checkArity:(NSMutableArray *)xs arities:(NSArray *)arities;
+ (void)checkArity:(NSMutableArray *)xs arity:(NSInteger)arity predicate:(enum ArityPredicate)predicate;
@end

extern NSInteger sortAscending(id obj1, id obj2, void *context);
extern NSInteger sortDescending(id obj1, id obj2, void *context);

NS_ASSUME_NONNULL_END
