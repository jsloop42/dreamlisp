//
//  TypeUtils.h
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSError.h"

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
+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block;
+ (void)checkArity:(NSMutableArray *)data arity:(NSUInteger)arity;
+ (void)checkArityCount:(NSUInteger)count arity:(NSUInteger)arity;
+ (void)checkArity:(NSMutableArray *)xs arity:(NSUInteger)arity fnName:(NSString *)fnName;
+ (void)checkArityCount:(NSUInteger)count arity:(NSUInteger)arity fnName:(NSString *)fnName;
+ (void)checkIndexBounds:(NSMutableArray *)xs index:(NSUInteger)index;
+ (void)checkArity:(NSMutableArray *)xs arities:(NSArray *)arities;
+ (void)checkArity:(NSMutableArray *)xs arity:(NSUInteger)arity predicate:(enum ArityPredicate)predicate;
@end

extern NSInteger sortAscending(id obj1, id obj2, void *context);
extern NSInteger sortDescending(id obj1, id obj2, void *context);

NS_ASSUME_NONNULL_END
