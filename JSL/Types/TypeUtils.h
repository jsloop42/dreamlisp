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

@interface TypeUtils: NSObject
+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block;
+ (void)checkArity:(NSMutableArray *)data arity:(NSUInteger)arity;
+ (void)checkIndexBounds:(NSMutableArray *)xs index:(NSUInteger)index;
@end

NS_ASSUME_NONNULL_END
