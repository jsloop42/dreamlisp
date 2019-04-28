//
//  JSBool.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSBool : JSData
- (instancetype)initWithBool:(BOOL)flag;
- (instancetype)initWithJSBool:(JSBool *)object;
- (BOOL)value;
- (BOOL)isEqual:(JSBool *)boolean;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
