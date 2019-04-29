//
//  JSNil.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSNil : JSData
+ (BOOL)isNil:(id)object;
- (instancetype)init;
- (BOOL)isEqual:(JSNil *)object;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
