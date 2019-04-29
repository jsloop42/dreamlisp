//
//  JSAtom.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSAtom : JSData
+ (BOOL)isAtom:(id)object;
+ (JSAtom *)dataToAtom:(JSData *)data;
+ (JSAtom *)dataToAtom:(JSData *)data position:(NSInteger)position;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(JSData *)data;
- (instancetype)initWithMeta:(JSData *)meta atom:(JSAtom *)atom;
- (void)setValue:(JSData *)data;
- (JSData *)value;
- (BOOL)isEqual:(JSAtom *)atom;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
