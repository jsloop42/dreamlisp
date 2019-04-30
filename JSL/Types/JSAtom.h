//
//  JSAtom.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSAtom : NSObject <JSDataProtocol>
+ (BOOL)isAtom:(id)object;
+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data;
+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(id<JSDataProtocol>)data;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta atom:(JSAtom *)atom;
- (void)setValue:(id<JSDataProtocol>)data;
- (id<JSDataProtocol>)value;
- (BOOL)isEqual:(JSAtom *)atom;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
