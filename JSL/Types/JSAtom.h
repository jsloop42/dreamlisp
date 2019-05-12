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
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isAtom:(id)object;
+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(id<JSDataProtocol>)data;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta atom:(JSAtom *)atom;
- (BOOL)isEqual:(JSAtom *)atom;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
