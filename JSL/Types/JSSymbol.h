//
//  JSSymbol.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSSymbol: NSObject <JSDataProtocol>
+ (BOOL)isSymbol:(id)object;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol;
- (NSString *)name;
- (BOOL)isEqual:(JSSymbol *)sym;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
