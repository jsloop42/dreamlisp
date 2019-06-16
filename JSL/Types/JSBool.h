//
//  JSBool.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSBool : NSObject <JSDataProtocol>
+ (BOOL)isBool:(id)object;
- (instancetype)initWithBool:(BOOL)flag;
- (instancetype)initWithJSBool:(JSBool *)object;
- (BOOL)value;
@end

NS_ASSUME_NONNULL_END
