//
//  MockStdIOService.h
//  JSLTests
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "StdIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

@interface MockStdIOService : NSObject <StdIOServiceDelegate>
@property (nonatomic, readwrite) NSString *input;
@property (nonatomic, readwrite) NSString *output;
@end

NS_ASSUME_NONNULL_END
