//
//  DLMockStdIOService.h
//  DreamLispTests
//
//  Created by Jaseem V V on 29/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLStdIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLMockStdIOService : NSObject <DLStdIOServiceDelegate>
@property (nonatomic, readwrite) NSString *input;
@property (nonatomic, readwrite) NSString *output;
@end

NS_ASSUME_NONNULL_END
