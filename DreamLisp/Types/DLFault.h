//
//  DLFault.h
//  DreamLisp
//
//  Created by Jaseem V V on 19/05/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLFault : NSObject <DLDataProtocol>
+ (BOOL)isFault:(id)object;
- (instancetype)init;
- (instancetype)initWithModule:(NSString *)moduleName isImportFault:(BOOL)isImportFault;
@end

NS_ASSUME_NONNULL_END
