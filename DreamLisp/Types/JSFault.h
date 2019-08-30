//
//  DLFault.h
//  DreamLisp
//
//  Created by jsloop on 19/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLFault : NSObject <DLDataProtocol>
+ (BOOL)isFault:(id)object;
- (instancetype)init;
- (instancetype)initWithModule:(NSString *)moduleName isImportFault:(BOOL)isImportFault;
@end

NS_ASSUME_NONNULL_END
