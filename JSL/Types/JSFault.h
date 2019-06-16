//
//  JSFault.h
//  JSL
//
//  Created by jsloop on 19/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSFault : NSObject <JSDataProtocol>
+ (BOOL)isFault:(id)object;
- (instancetype)init;
- (instancetype)initWithModule:(NSString *)moduleName isImportFault:(BOOL)isImportFault;
@end

NS_ASSUME_NONNULL_END
