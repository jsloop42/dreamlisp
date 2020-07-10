//
//  DLData.h
//  DreamLisp
//
//  Created by Jaseem V V on 07/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLState.h"
#import "DLError.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLData : NSObject <DLDataProtocol>
+ (BOOL)isData:(id)object;
+ (DLData *)dataToData:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLData *)dataToData:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)initWithData:(NSData *)data;
- (instancetype)initWithDLData:(DLData *)data;
@end

NS_ASSUME_NONNULL_END
