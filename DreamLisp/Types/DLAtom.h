//
//  DLAtom.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLError.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLAtom : NSObject <DLDataProtocol>
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isAtom:(id)object;
+ (DLAtom *)dataToAtom:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLAtom *)dataToAtom:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(id<DLDataProtocol>)data;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta atom:(DLAtom *)atom;
@end

NS_ASSUME_NONNULL_END
