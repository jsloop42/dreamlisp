//
//  DLSet.h
//  DreamLisp
//
//  Created by Jaseem V V on 24.07.2024.
//  Copyright © 2024 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLError.h"
#import "DLLogger.h"
#import "DLConst.h"
#import "DLTypeUtils.h"
#import "NSMutableSet+DLSet.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLSet : NSObject <DLDataProtocol, NSSecureCoding>
+ (BOOL)isSet:(id)object;
+ (DLSet *)dataToSet:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLSet *)dataToSet:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithSet:(NSMutableSet *)set;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta set:(DLSet *)hashSet;
- (NSUInteger)count;
- (BOOL)isEmpty;
- (NSMutableSet *)unionSet:(NSMutableSet *)set;
/** Checks if the given object is a member of the set. */
- (BOOL)contains:(id<DLDataProtocol>)object;
- (NSMutableSet *)removeImmutable:(id<DLDataProtocol>)object;
- (NSArray *)allObjects;
- (BOOL)isEqual:(id)object;
@end

NS_ASSUME_NONNULL_END
