//
//  JSLazySequence.h
//  JSL
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSList.h"
#import "JSString.h"
#import "JSFunction.h"
#import "JSError.h"
#import "TypeUtils.h"

NS_ASSUME_NONNULL_BEGIN

@class JSFunction;

@interface JSLazySequence : NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSUInteger index;
@property (nonatomic, readwrite) NSUInteger length;
@property (nonatomic, readwrite) enum SequenceType sequenceType;
@property (nonatomic, readwrite) NSMutableArray<JSFunction *> *fns;
+ (BOOL)isLazySequence:(id)object;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)initWithArray:(NSMutableArray *)array sequenceType:(enum SequenceType)sequenceType;
- (id<JSDataProtocol>)next;
- (BOOL)hasNext;
@end

NS_ASSUME_NONNULL_END
