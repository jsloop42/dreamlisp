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
#import "JSLazyFunction.h"
#import "JSError.h"
#import "TypeUtils.h"

NS_ASSUME_NONNULL_BEGIN

@class JSLazyFunction;
@class JSFunction;

@interface JSLazySequenceFn : NSObject
@property (nonatomic, readwrite) JSLazyFunction *lazyFn;
@property (nonatomic, readwrite) JSFunction *fn;
- (instancetype)initWithLazyFunction:(JSLazyFunction *)lazyFunction;
- (instancetype)initWithLazyFunction:(JSLazyFunction *)lazyFunction fn:(JSFunction * _Nullable)fn;
@end

@interface JSLazySequence : NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSMutableArray *acc;
@property (nonatomic, readwrite) NSUInteger index;
@property (nonatomic, readwrite) NSUInteger length;
@property (nonatomic, readwrite) enum SequenceType sequenceType;
@property (nonatomic, readwrite) NSMutableArray<JSLazySequenceFn *> *fns;
@property (nonatomic, readwrite) BOOL isNative;
+ (BOOL)isLazySequence:(id)object;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithArray:(NSMutableArray *)array sequenceType:(enum SequenceType)sequenceType;
- (NSMutableArray *)value;
- (void)updateLength;
- (id)next;
- (BOOL)hasNext;
- (void)addLazyFunction:(JSLazyFunction *)lazyFunction;
- (void)addLazyFunction:(JSLazyFunction *)lazyFunction fn:(JSFunction * _Nullable)fn;
- (void)apply;
@end

NS_ASSUME_NONNULL_END
