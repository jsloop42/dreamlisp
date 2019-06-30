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

@interface JSLazyFunction : NSObject
@property (nonatomic, readwrite) JSFunction *lazyFn;
@property (nonatomic, readwrite) JSFunction *fn;
- (instancetype)initWithLazyFunction:(JSFunction *)lazyFunction;
- (instancetype)initWithLazyFunction:(JSFunction *)lazyFunction fn:(JSFunction * _Nullable)fn;
@end

@interface JSLazySequence : NSObject <JSDataProtocol>
@property (nonatomic, readwrite) NSUInteger index;
@property (nonatomic, readwrite) NSUInteger length;
@property (nonatomic, readwrite) enum SequenceType sequenceType;
@property (nonatomic, readwrite) NSMutableArray<JSLazyFunction *> *fns;
+ (BOOL)isLazySequence:(id)object;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithArray:(NSMutableArray *)array sequenceType:(enum SequenceType)sequenceType;
- (id<JSDataProtocol>)next;
- (BOOL)hasNext;
//- (void)addFunction:(id<JSDataProtocol> (^)(NSMutableArray *))fn;
- (void)addLazyFunction:(JSFunction *)lazyFunction;
- (void)addLazyFunction:(JSFunction *)lazyFunction fn:(JSFunction * _Nullable)fn;
- (id<JSDataProtocol>)apply;
@end

NS_ASSUME_NONNULL_END
