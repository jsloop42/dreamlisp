//
//  DLLazySequence.h
//  DreamLisp
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLList.h"
#import "DLString.h"
#import "DLFunction.h"
#import "DLLazyFunction.h"
#import "DLError.h"
#import "TypeUtils.h"

NS_ASSUME_NONNULL_BEGIN

@class DLLazyFunction;
@class DLFunction;

@interface DLLazySequenceFn : NSObject
@property (nonatomic, readwrite) DLLazyFunction *lazyFn;
@property (nonatomic, readwrite) DLFunction *fn;
- (instancetype)initWithLazyFunction:(DLLazyFunction *)lazyFunction;
- (instancetype)initWithLazyFunction:(DLLazyFunction *)lazyFunction fn:(DLFunction * _Nullable)fn;
@end

@interface DLLazySequence : NSObject <DLDataProtocol>
@property (nonatomic, readwrite) NSMutableArray *acc;
@property (nonatomic, readwrite) NSInteger index;
@property (nonatomic, readwrite) NSUInteger length;
@property (nonatomic, readwrite) enum SequenceType sequenceType;
@property (nonatomic, readwrite) NSMutableArray<DLLazySequenceFn *> *fns;
@property (nonatomic, readwrite) BOOL isNative;
@property (nonatomic, readwrite) BOOL isReverseEnumerate;
+ (BOOL)isLazySequence:(id)object;
+ (DLLazySequence *)dataToLazySequence:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLLazySequence *)dataToLazySequence:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithArray:(NSMutableArray *)array sequenceType:(enum SequenceType)sequenceType;
- (NSMutableArray *)value;
- (void)updateEnumerator;
- (id)next;
- (BOOL)hasNext;
- (void)addLazyFunction:(DLLazyFunction *)lazyFunction;
- (void)addLazyFunction:(DLLazyFunction *)lazyFunction fn:(DLFunction * _Nullable)fn;
- (void)apply;
@end

NS_ASSUME_NONNULL_END
