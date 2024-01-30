//
//  DLLazyFunction.h
//  DreamLisp
//
//  Created by jsloop on 30/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLEnv.h"
#import "DLLazySequence.h"

NS_ASSUME_NONNULL_BEGIN

@class DLEnv;
@class DLLazySequence;

@interface DLLazyFunction: NSObject <DLDataProtocol>
// ([id<DLDataProtocol>]) -> id<DLDataProtocol>
@property (nonatomic, copy) void (^fn)(DLLazySequence *seq, NSMutableArray *xs);
@property (nonatomic, readwrite, retain, nullable) id<DLDataProtocol> ast;
// [Symbols]
@property (nonatomic, readwrite, retain, nullable) NSMutableArray *params;
@property (nonatomic, readwrite, retain, nullable) DLEnv *env;
@property (nonatomic, readwrite, assign, getter=isMacro) BOOL macro;
/**
  The arity count of the function. For variadic functions it returns @c n and @c n+m where @c m is the non-variadic arguments if present.

 @code
 (def a (fn (&more) nil))  ; a/n
 (def a (fn (x y &more) nil))  ; a/n+2
 @endcode
 */
@property (nonatomic, readwrite) NSInteger argsCount;
@property (nonatomic, readwrite, retain) NSString *name;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isLazyFunction:(id)object;
+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data;
+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data position:(NSInteger)position;
+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(DLEnv *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn;
- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(DLEnv *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name;
- (instancetype)initWithFn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name;
- (instancetype)initWithFn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn argCount:(NSInteger)count name:(NSString *)name;
- (instancetype)initWithMacro:(DLLazyFunction *)func;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta func:(DLLazyFunction *)func;
- (instancetype)initWithFunction:(DLLazyFunction *)function;
- (BOOL)isVariadic;
- (void)apply:(DLLazySequence *)seq;
- (void)apply:(NSMutableArray *)args forLazySequence:(DLLazySequence *)seq;
@end

NS_ASSUME_NONNULL_END
