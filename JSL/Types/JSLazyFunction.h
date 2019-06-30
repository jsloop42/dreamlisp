//
//  JSLazyFunction.h
//  JSL
//
//  Created by jsloop on 30/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "Env.h"
#import "JSLazySequence.h"

NS_ASSUME_NONNULL_BEGIN

@class Env;
@class JSLazySequence;

@interface JSLazyFunction: NSObject <JSDataProtocol>
// ([id<JSDataProtocol>]) -> id<JSDataProtocol>
@property (nonatomic, copy) void (^fn)(JSLazySequence *seq, NSMutableArray *xs);
@property (nonatomic, readwrite, copy, nullable) id<JSDataProtocol> ast;
// [Symbols]
@property (nonatomic, readwrite, retain, nullable) NSMutableArray *params;
@property (nonatomic, readwrite, copy, nullable) Env *env;
@property (nonatomic, readwrite, assign, getter=isMacro) BOOL macro;
/**
  The arity count of the function. For variadic functions it returns @c n and @c n+m where @c m is the non-variadic arguments if present.

 @code
 (def a (fn (&more) nil))  ; a/n
 (def a (fn (x y &more) nil))  ; a/n+2
 @endcode
 */
@property (nonatomic, readwrite) NSInteger argsCount;
@property (nonatomic, readwrite) NSString *name;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isLazyFunction:(id)object;
+ (JSLazyFunction *)dataToLazyFunction:(id<JSDataProtocol>)data;
+ (JSLazyFunction *)dataToLazyFunction:(id<JSDataProtocol>)data position:(NSInteger)position;
+ (JSLazyFunction *)dataToLazyFunction:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(void (^)(JSLazySequence *seq, NSMutableArray *xs))fn;
- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(void (^)(JSLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name;
- (instancetype)initWithFn:(void (^)(JSLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name;
- (instancetype)initWithFn:(void (^)(JSLazySequence *seq, NSMutableArray *xs))fn argCount:(NSInteger)count name:(NSString *)name;
- (instancetype)initWithMacro:(JSLazyFunction *)func;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta func:(JSLazyFunction *)func;
- (instancetype)initWithFunction:(JSLazyFunction *)function;
- (BOOL)isVariadic;
- (void)apply:(JSLazySequence *)seq;
- (void)apply:(NSMutableArray *)args forLazySequence:(JSLazySequence *)seq;
@end

NS_ASSUME_NONNULL_END
