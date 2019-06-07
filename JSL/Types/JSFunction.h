//
//  JSFunction.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "Env.h"

NS_ASSUME_NONNULL_BEGIN

@class Env;

@interface JSFunction: NSObject <JSDataProtocol>
// ([id<JSDataProtocol>]) -> id<JSDataProtocol>
@property (nonatomic, copy) id<JSDataProtocol> (^fn)(NSMutableArray *);
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
+ (BOOL)isFunction:(id)object;
+ (JSFunction *)dataToFunction:(id<JSDataProtocol>)data;
+ (JSFunction *)dataToFunction:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(id<JSDataProtocol> (^)(NSMutableArray *))fn;
- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(id<JSDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name;
- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name;
- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn argCount:(NSInteger)count name:(NSString *)name;
- (instancetype)initWithMacro:(JSFunction *)func;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta func:(JSFunction *)func;
- (instancetype)initWithFunction:(JSFunction *)function;
- (BOOL)isVariadic;
- (id<JSDataProtocol>)apply;
- (id<JSDataProtocol>)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
