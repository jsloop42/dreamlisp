//
//  DLFunction.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "Env.h"

NS_ASSUME_NONNULL_BEGIN

@class Env;

@interface DLFunction: NSObject <DLDataProtocol>
// ([id<DLDataProtocol>]) -> id<DLDataProtocol>
@property (nonatomic, copy) id<DLDataProtocol> (^fn)(NSMutableArray *);
@property (nonatomic, readwrite, copy, nullable) id<DLDataProtocol> ast;
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
@property (nonatomic, readwrite, retain) NSString *name;
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isFunction:(id)object;
+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data;
+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data position:(NSInteger)position;
+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(id<DLDataProtocol> (^)(NSMutableArray *))fn;
- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(id<DLDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name;
- (instancetype)initWithFn:(id<DLDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name;
- (instancetype)initWithFn:(id<DLDataProtocol> (^)(NSMutableArray *))fn argCount:(NSInteger)count name:(NSString *)name;
- (instancetype)initWithMacro:(DLFunction *)func;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta func:(DLFunction *)func;
- (instancetype)initWithFunction:(DLFunction *)function;
- (BOOL)isVariadic;
- (id<DLDataProtocol>)apply;
- (id<DLDataProtocol>)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
