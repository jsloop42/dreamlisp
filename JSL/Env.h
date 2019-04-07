//
//  Env.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;
@class JSData;

@interface Env : NSObject
@property (nonatomic, readwrite, copy) Env *outer;
@property (nonatomic, readwrite, copy) NSMutableDictionary *data;
- (instancetype)initWithEnv:(Env *)env;

/**
 Initializes environment with an outer environment and binds symbols with expressions.

 @param env The outer environment.
 @param binds A array of `JSSymbol` symbols.
 @param exprs A array of `JSData` expressions.
 */
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (void)setValue:(JSData *)value forSymbol:(JSSymbol *)key;
- (Env *)findEnvForKey:(JSSymbol *)key;
- (JSData *)valueForSymbol:(JSSymbol *)key;
@end

NS_ASSUME_NONNULL_END
