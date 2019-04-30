//
//  Env.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;

@interface Env : NSObject
@property (nonatomic, readwrite, copy) Env *outer;
@property (nonatomic, readwrite, retain) NSMutableDictionary *data;
- (instancetype)initWithEnv:(Env *)env;
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs;
- (void)setObject:(id<JSDataProtocol>)value forSymbol:(JSSymbol *)key;
- (Env *)findEnvForKey:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key;
@end

NS_ASSUME_NONNULL_END
