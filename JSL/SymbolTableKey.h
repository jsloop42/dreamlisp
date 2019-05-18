//
//  SymbolTableKey.h
//  JSL
//
//  Created by jsloop on 18/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSSymbol.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;

@interface SymbolTableKey : NSObject
@property (nonatomic, readwrite) NSInteger arity;
@property (nonatomic, readwrite) NSString *initialValue;
@property (nonatomic, readwrite) NSString *moduleName;
- (instancetype)initWithSymbol:(JSSymbol *)symbol;
- (instancetype)initWithKey:(SymbolTableKey *)key;
- (BOOL)isEqual:(id)symbol;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
