//
//  SymbolTable.h
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "NSMapTable+JSHashMap.h"
#import "JSSymbol.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;

@interface SymbolTable : NSObject
@property (nonatomic, readwrite) SymbolTable *outer;
@property (nonatomic, readonly) NSMapTable<NSString *, JSSymbol *> *table;
- (instancetype)init;
- (instancetype)initWithTable:(SymbolTable *)table;
- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol;
- (void)setSymbol:(JSSymbol *)symbol;
- (NSUInteger)count;
@end

NS_ASSUME_NONNULL_END
