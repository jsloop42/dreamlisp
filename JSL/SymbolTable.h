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
#import "SymbolTableKey.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;
@class SymbolTableKey;

@interface SymbolTable : NSObject
@property (nonatomic, readwrite) SymbolTable *outer;
@property (nonatomic, readonly) NSMapTable<SymbolTableKey *, JSSymbol *> *table;
- (instancetype)init;
- (instancetype)initWithTable:(SymbolTable *)table;
- (void)merge:(SymbolTable *)table;
- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol;
- (void)setSymbol:(JSSymbol *)symbol;
- (void)clearAll;
- (NSUInteger)count;
@end

NS_ASSUME_NONNULL_END
