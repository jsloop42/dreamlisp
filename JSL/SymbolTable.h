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
@property (nonatomic, readonly) NSMapTable<NSString *, JSSymbol *> *table;
@property (nonatomic, readonly) NSMapTable<NSString *, JSSymbol *> *expTable;
+ (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol;
+ (void)setSymbol:(JSSymbol *)symbol;
+ (void)removeSymbol:(JSSymbol *)symbol;
+ (void)startExpressionSymbolCapture;
+ (void)stopExpressionSymbolCapture;
+ (void)clearSymbolCapture;
+ (BOOL)isCapture;
+ (NSUInteger)count;
+ (NSUInteger)captureCount;
+ (NSUInteger)counter;
+ (NSInteger)currentCounter;
@end

NS_ASSUME_NONNULL_END
