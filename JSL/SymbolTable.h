//
//  SymbolTable.h
//  JSL
//
//  Created by jsloop on 22/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSSymbol.h"
#import "NSMapTable+JSHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;

@interface SymbolKey : NSObject
@property (nonatomic, readwrite) NSString *name;
@property (nonatomic, readwrite) NSString *moduleName;
+ (SymbolKey *)fromSymbol:(JSSymbol *)symbol;
- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName;
- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;
@end

@interface SymbolTable : NSObject
@property (nonatomic, readwrite) NSMapTable <SymbolKey *, NSMutableArray<JSSymbol *> *> *table;
- (instancetype)init;
- (NSMutableArray * _Nullable)arrayForKey:(SymbolKey *)key;
- (void)setKey:(JSSymbol *)key;
- (id<JSDataProtocol> _Nullable)symbolForKey:(JSSymbol *)key;
- (JSSymbol * _Nullable)dataSymbolForKey:(JSSymbol *)key symbolArray:(NSMutableArray *)array;
- (JSSymbol * _Nullable)functionSymbolForKey:(JSSymbol *)key symbolArray:(NSMutableArray *)array;
@end

NS_ASSUME_NONNULL_END
