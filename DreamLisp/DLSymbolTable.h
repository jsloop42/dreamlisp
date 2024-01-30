//
//  DLSymbolTable.h
//  DreamLisp
//
//  Created by jsloop on 22/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLSymbol.h"
#import "NSMapTable+DLHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;

@interface DLSymbolKey : NSObject
@property (nonatomic, readwrite, retain) NSString *name;
@property (nonatomic, readwrite, retain) NSString *moduleName;
+ (DLSymbolKey *)fromSymbol:(DLSymbol *)symbol;
- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName;
- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;
@end

@interface DLSymbolTable : NSObject
@property (nonatomic, readwrite, retain) NSMapTable <DLSymbolKey *, NSMutableArray<DLSymbol *> *> *table;
- (instancetype)init;
- (NSMutableArray * _Nullable)arrayForKey:(DLSymbolKey *)key;
- (void)setKey:(DLSymbol *)key;
- (id<DLDataProtocol> _Nullable)symbolForKey:(DLSymbol *)key;
- (DLSymbol * _Nullable)dataSymbolForKey:(DLSymbol *)key symbolArray:(NSMutableArray *)array;
- (DLSymbol * _Nullable)functionSymbolForKey:(DLSymbol *)key symbolArray:(NSMutableArray *)array;
@end

NS_ASSUME_NONNULL_END
