//
//  ModuleTable.h
//  DreamLisp
//
//  Created by jsloop on 12/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLSymbol.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;

@interface ModuleTable : NSObject
@property (nonatomic, readwrite, retain) NSMapTable<DLSymbol *, id<DLDataProtocol>> *table;
@property (nonatomic, readwrite, retain) NSString *name;
- (instancetype)init;
- (instancetype)initWithModuleTable:(ModuleTable *)table;
- (void)setObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key;
- (void)updateObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key;
- (id<DLDataProtocol> _Nullable)objectForSymbol:(DLSymbol *)key;
- (void)removeAllObjects;
- (NSUInteger)count;
- (NSArray<DLSymbol *> *)allKeys;
- (NSArray<id<DLDataProtocol>> *)allObjects;
@end

NS_ASSUME_NONNULL_END
