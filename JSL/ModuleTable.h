//
//  ModuleTable.h
//  JSL
//
//  Created by jsloop on 12/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSSymbol.h"

NS_ASSUME_NONNULL_BEGIN

@class JSSymbol;

@interface ModuleTable : NSObject
@property (nonatomic, readwrite) NSMapTable<JSSymbol *, id<JSDataProtocol>> *table;
@property (nonatomic, readwrite) NSString *name;
- (instancetype)init;
- (instancetype)initWithModuleTable:(ModuleTable *)table;
- (void)setObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key;
- (void)updateObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key;
- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key;
- (void)removeAllObjects;
- (NSUInteger)count;
- (NSArray<JSSymbol *> *)allKeys;
- (NSArray<id<JSDataProtocol>> *)allObjects;
@end

NS_ASSUME_NONNULL_END
