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
- (void)setObject:(id<JSDataProtocol>)obj forSymbol:(JSSymbol *)key;
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key;
@end

NS_ASSUME_NONNULL_END
