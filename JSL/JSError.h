//
//  JSError.h
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types/Types.h"

NS_ASSUME_NONNULL_BEGIN

@class JSData;

extern NSString *ArityError;
extern NSString *DataTypeMismatch;
extern NSString *DataTypeMismatchWithArity;
extern NSString *IndexOutOfBounds;
extern NSString *JSLException;
extern NSString *SymbolNotFound;

@interface JSError : NSObject
@property (nonatomic, readwrite) NSString *description;
- (instancetype)initWithDescription:(NSString *)description;
- (instancetype)initWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithData:(JSData *)data;
- (instancetype)initWithArray:(NSMutableArray *)array;
- (NSMutableDictionary *)value;
- (void)throw;
@end

NS_ASSUME_NONNULL_END
