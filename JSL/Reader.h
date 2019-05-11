//
//  Reader.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "Constants.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface Reader : NSObject
@property (nonatomic, readonly) NSUInteger position;
- (instancetype)initWithTokens:(NSMutableArray *)array;
- (nullable NSString *)next;
- (nullable NSString *)peek;
- (void)pass;
- (nullable NSMutableArray<id<JSDataProtocol>> *)readString:(NSString *)string;
- (nullable id<JSDataProtocol>)readForm;
- (nullable id<JSDataProtocol>)readListStartingWith:(NSString *)leftParens;
- (nullable id<JSDataProtocol>)readAtom;
- (NSMutableArray *)tokenize:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
