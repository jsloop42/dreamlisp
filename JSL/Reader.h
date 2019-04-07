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

NS_ASSUME_NONNULL_BEGIN

@class JSData;

@interface Reader : NSObject
- (instancetype)initWithTokens:(NSMutableArray *)array;
- (nullable NSString *)next;
- (nullable NSString *)peek;
- (void)pass;
- (nullable JSData *)readString:(NSString *)string;
- (nullable JSData *)readForm;
- (nullable JSData *)readListStartingWith:(NSString *)leftParens;
- (nullable JSData *)readAtom;
- (NSMutableArray *)tokenize:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
