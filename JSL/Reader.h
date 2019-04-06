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

@class JSData;

NS_ASSUME_NONNULL_BEGIN

@interface Reader : NSObject
- (instancetype)initWithTokens:(NSMutableArray *)array;
- (NSString *)next;
- (NSString *)peek;
- (void)pass;
- (JSData *)readString:(NSString *)string;
- (JSData *)readForm;
- (JSData *)readListStartingWith:(NSString *)leftParens;
- (JSData *)readAtom;
- (NSMutableArray *)tokenize:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
