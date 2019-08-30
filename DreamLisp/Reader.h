//
//  Reader.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "Const.h"
#import "DLError.h"

NS_ASSUME_NONNULL_BEGIN

@interface Reader : NSObject
@property (nonatomic, readonly) NSUInteger position;
@property (nonatomic, readwrite) NSString *moduleName;
- (instancetype)initWithTokens:(NSMutableArray *)array moduleName:(NSString *)name;
- (nullable NSString *)next;
- (nullable NSString *)peek;
- (void)pass;
- (nullable NSMutableArray<id<DLDataProtocol>> *)readString:(NSString *)string;
- (nullable id<DLDataProtocol>)readForm;
- (nullable id<DLDataProtocol>)readListStartingWith:(NSString *)leftParens;
- (nullable id<DLDataProtocol>)readAtom;
- (NSMutableArray *)tokenize:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
