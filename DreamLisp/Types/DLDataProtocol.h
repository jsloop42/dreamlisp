//
//  DLDataProtocol.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@protocol DLDataProtocol <NSObject, NSCopying, NSMutableCopying, NSSecureCoding>
@property (nonatomic, readonly) NSString *dataType;
@property (nonatomic, readonly) NSString *dataTypeName;
@property (nonatomic, readonly) NSUInteger hash;
@property (nonatomic, readonly) NSInteger sortValue;
@property (nonatomic, readwrite, retain) id value;
@property (nonatomic, readwrite, retain) id<DLDataProtocol> meta;
@property (nonatomic, readwrite, retain) NSString *moduleName;
@property (nonatomic, readwrite) BOOL isImported;
@property (nonatomic, readwrite) BOOL isMutable;
- (BOOL)hasMeta;
- (NSInteger)position;
- (id<DLDataProtocol>)setPosition:(NSInteger)position;
- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
