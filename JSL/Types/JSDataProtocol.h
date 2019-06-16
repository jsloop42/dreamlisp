//
//  JSData.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@protocol JSDataProtocol <NSObject, NSCopying, NSMutableCopying>
@property (nonatomic, readonly) NSString *dataType;
@property (nonatomic, readonly) NSString *dataTypeName;
@property (nonatomic, readonly) NSUInteger hash;
@property (nonatomic, readonly) NSInteger sortValue;
@property (nonatomic, readwrite) id value;
@property (nonatomic, readwrite) id<JSDataProtocol> meta;
@property (nonatomic, readwrite) BOOL isImported;
@property (nonatomic, readwrite) NSString *moduleName;
- (BOOL)hasMeta;
- (NSInteger)position;
- (id<JSDataProtocol>)setPosition:(NSInteger)position;
- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
