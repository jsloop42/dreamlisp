//
//  JSData.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@class JSData;

@protocol JSDataProtocol <NSObject, NSCopying, NSMutableCopying>
@property (nonatomic, readonly) NSString *dataType;
@property (nonatomic, readonly) NSString *dataTypeName;
@property (nonatomic, readwrite) id value;
@property (nonatomic, readwrite) JSData *meta;
- (BOOL)hasMeta;
@end

@protocol JSHashable <JSDataProtocol>
- (NSUInteger)hash;
@end

@protocol JSEquatable <JSDataProtocol>
- (BOOL)isEqual:(JSData *)object;
@end


@interface JSData : NSObject <JSDataProtocol, JSHashable, JSEquatable>
@end

NS_ASSUME_NONNULL_END
