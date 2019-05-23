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
@property (nonatomic, readwrite) id value;
@property (nonatomic, readwrite) id<JSDataProtocol> meta;
@property (nonatomic, readwrite) BOOL isImported;
- (BOOL)hasMeta;
- (NSInteger)position;
- (id<JSDataProtocol>)setPosition:(NSInteger)position;
@end

NS_ASSUME_NONNULL_END
