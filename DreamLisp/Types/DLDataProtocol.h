//
//  DLDataProtocol.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** The contract that all DreamLisp data types conforms to. */
@protocol DLDataProtocol <NSObject, NSCopying, NSMutableCopying, NSSecureCoding>
/** The data type associated with Objective-C. */
@property (nonatomic, readonly) NSString *dataType;
/** The data type name associated with DreamLisp. */
@property (nonatomic, readonly) NSString *dataTypeName;
/** Hash of the data type used mainly in comparison. */
@property (nonatomic, readonly) NSUInteger hash;
/** Hash value used for sorting. */
@property (nonatomic, readonly) NSInteger sortValue;
/** The underlying data. */
@property (nonatomic, readwrite, retain) id value;
/** Metadata associated with the data type. */
@property (nonatomic, readwrite, retain) id<DLDataProtocol> meta;
/** Module to which the variable that holds the data type belongs to. */
@property (nonatomic, readwrite, retain) NSString *moduleName;
/** Is the variable associated with the data type imported from another module. */
@property (nonatomic, readwrite) BOOL isImported;
/** Represents whether the data type is mutable. By default it is immutable. */
@property (nonatomic, readwrite) BOOL isMutable;
- (BOOL)hasMeta;
/** The position of the data type in the expression. */
- (NSInteger)position;
/** Sets the position of the data type in the given expression. */
- (id<DLDataProtocol>)setPosition:(NSInteger)position;
/** Checks if the two data types are equal. */
- (BOOL)isEqual:(id)object;
/** The hash of the data type used in comparison. */
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
