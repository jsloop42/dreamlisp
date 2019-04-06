//
//  Types.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@protocol JSDataProtocol<NSObject>
@property (nonatomic, readonly) NSString* dataType;
@end

@interface JSData : NSObject<JSDataProtocol> {
}
@end

@interface JSString : JSData
@property (nonatomic, readwrite) BOOL isKeyword;
- (instancetype)initWithString:(NSString*)str;
- (instancetype)initWithKeyword:(NSString *)str;
- (NSString*)value;
@end

@interface JSHashMap : JSData
@property (nonatomic, readwrite) JSData* meta;
- (NSUInteger)count;
@end

@interface JSList : JSData
@property (nonatomic, readwrite) JSData* meta;
- (instancetype)initWith:(NSArray*)list;
- (void)add:(JSData*)object;
- (void)add:(JSData*)object atIndex:(NSUInteger)index;
- (void)remove:(JSData*)object;
- (void)removeAtIndex:(NSUInteger)index;
- (NSMutableArray*)value;
- (NSUInteger)count;
- (JSData*)first;
- (JSData*)second;
- (JSData*)rest;
- (JSData*)last;
@end

@interface JSVector: JSList
@end

@interface JSNumber : JSData
@property (nonatomic, readwrite) JSData* meta;
- (instancetype)initWith:(float)num;
- (BOOL)isEqual:(JSNumber*)num;
- (float)value;
- (NSNumber*)val;
@end

@interface JSSymbol: JSData
- (NSString*)name;
- (BOOL)isEqual:(JSSymbol*)sym;
@end

@interface JSAtom : JSData
- (instancetype)initWith:(JSData*)data;
- (JSData*)value;
@end

@interface JSNil : JSData
@end

@interface JSTrue : JSData
@end

@interface JSFalse : JSData
@end

@interface JSFunction: JSData
@property (nonatomic, copy, nullable) JSData*(^fn)(NSMutableArray*);
@property (nonatomic, readwrite) JSData* isMacro;
@property (nonatomic, readwrite) JSData* meta;
@end

NS_ASSUME_NONNULL_END
