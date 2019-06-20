//
//  Utils.h
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "CacheTable.h"

NS_ASSUME_NONNULL_BEGIN

//@class JSVector;

@interface Utils : NSObject
+ (CacheTable *)cache;
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern;
+ (NSArray *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (double)timestamp;
+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object;
+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object isNative:(BOOL)isNative;
#pragma mark - List
+ (JSList *)addObjectsToList:(JSList *)list fromList:(JSList *)aList;
+ (JSList *)addObjectsToList:(JSList *)list fromHashMap:(JSHashMap *)hashMap;
#pragma mark - Vector
+ (JSVector *)addObjectsToVector:(JSVector *)vector fromList:(JSList *)list;
+ (JSVector *)addObjectsToVector:(JSVector *)vector fromVector:(JSVector *)aVector;
+ (JSVector *)addObjectsToVector:(JSVector *)vector fromHashMap:(JSHashMap *)hashMap;
#pragma mark - HashMap
+ (JSHashMap *)addObjectsToHashMap:(JSHashMap *)hashMap fromList:(JSList *)list;
+ (JSHashMap *)addObjectsToHashMap:(JSHashMap *)hashMap fromHashMap:(JSHashMap *)aHashMap;
+ (JSList *)hashMapToList:(JSHashMap *)hashMap;
+ (JSVector *)hashMapToVector:(JSHashMap *)hashMap;
#pragma mark String
+ (NSMutableArray *)stringToArray:(JSString *)string isNative:(BOOL)isNative;
+ (void)appendStringFromArray:(NSMutableArray *)array string:(JSString *)string;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
