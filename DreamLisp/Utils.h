//
//  Utils.h
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "CacheTable.h"

NS_ASSUME_NONNULL_BEGIN

@interface Utils : NSObject
+ (CacheTable *)cache;
+ (void)enableCache;
+ (void)disableCache;
+ (BOOL)isCacheEnabled;
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern;
+ (NSArray *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (double)timestamp;
+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object;
+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object isNative:(BOOL)isNative;
#pragma mark - List
+ (DLList *)addObjectsToList:(DLList *)list fromList:(DLList *)aList;
+ (DLList *)addObjectsToList:(DLList *)list fromHashMap:(DLHashMap *)hashMap;
#pragma mark - Vector
+ (DLVector *)addObjectsToVector:(DLVector *)vector fromList:(DLList *)list;
+ (DLVector *)addObjectsToVector:(DLVector *)vector fromVector:(DLVector *)aVector;
+ (DLVector *)addObjectsToVector:(DLVector *)vector fromHashMap:(DLHashMap *)hashMap;
#pragma mark - HashMap
+ (DLHashMap *)addObjectsToHashMap:(DLHashMap *)hashMap fromList:(DLList *)list;
+ (DLHashMap *)addObjectsToHashMap:(DLHashMap *)hashMap fromHashMap:(DLHashMap *)aHashMap;
+ (void)appendObjectsToHashMap:(DLHashMap *)hashMap fromHashMap:(DLHashMap *)aHashMap;
+ (DLList *)hashMapToList:(DLHashMap *)hashMap;
+ (DLVector *)hashMapToVector:(DLHashMap *)hashMap;
+ (NSMutableArray *)hashMapToHashMapArray:(DLHashMap *)hashMap;
#pragma mark String
+ (NSMutableArray *)stringToArray:(DLString *)string isNative:(BOOL)isNative;
+ (void)appendStringFromArray:(NSMutableArray *)array string:(DLString *)string;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
