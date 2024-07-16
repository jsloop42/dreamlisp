//
//  DLUtils.h
//  DreamLisp
//
//  Created by Jaseem V V on 06/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"
#import "DLCacheTable.h"
#import <objc/objc-runtime.h>

NS_ASSUME_NONNULL_BEGIN

@interface DLUtils : NSObject
+ (DLCacheTable *)cache;
+ (void)enableCache;
+ (void)disableCache;
+ (BOOL)isCacheEnabled;
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern;
+ (NSArray *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (double)timestamp;
+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object;
+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object isNative:(BOOL)isNative;
+ (DLNumber *)rand:(DLNumber *)upperBound;
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
+ (NSMutableArray *)hashMapToArray:(DLHashMap *)hashMap;
+ (NSMutableArray *)hashMapToHashMapArray:(DLHashMap *)hashMap;
+ (NSMutableArray *)hashMapToVectorArray:(DLHashMap *)hashMap;
+ (DLHashMap *)convertKeywordKeysToString:(DLHashMap *)hashmap;
+ (DLHashMap *)errorToHashMap:(NSError *)error;
+ (NSMutableDictionary *)dictionaryFromHashMap:(DLHashMap *)hashMap;
+ (NSMutableDictionary *)mapTableToDictionary:(NSMapTable *)table;
+ (id)convertFromDLTypeToFoundationType:(id<DLDataProtocol>)value;
+ (NSMutableDictionary *)hashMapToFoundationType:(DLHashMap *)hashMap;
+ (BOOL)isBoolNumber:(NSNumber *)num;
+ (id<DLDataProtocol>)convertFromFoundationTypeToDLType:(id)value;
+ (DLHashMap *)dictionaryToDLType:(NSMutableDictionary *)dict;
+ (DLHashMap *)decodeJSON:(DLString *)string;
+ (DLHashMap *)decodeJSONFromData:(NSData *)data;
+ (DLString *)encodeJSON:(DLHashMap *)hashMap;
+ (NSData *)encodeDictionaryToJSONData:(NSMutableDictionary *)dict;
#pragma mark String
+ (NSMutableArray *)stringToArray:(DLString *)string isNative:(BOOL)isNative;
+ (void)appendStringFromArray:(NSMutableArray *)array string:(DLString *)string;
+ (NSString *)httpMethodTypeToString:(DLKeyword *)methodType;
+ (NSString *)promptWithModule:(NSString *)moduleName;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
