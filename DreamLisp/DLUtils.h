//
//  DLUtils.h
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"
#import "DLCacheTable.h"
#import "DLState.h"
#import <objc/runtime.h>

NS_ASSUME_NONNULL_BEGIN

@class DLInvocationArgument;
@class DLObjcPropertyAttr;
@class DLMethod;
@class DLTrie;

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
+ (DLHashMap *)convertKeywordKeysToString:(DLHashMap *)hashmap;
+ (DLHashMap *)errorToHashMap:(NSError *)error;
+ (NSMutableDictionary *)dictionaryFromHashMap:(DLHashMap *)hashMap;
+ (NSMutableDictionary *)hashMapToFoundationType:(DLHashMap *)hashMap;
+ (DLHashMap *)dictionaryToDLType:(NSMutableDictionary *)dict;
+ (DLHashMap *)decodeJSON:(DLString *)string;
+ (DLHashMap *)decodeJSONFromData:(NSData *)data;
+ (DLString *)encodeJSON:(DLHashMap *)hashMap;
+ (NSData *)encodeDictionaryToJSONData:(NSMutableDictionary *)dict;
#pragma mark - String
+ (NSMutableArray *)stringToArray:(DLString *)string isNative:(BOOL)isNative;
+ (void)appendStringFromArray:(NSMutableArray *)array string:(DLString *)string;
+ (NSString *)promptWithModule:(NSString *)moduleName;
+ (NSString *)lispCaseToCamelCase:(NSString *)string;
+ (NSString *)camelCaseToLispCase:(NSString *)string;
+ (NSMutableArray *)splitString:(NSString *)string;
#pragma mark - Network
+ (NSString *)httpMethodTypeToString:(DLKeyword *)methodType;
#pragma mark - Objective-C RT
+ (BOOL)isBoolNumber:(NSNumber *)num;
+ (id<DLDataProtocol>)convertFromFoundationTypeToDLType:(id)value;
+ (id)convertFromDLTypeToFoundationType:(id<DLDataProtocol>)value;
+ (SEL)convertKeywordToSelector:(NSMutableArray<DLKeyword *> *)keywordArr;
+ (void)updateSELForMethod:(DLMethod *)method;
+ (void)updateSelectorStringForMethod:(DLMethod *)method;
+ (void)updatePropertyAttr:(DLObjcPropertyAttr *)attr;
+ (NSString *)propertyNameFromSelector:(NSString *)selector;
+ (NSString *)toAccessorVarFromGetter:(NSString *)string;
+ (NSString *)toSetterName:(NSString *)propName isCamelCase:(BOOL)isCamelCase;
+ (DLInvocationArgument *)convertToInvocationArgument:(id<DLDataProtocol>)elem;
#pragma mark - Build Helpers
//+ (BOOL)serializePrefixState:(DLState *)state toFile:(NSString *)file;
//+ (DLState * _Nullable)deserializePrefixStateFromFile:(NSString *)file;
//+ (BOOL)initializePrefixState;
//+ (BOOL)generatePrefixState;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
