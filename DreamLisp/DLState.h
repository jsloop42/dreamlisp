//
//  State.h
//  DreamLisp
//
//  Created by Jaseem V V on 05/05/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLConst.h"
#import "DLTrie.h"
#import "DLLogger.h"
#import "DLConst.h"

NS_ASSUME_NONNULL_BEGIN

@class DLTrie;

@interface DLState : NSObject<NSSecureCoding>
@property (nonatomic, readwrite, retain) DLTrie *prefixTree;
@property (nonatomic, readwrite, retain) DLTrie *userPrefixTree;
@property (atomic, readonly) NSUInteger genSymCounter;
@property (atomic, readonly) NSUInteger currentGenSymCount;
@property (nonatomic, readwrite, assign) BOOL isVerbose;
@property (atomic, readonly) NSUInteger assocObjectCounter;
@property (atomic, readonly) NSUInteger currentAssocObjectCount;
+ (instancetype)shared;
+ (NSString *)currentModuleName;
+ (void)setCurrentModuleName:(NSString *)name;
- (void)addPrefix:(NSString *)prefix;
- (void)removePrefix:(NSString *)prefix;
- (void)initPrefixes:(NSArray *)prefixes;
@end

NS_ASSUME_NONNULL_END
