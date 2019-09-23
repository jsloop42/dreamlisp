//
//  State.h
//  DreamLisp
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
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
@property (atomic, readwrite, retain) NSString *currentModuleName;
@property (atomic, readonly) NSUInteger assocObjectCounter;
@property (atomic, readonly) NSUInteger currentAssocObjectCount;
+ (instancetype)shared;
- (void)addPrefix:(NSString *)prefix;
- (void)removePrefix:(NSString *)prefix;
- (void)initPrefixes:(NSArray *)prefixes;
@end

NS_ASSUME_NONNULL_END
