//
//  DLTrie.h
//  DreamLisp
//
//  Created by Jaseem V V on 17/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLResultTypes.h"
#import "NSMutableArray+DLCat.h"
#import "DLTypeUtils.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLTrieSearchResult: NSObject
@property (nonatomic, readwrite, retain) NSMutableArray *prefixes;
@property (nonatomic, readwrite, assign) BOOL isExist;
@property (nonatomic, readwrite, assign) BOOL isCaps;
@end

@interface DLTrie : NSObject<NSSecureCoding>
@property (nonatomic, readwrite, retain) NSString *name;
@property (nonatomic, readwrite, retain) id value;
@property (nonatomic, readwrite, retain) NSDecimalNumber *weight;
@property (nonatomic, readwrite, assign) BOOL isPathExists;
@property (nonatomic, readwrite, retain) NSMutableArray *children;
@property (nonatomic, readwrite, retain) NSData *sortHint;
@property (nonatomic, readwrite, assign) BOOL *isRoot;
- (BOOL)insert:(NSString *)string;
- (DLTrie * _Nullable)findNodeWithName:(NSString *)string;
- (DLTrieSearchResult *)search:(NSString *)string isResultInCaps:(BOOL)isCaps;
- (DLRet)delete:(NSString *)name;
@end

NS_ASSUME_NONNULL_END
