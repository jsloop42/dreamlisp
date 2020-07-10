//
//  DLTrie.m
//  DreamLisp
//
//  Created by Jaseem V V on 17/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLTrie.h"

/*!
 A result type used as a return in trie search.
 */
@implementation DLTrieSearchResult {
    /*!
     An array containing prefix words.
     */
    NSMutableArray *_prefixes;
    /*!
      Indicates whether the match is exact or next best.
     */
    BOOL _isExist;
    /*!
     Indicates if the prefixes are in uppercased form.
     */
    BOOL _isCaps;
}

@synthesize prefixes = _prefixes;
@synthesize isExist = _isExist;
@synthesize isCaps = _isCaps;

- (instancetype)init {
    self = [super init];
    if (self) {
        _prefixes = [NSMutableArray new];
        _isExist = NO;
        _isCaps = NO;
    }
    return self;
}

@end

/*!
 A trie data structure.
 */
@implementation DLTrie {
    /*!
     The name that the trie holds. Usually a letter, if we are tracking words. We can also use words, if we are tracking sentences.
     */
    NSString *_name;
    /*!
     Indicates a value usually used for in ordering between nodes.
     */
    NSDecimalNumber *_weight;
    /*!
     Any value associated at this particular path.
     */
    id _value;
    /*!
     Indicates that there is a path at this depth in the tree.
     */
    BOOL _isPathExist;
    /*!
     The child nodes this particular node has.
     */
    NSMutableArray *_children;
    /*!
     The sort hint from a sorting operation performed after an insertion
     */
    NSData *_sortHint;
    BOOL *_isRoot;
}

@synthesize name = _name;
@synthesize weight = _weight;
@synthesize value = _value;
@synthesize isPathExists = _isPathExist;
@synthesize children = _children;
@synthesize sortHint = _sortHint;
@synthesize isRoot = _isRoot;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

/*!
 Initializes a node with the given name.
 */
- (instancetype)initNodeWithName:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _name = [coder decodeObjectOfClass:[NSString class] forKey:@"DLTrie_name"];
        //_value = [coder decodeObjectOfClass:[NSObject class] forKey:@"DLTrie_value"];
        _weight = [coder decodeObjectOfClass:[NSDecimalNumber class] forKey:@"DLTrie_weight"];
        NSValue *isPathExistsValue = [coder decodeObjectOfClass:[NSValue class] forKey:@"DLTrie_isPathExists"];
        [isPathExistsValue getValue:&_isPathExist];
        NSMutableDictionary *dict = [coder decodeObjectOfClass:[NSMutableDictionary class] forKey:@"DLTrie_children"];
        _children = [dict objectForKey:@"children"];
        _sortHint = [coder decodeObjectOfClass:[NSData class] forKey:@"DLTrie_sortHint"];
        NSValue *isRootValue = [coder decodeObjectOfClass:[NSValue class] forKey:@"DLTrie_isRoot"];
        [isRootValue getValue:&_isRoot];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_name forKey:@"DLTrie_name"];
    //[coder encodeObject:_value forKey:@"DLTrie_value"];
    [coder encodeObject:_weight forKey:@"DLTrie_weight"];
    NSValue *isPathExistValue = [[NSValue alloc] initWithBytes:&_isPathExist objCType:@encode(BOOL)];
    [coder encodeObject:isPathExistValue forKey:@"DLTrie_isPathExists"];
    [coder encodeObject:[@{@"children": _children} mutableCopy] forKey:@"DLTrie_children"];
    [coder encodeObject:_sortHint forKey:@"DLTrie_sortHint"];
    NSValue *isRootValue = [[NSValue alloc] initWithBytes:&_isRoot objCType:@encode(BOOL)];
    [coder encodeObject:isRootValue forKey:@"DLTrie_isRoot"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _children = [NSMutableArray new];
}

/*!
 Inserts the given string into the trie.
 @returns BOOL Indicates if the operation was a success.
 */
- (BOOL)insert:(NSString *)string {
    NSMutableArray *strArr = [DLTypeUtils splitString:string];
    NSUInteger len = [strArr count];
    NSUInteger i;
    NSString *unit = nil;
    DLTrie *trie = self;
    DLTrie *aTrie = nil;
    for (i = 0; i < len; i++) {
        unit = [strArr objectAtIndex:i];
        aTrie = [trie findNodeWithName:unit];
        if (aTrie) {  /* Continue with the next value */
            trie = aTrie;
        } else {
            trie = [trie addNodeWithName:unit];
        }
    }
    [trie setIsPathExists:YES];
    return YES;
}

/*!
 Adds a node with given name into the current trie.
 @returns DLTrie
 */
- (DLTrie *)addNodeWithName:(NSString *)name {
    DLTrie *trie = [[DLTrie alloc] initNodeWithName:name];
    [self.children addObject:trie];
    return trie;
}

/*!
 Find node with the given value in the current trie.
 @returns DLTrie Returns the node if present else @c nil
 */
- (DLTrie * _Nullable)findNodeWithName:(NSString *)string {
    NSEnumerator *iter = [_children objectEnumerator];
    DLTrie *trie;
    BOOL isFound = NO;
    while ((trie = [iter nextObject]) != nil) {
        if ([trie.name isEqual:string]) {
            isFound = YES;
            break;
        }
    }
    return isFound ? trie : nil;
}

/*!
 Search if the given string exists in the trie. If not present the prefixes array will contain the next best match.

 @param string The search prefix in uppercase format.
 @param isCaps Indicates if the result needs to be in uppercase format.
 @returns DLTrieSearchResult
 */
- (DLTrieSearchResult *)search:(NSString *)string isResultInCaps:(BOOL)isCaps {
    NSMutableArray *xs = [DLTypeUtils splitString:string];
    NSEnumerator *iter = [xs objectEnumerator];
    NSString *elem = nil;
    DLTrie *trie = self;
    DLTrieSearchResult *result = [DLTrieSearchResult new];
    NSMutableArray *prefixes = result.prefixes;
    NSMutableString *str = [NSMutableString new];
    result.isCaps = isCaps;
    while ((elem = [iter nextObject]) != nil) {
        trie = [trie findNodeWithName:elem];
        if (trie) {
            if (trie.isPathExists) {
                [str appendString:isCaps ? trie.name : [trie.name lowercaseString]];
                [prefixes addObject:str];
                str = [NSMutableString new];
            } else {
                [str appendString:isCaps ? trie.name : [trie.name lowercaseString]];
            }
        } else {
            break;
        }
    }
    if (trie && trie.isPathExists) {
        result.isExist = YES;
    }
    return result;
}

/*!
 Removes the given string from the trie if present.

 @return DLRet
 */
- (DLRet)delete:(NSString *)name {
    NSMutableArray *arr = [DLTypeUtils splitString:name];
    NSEnumerator *iter = [arr objectEnumerator];
    DLTrie *trie = self;
    NSString *elem = nil;
    DLTrie *prevPrefix = nil;  /* The previous prefix node encountered if any */
    DLTrie *prevPrefixFirst = nil;  /* The node that follows the previous prefix, which if the name is a leaf node should be removed from the prevPrefix */
    BOOL encounteredPrevPrefix = NO;
    DLRet ret = DLRetOK;
    NSUInteger idx = 0;
    NSUInteger len = [arr count] - 1;
    while ((elem = [iter nextObject]) != nil) {
        idx++;
        trie = [trie findNodeWithName:elem];
        if (trie) {
            if (trie.isPathExists && idx < len) {
                prevPrefix = trie;
                encounteredPrevPrefix = YES;
            } else {
                if (encounteredPrevPrefix) {  /* The first node that follows the previous prefix */
                    prevPrefixFirst = trie;
                    encounteredPrevPrefix = NO;
                }
            }
        } else {
            ret = DLRetNotFound;
            break;  /* Trie path less than the given string => the string does not exist */
        }
    }
    if (ret == DLRetNotFound) return ret;
    if (trie && trie.isPathExists) {
        [trie setIsPathExists:NO];
    }
    if (![trie.children isEmpty]) {  /* Leaf node => clear the path till the previous trie */
        [prevPrefix.children removeObject:prevPrefixFirst];
    }
    return ret;
}

@end
