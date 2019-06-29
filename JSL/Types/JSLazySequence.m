//
//  JSLazySequence.m
//  JSL
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSLazySequence.h"

@implementation JSLazySequence {
    NSMutableArray *_array;
    NSUInteger _position;
    NSUInteger _index;
    NSUInteger _length;
    BOOL _isImported;
    BOOL _isMutable;
    id<JSDataProtocol> _meta;
    NSString *_moduleName;
    enum SequenceType _sequenceType;
    NSMutableArray<JSFunction *> *_fns;
}

@synthesize isImported = _isImported;
@synthesize isMutable = _isMutable;
@synthesize meta = _meta;
@synthesize moduleName = _moduleName;
@synthesize value = _array;
@synthesize index = _index;
@synthesize length = _length;
@synthesize sequenceType = _sequenceType;
@synthesize fns = _fns;

+ (BOOL)isLazySequence:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)initWithArray:(NSMutableArray *)array sequenceType:(enum SequenceType)sequenceType {
    self = [super init];
    if (self) {
        _array = array;
        [self bootstrap];
        _sequenceType = sequenceType;
    }
    return self;
}

- (void)bootstrap {
    _position = 0;
    _length = [_array count];
    _sequenceType = SequenceTypeVector;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"lazy-sequence";
}

- (id<JSDataProtocol>)next {
    if (_position >= _length) [[[JSError alloc] initWithFormat:IndexOutOfBounds, _position, _length] throw];
    return _array[_position++];
}

- (BOOL)hasNext {
    return _position < _length;
}

- (void)addFunction:(JSFunction *)fn {
    [_fns addObject:fn];
}

- (void)apply {
    NSUInteger count = [_fns count];
    JSFunction *fn = nil;
}

- (NSInteger)position {
    return _position;
}

- (NSInteger)sortValue {
    return [_array count];
}

- (BOOL)hasMeta {
    return _meta == nil;
}

- (NSUInteger)hash {
    return [_array hash];
}

- (BOOL)isEqual:(nonnull id)object {
    if (![JSLazySequence isLazySequence:object]) return NO;
    JSLazySequence *seq = (JSLazySequence *)object;
    return [_array isEqual:[seq value]] && _index == [seq index] && _length == [seq length];
}

- (nonnull id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    JSLazySequence *copy = [JSLazySequence new];
    [copy setValue:[_array copy]];
    [copy setPosition:_position];
    [copy setIsMutable:_isMutable];
    [copy setIsImported:_isImported];
    [copy setMeta:_meta];
    [copy setLength:_length];
    [copy setIndex:_index];
    [copy setModuleName:_moduleName];
    return copy;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    JSLazySequence *copy = [JSLazySequence new];
    [copy setValue:[_array mutableCopy]];
    [copy setPosition:_position];
    [copy setIsMutable:_isMutable];
    [copy setIsImported:_isImported];
    [copy setMeta:_meta];
    [copy setLength:_length];
    [copy setIndex:_index];
    [copy setModuleName:_moduleName];
    return copy;
}

@end
