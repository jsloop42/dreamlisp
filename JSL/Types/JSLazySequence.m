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
    NSMutableArray *_fns;
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

+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToLazySequence:data position:-1 fnName:fnName];
}

+ (JSLazySequence *)dataToLazySequence:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSLazySequence isLazySequence:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'lazy-sequence'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'lazy-sequence'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSLazySequence *)data;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
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
    _fns = [NSMutableArray new];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"lazy-sequence";
}

- (void)setValue:(id)value {
    _array = value;
    _length = [_array count];
}

- (id<JSDataProtocol>)next {
    if (_position >= _length) [[[JSError alloc] initWithFormat:IndexOutOfBounds, _position, _length] throw];
    return _array[_position++];
}

- (BOOL)hasNext {
    return _position < _length;
}

- (void)addFunction:(id<JSDataProtocol> (^)(NSMutableArray *))fn {
    [_fns addObject:fn];
}

- (id<JSDataProtocol>)apply {
    id<JSDataProtocol> (^fn)(NSMutableArray *) = nil;
    id<JSDataProtocol> res = nil;
    if (!_fns) return [self next];
    for (fn in _fns) {
        if (_sequenceType == SequenceTypeList) {
            res = fn([@[[[JSList alloc] initWithArray:[@[[self next]] mutableCopy]]] mutableCopy]);
        } else if (_sequenceType == SequenceTypeVector || _sequenceType == SequenceTypeString) {
            res = fn([@[[[JSVector alloc] initWithArray:[@[[self next]] mutableCopy]]] mutableCopy]);
        }
    }
    return res ? res : [JSNil new];
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
