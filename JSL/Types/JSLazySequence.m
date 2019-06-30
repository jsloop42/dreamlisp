//
//  JSLazySequence.m
//  JSL
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSLazySequence.h"

@implementation JSLazySequenceFn {
    JSLazyFunction *_lazyFn;
    JSFunction *_fn;
}

@synthesize lazyFn = _lazyFn;
@synthesize fn = _fn;

- (instancetype)initWithLazyFunction:(JSLazyFunction *)lazyFunction {
    return [self initWithLazyFunction:lazyFunction fn:nil];
}

- (instancetype)initWithLazyFunction:(JSLazyFunction *)lazyFunction fn:(JSFunction *)fn {
    self = [super init];
    if (self) {
        _lazyFn = lazyFunction;
        _fn = fn;
    }
    return self;
}

@end

@implementation JSLazySequence {
    NSMutableArray *_array;
    NSMutableArray *_acc;
    NSUInteger _position;
    NSUInteger _index;
    NSUInteger _length;
    BOOL _isImported;
    BOOL _isMutable;
    id<JSDataProtocol> _meta;
    NSString *_moduleName;
    enum SequenceType _sequenceType;
    NSMutableArray<JSLazySequenceFn *> *_fns;
    BOOL _isNative;
}

@synthesize acc = _acc;
@synthesize isImported = _isImported;
@synthesize isMutable = _isMutable;
@synthesize meta = _meta;
@synthesize moduleName = _moduleName;
@synthesize value = _array;
@synthesize index = _index;
@synthesize length = _length;
@synthesize sequenceType = _sequenceType;
@synthesize fns = _fns;
@synthesize isNative = _isNative;

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
        [self bootstrap];
        _array = array;
        _length = [_array count];
        _sequenceType = sequenceType;
    }
    return self;
}

- (void)bootstrap {
    _array = [NSMutableArray new];
    _acc = [NSMutableArray new];
    _position = 0;
    _index = 0;
    _length = 0;
    _sequenceType = SequenceTypeVector;
    _fns = [NSMutableArray new];
    _isNative = YES;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"lazy-sequence";
}

- (NSMutableArray *)value {
    return _array;
}

- (void)setValue:(id)value {
    _array = value;
    _length = [_array count];
}

- (void)updateLength {
    if (_isNative) {
        _length = [_array count];
    } else {
        _length = [[_array first] count];
    }
}

- (id)next {
    if (_index >= _length) [[[JSError alloc] initWithFormat:IndexOutOfBounds, _index, _length] throw];
    if (_isNative) return _array[_index++];
    if ([_array count] > 1) {
        NSMutableArray *res = [NSMutableArray new];
        NSMutableArray *arr = nil;
        for (arr in _array) {
            [res addObject:arr[_index]];
        }
        _index++;
        return res;
    }
    return [JSNil new];
}

- (BOOL)hasNext {
    return _index < _length;
}

- (void)addLazyFunction:(JSLazyFunction *)lazyFunction {
    return [self addLazyFunction:lazyFunction fn:nil];
}

- (void)addLazyFunction:(JSLazyFunction *)lazyFunction fn:(JSFunction * _Nullable)fn {
    [_fns addObject:[[JSLazySequenceFn alloc] initWithLazyFunction:lazyFunction fn:fn]];
}

- (void)apply {
    JSLazySequenceFn *lfn = nil;
    if (!_fns) [_acc addObject:[self next]];
    for (lfn in _fns) {
        if ([lfn fn]) {
            if (_isNative) {
                // Calls lazy function block which returns void, with the lazy sequence, user supplied function and params
                [[lfn lazyFn] apply:[@[[lfn fn], [self next]] mutableCopy] forLazySequence:self];
            } else {
                // The user supplied function takes more than one argument
                NSMutableArray *args = [NSMutableArray new];
                [args addObject:[lfn fn]];
                [args addObjectsFromArray:[self next]];
                [[lfn lazyFn] apply:args forLazySequence:self];
            }
        } else {
            [[lfn lazyFn] apply:[@[[self next]] mutableCopy] forLazySequence:self];  // The lazy function does not take a function as argument
        }
    }
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

- (NSString *)description {
    return [_array description];
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

