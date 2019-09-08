//
//  DLLazySequence.m
//  DreamLisp
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLLazySequence.h"

@implementation DLLazySequenceFn {
    DLLazyFunction *_lazyFn;
    DLFunction *_fn;
}

@synthesize lazyFn = _lazyFn;
@synthesize fn = _fn;

- (instancetype)initWithLazyFunction:(DLLazyFunction *)lazyFunction {
    return [self initWithLazyFunction:lazyFunction fn:nil];
}

- (instancetype)initWithLazyFunction:(DLLazyFunction *)lazyFunction fn:(DLFunction *)fn {
    self = [super init];
    if (self) {
        _lazyFn = lazyFunction;
        _fn = fn;
    }
    return self;
}

@end

@implementation DLLazySequence {
    NSMutableArray *_array;
    NSMutableArray *_acc;
    NSUInteger _position;
    NSInteger _index;
    NSUInteger _length;
    BOOL _isImported;
    BOOL _isMutable;
    id<DLDataProtocol> _meta;
    NSString *_moduleName;
    enum SequenceType _sequenceType;
    NSMutableArray<DLLazySequenceFn *> *_fns;
    /**
     If the element is a native data structure or if it is wrapped in an @a NSMutableArray, encountered in cases where there are multiple arguments
     to a function.
     */
    BOOL _isNative;
    BOOL _isReverseEnumerate;
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
@synthesize isReverseEnumerate = _isReverseEnumerate;

+ (BOOL)isLazySequence:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLLazySequence *)dataToLazySequence:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToLazySequence:data position:-1 fnName:fnName];
}

+ (DLLazySequence *)dataToLazySequence:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLLazySequence isLazySequence:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'lazy-sequence'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'lazy-sequence'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLLazySequence *)data;
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

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _acc = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_acc"];
        NSValue *indexValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_index"];
        [indexValue getValue:&_index];
        NSValue *lengthValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_length"];
        [lengthValue getValue:&_length];
        NSValue *sequenceTypeValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_sequenceType"];
        [sequenceTypeValue getValue:&_sequenceType];
        _fns = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_fns"];
        NSValue *isNativeValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_isNative"];
        [isNativeValue getValue:&_isNative];
        NSValue *isReverseEnumerateValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLLazySequence_isReverseEnumerate"];
        [isReverseEnumerateValue getValue:&_isReverseEnumerate];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_acc forKey:@"DLLazySequence_acc"];
    [coder encodeObject:@(_index) forKey:@"DLLazySequence_index"];
    [coder encodeObject:@(_length) forKey:@"DLLazySequence_length"];
    NSValue *sequenceTypeValue = [[NSValue alloc] initWithBytes:&_sequenceType objCType:@encode(SequenceType)];
    [coder encodeObject:sequenceTypeValue forKey:@"DLLazySequence_sequenceType"];
    [coder encodeObject:_fns forKey:@"DLLazySequence_fns"];
    NSValue *isNativeValue = [[NSValue alloc] initWithBytes:&_isNative objCType:@encode(BOOL)];
    [coder encodeObject:isNativeValue forKey:@"DLLazySequence_isNative"];
    NSValue *isReverseEnumerateValue = [[NSValue alloc] initWithBytes:&_isReverseEnumerate objCType:@encode(BOOL)];
    [coder encodeObject:isReverseEnumerateValue forKey:@"DLLazySequence_isReverseEnumerate"];
    [coder encodeObject:_meta forKey:@"DLLazySequence_meta"];
    [coder encodeObject:_moduleName forKey:@"DLLazySequence_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLLazySequence_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLLazySequence_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLLazySequence_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
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
    _isReverseEnumerate = NO;
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

- (void)updateEnumerator {
    if (_isNative) {
        _length = [_array count];
    } else {
        _length = [[_array first] count];
    }
    if (_isReverseEnumerate) {
        _index = _length - 1;
    }
}

- (id)next {
    if (_index < 0) [[[DLError alloc] initWithFormat:IndexOutOfBounds, _index, 0] throw];
    if (_index >= _length) [[[DLError alloc] initWithFormat:IndexOutOfBounds, _index, _length] throw];
    if (_isNative) return _array[_isReverseEnumerate ? _index-- : _index++];
    if ([_array count] > 1) {  // not native => array count will be always greater than 1
        NSMutableArray *res = [NSMutableArray new];
        NSMutableArray *arr = nil;
        for (arr in _array) {  // in case of mutiple list arguments
            [res addObject:arr[_index]];
        }
        _isReverseEnumerate ? _index-- : _index++;
        return res;
    }
    return [DLNil new];
}

- (BOOL)hasNext {
    return _isReverseEnumerate ? _index >= 0 : _index < _length;
}

- (void)addLazyFunction:(DLLazyFunction *)lazyFunction {
    return [self addLazyFunction:lazyFunction fn:nil];
}

- (void)addLazyFunction:(DLLazyFunction *)lazyFunction fn:(DLFunction * _Nullable)fn {
    [_fns addObject:[[DLLazySequenceFn alloc] initWithLazyFunction:lazyFunction fn:fn]];
}

- (void)apply {
    DLLazySequenceFn *lfn = nil;
    NSMutableArray *args = [NSMutableArray new];
    if (!_fns) [_acc addObject:[self next]];
    for (lfn in _fns) {
        if ([lfn fn]) {
            if (_isNative) {
                // Calls lazy function block which returns void, with the lazy sequence, user supplied function and params
                @autoreleasepool {
                    [[lfn lazyFn] apply:[@[[lfn fn], [self next]] mutableCopy] forLazySequence:self];
                }
            } else {
                // The user supplied function takes more than one argument
                [args removeAllObjects];
                [args addObject:[lfn fn]];
                [args addObjectsFromArray:[self next]];
                [[lfn lazyFn] apply:args forLazySequence:self];
            }
        } else {
            @autoreleasepool {
                [[lfn lazyFn] apply:[@[[self next]] mutableCopy] forLazySequence:self];  // The lazy function does not take a function as argument
            }
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
    if (![DLLazySequence isLazySequence:object]) return NO;
    DLLazySequence *seq = (DLLazySequence *)object;
    return [_array isEqual:[seq value]] && _index == [seq index] && _length == [seq length];
}

- (nonnull id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSString *)description {
    return [_array description];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLLazySequence *copy = [DLLazySequence new];
    [copy setValue:[_array copy]];
    [copy setPosition:_position];
    [copy setIsMutable:_isMutable];
    [copy setIsImported:_isImported];
    [copy setMeta:_meta];
    [copy setLength:_length];
    [copy setIndex:_index];
    [copy setModuleName:_moduleName];
    [copy setIsNative:_isNative];
    [copy setIsReverseEnumerate:_isReverseEnumerate];
    return copy;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    DLLazySequence *copy = [DLLazySequence new];
    [copy setValue:[_array mutableCopy]];
    [copy setPosition:_position];
    [copy setIsMutable:_isMutable];
    [copy setIsImported:_isImported];
    [copy setMeta:_meta];
    [copy setLength:_length];
    [copy setIndex:_index];
    [copy setModuleName:_moduleName];
    [copy setIsNative:_isNative];
    [copy setIsReverseEnumerate:_isReverseEnumerate];
    return copy;
}

@end
