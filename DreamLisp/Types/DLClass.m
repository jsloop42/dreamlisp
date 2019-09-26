//
//  DLClass.m
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLClass.h"

@implementation DLClass {
    Class _proxy;
    __unsafe_unretained id _returnValue;
    NSString *_proxyAssocKey;
    NSString *_returnAssocKey;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize proxy = _proxy;
@synthesize returnValue = _returnValue;
@synthesize proxyAssocKey = _proxyAssocKey;
@synthesize returnAssocKey = _returnAssocKey;
@synthesize meta = _meta;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isClass:(id)any {
    return [[any className] isEqual:[self className]];
}

+ (DLClass *)dataToClass:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToClass:data position:-1 fnName:fnName];
}

+ (DLClass *)dataToClass:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLClass isClass:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'class'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'class'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLClass *)data;
}

+ (NSString *)className {
    return NSStringFromClass([self class]);
}

+ (instancetype)new {
    return [[DLClass alloc] init];
}

- (void)dealloc {
    [DLLog debug:@"DLClass dealloc"];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (nonnull instancetype)initWithProxy:(nonnull id)object {
    if (self) {
        [self bootstrap];
        _proxy = object;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _proxy = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_proxy"];
        _returnValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_returnValue"];
        _proxyAssocKey = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_proxyAssocKey"];
        _returnAssocKey = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_returnAssocKey"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_proxy forKey:@"DLClass_proxy"];
    [coder encodeObject:_returnValue forKey:@"DLClass_returnValue"];
    [coder encodeObject:_proxyAssocKey forKey:@"DLClass_proxyAssocKey"];
    [coder encodeObject:_returnAssocKey forKey:@"DLClass_returnAssocKey"];
    [coder encodeObject:_meta forKey:@"DLClass_meta"];
    [coder encodeObject:_moduleName forKey:@"DLClass_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLClass_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLClass_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLClass_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _slots = [NSMutableArray new];
    _conformance = [NSMutableArray new];
    _methods = [NSMutableSet new];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"class";
}

- (id)value {
    return _proxy;
}

- (void)setValue:(id)value {
    [_proxy doesNotRecognizeSelector:_cmd];
}

- (BOOL)containsSlotWithInitArg:(DLKeyword *)keyword {
    DLSlot *slot = nil;
    for (slot in self.slots) {
        if ([slot.initializationArg isEqual:keyword]) return YES;
    }
    return NO;
}

- (DLSlot  * _Nullable)slotWithInitArg:(DLKeyword *)keyword {
    DLSlot *slot = nil;
    for (slot in self.slots) {
        if ([slot.initializationArg isEqual:keyword]) return slot;
    }
    return nil;
}

- (BOOL)isEqual:(id)object {
    if (![DLClass isClass:object]) return NO;
    DLClass *cls = (DLClass *)object;
    return [cls.value isEqual:_proxy];
}

- (NSUInteger)hash {
    return [_proxy hash];
}

- (BOOL)hasMeta {
    return NO;
}

- (NSInteger)position {
    return _position;
}

- (nonnull id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (void)forwardInvocation:(NSInvocation *)invocation {
    // TODO: Convert DL types to NS types. When creating an Objective-C class, method, even if we are giving DL type, they are converted into NS types as arg.
    if ([_proxy respondsToSelector:invocation.selector]) {
        [invocation invokeWithTarget:_proxy];
    } else {
        @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, _proxy, invocation.selector];
    }
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)sel {
    if ([_proxy respondsToSelector:sel]) return [_proxy methodSignatureForSelector:sel];
    @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, _proxy, sel];
}

- (nonnull NSString *)className {
    return NSStringFromClass([self class]);
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLClass *cls = [DLClass new];
    cls.proxy = _proxy;
    cls.returnValue = _returnValue;
    cls.proxyAssocKey = _proxyAssocKey;
    cls.returnAssocKey = _returnAssocKey;
    cls.isMutable = _isMutable;
    cls.isImported = _isImported;
    cls.meta = _meta;
    cls.moduleName = _moduleName;
    return cls;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return NSStringFromClass(_proxy);
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
