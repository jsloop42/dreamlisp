//
//  DLObject.m
//  DreamLisp
//
//  Created by Jaseem V V on 01/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLObject.h"

@implementation DLObject {
    id _proxy;
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

+ (BOOL)isObject:(id)any {
    return [[any className] isEqual:[self className]];
}

+ (NSString *)className {
    return NSStringFromClass([self class]);
}

+ (instancetype)new {
    return [[DLObject alloc] init];
}

+ (DLObject *)dataToObject:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToObject:data position:-1 fnName:fnName];
}

+ (DLObject *)dataToObject:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLObject isObject:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'object'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'object'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLObject *)data;
}

- (void)dealloc {
    [DLLog debug:@"DLObject dealloc"];
}

- (instancetype)init {
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithProxy:(id)object {
    if (self) {
        //[self bootstrap];
        _proxy = object;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    if (self) {
        _proxy = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_proxy"];
        _cls = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_cls"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_proxy forKey:@"DLObject_proxy"];
    [coder encodeObject:_cls forKey:@"DLObject_cls"];
    [coder encodeObject:_meta forKey:@"DLObject_meta"];
    [coder encodeObject:_moduleName forKey:@"DLObject_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLObject_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLObject_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLObject_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
}

- (NSString *)className {
    return NSStringFromClass([self class]);
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"object";
}

- (id)value {
    return _proxy;
}

- (void)setValue:(id)value {
    [_proxy doesNotRecognizeSelector:_cmd];
}

- (BOOL)isEqual:(id)object {
    if (![DLObject isObject:object]) return NO;
    DLObject *obj = (DLObject *)object;
    return [obj.value isEqual:_proxy];
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
    [invocation invokeWithTarget:_proxy];
//    if (_proxy && [_proxy respondsToSelector:invocation.selector]) {
//        [invocation invokeWithTarget:_proxy];
//    } else {
//        @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, _proxy, invocation.selector];
//    }
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)sel {
    return [_proxy methodSignatureForSelector:sel];
//    if (_proxy && [_proxy respondsToSelector:sel]) return [_proxy methodSignatureForSelector:sel];
//    @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, _proxy, sel];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLObject *obj = [DLObject new];
    obj.proxy = _proxy;
    obj.proxyAssocKey = _proxyAssocKey;
    obj.returnAssocKey = _returnAssocKey;
    obj.returnValue = _returnValue;
    obj.cls = _cls;
    obj.isMutable = _isMutable;
    obj.isImported = _isImported;
    obj.meta = _meta;
    obj.moduleName = _moduleName;
    return obj;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [_proxy description];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
