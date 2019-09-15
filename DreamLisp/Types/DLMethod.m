//
//  DLMethod.m
//  DreamLisp
//
//  Created by jsloop on 13/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLMethod.h"

/*! Represents method parameter in a `defmethod` form as well as in method invocation expression. Accordingly only the relevent parts will be set. */
@implementation DLMethodParam {
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isMethodParam:(id)any {
    return [[any className] isEqual:[self className]];
}

- (void)dealloc {
    [DLLog debug:@"DLMethodParam dealloc"];
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _selectorName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_selectorName"];
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_name"];
        _attr = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_attr"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethodParam_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_selectorName forKey:@"DLMethodParam_selectorName"];
    [coder encodeObject:_name forKey:@"DLMethodParam_name"];
    [coder encodeObject:_attr forKey:@"DLMethodParam_attr"];
    [coder encodeObject:_meta forKey:@"DLMethodParam_meta"];
    [coder encodeObject:_moduleName forKey:@"DLMethodParam_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLMethodParam_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLMethodParam_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLMethodParam_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"object";
}

/*! Returns the method param as a string, which can be either the param name or the parameterized name part. */
- (NSString *)string {
    if (_name) {  /* => arg name symbol */
        return [_name value];
    }
    if (_selectorName) {  /* => parameterized part of the arg */
        return [_selectorName string];
    }
    return @"";
}

- (BOOL)isEqual:(id)object {
    if (![DLMethodParam isMethodParam:object]) return NO;
    DLMethodParam *obj = (DLMethodParam *)object;
    return [obj.value isEqual:_selectorName];
}

- (NSUInteger)hash {
    return [_selectorName hash];
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

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLMethodParam *obj = [DLMethodParam new];
    obj.selectorName = _selectorName;
    obj.name = _name;
    obj.attr = _attr;
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
    return [_selectorName description];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end

@implementation DLMethod {
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
    /*! Indicates whether the method represent a user defined method or an existing method (eg: Foundation methods). If it's native, there won't be ast. */
    BOOL _isNative;
}

@synthesize isNative = _isNative;
@synthesize meta = _meta;
@synthesize value;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isMethod:(id)any {
    return [[any className] isEqual:[self className]];
}

- (void)dealloc {
    [DLLog debug:@"DLMethod dealloc"];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_name"];
        _params = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_params"];
        _ast = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_value"];
//        _fn = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_fn"];
        _env = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_env"];
        NSValue *isNativeValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_isNative"];
        [isNativeValue getValue:&_isNative];
        _attr = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_attr"];
        _cls = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_cls"];
        _selectorString = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_selectorString"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLMethod_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_name forKey:@"DLMethod_name"];
    [coder encodeObject:_params forKey:@"DLMethod_params"];
    [coder encodeObject:_ast forKey:@"DLMethod_value"];
//    [coder encodeObject:_fn forKey:@"DLMethod_fn"];
    [coder encodeObject:_env forKey:@"DLMethod_env"];
    NSValue *isNativeValue = [[NSValue alloc] initWithBytes:&_isNative objCType:@encode(BOOL)];
    [coder encodeObject:isNativeValue forKey:@"DLMethod_isNative"];
    [coder encodeObject:_attr forKey:@"DLMethod_attr"];
    [coder encodeObject:_cls forKey:@"DLMethod_cls"];
    [coder encodeObject:_selectorString forKey:@"DLMethod_selectorString"];
    [coder encodeObject:_meta forKey:@"DLMethod_meta"];
    [coder encodeObject:_moduleName forKey:@"DLMethod_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLMethod_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLMethod_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLMethod_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _isNative = YES;
    _type = [[[NSString alloc] initWithFormat:@"%s%s%s%s%s%s", @encode(id), @encode(id), @encode(SEL), @encode(id), @encode(id), @encode(id)] UTF8String];
    _params = [NSMutableArray new];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"object";
}

- (void)setIsNative:(BOOL)isNative {
    _isNative = isNative;
    if (!isNative) {
        _type = "";
    }
}

/*! Parameter name binding for the env. */
- (NSMutableArray *)binds {
    DLMethodParam *param;
    NSMutableArray *bind = [NSMutableArray new];
    for (param in _params) {
        if (param.name) {
            [bind addObject:param.name];
        }
    }
    return bind;
}

/*! Returns the method arguments which can be passed directly as invocation argument */
- (NSMutableArray *)args {
    DLMethodParam *param;
    NSMutableArray *args = [NSMutableArray new];
    for (param in _params) {
        if (param.value) {
            [args addObject:param.value];
        }
    }
    return args;
}

- (BOOL)isEqual:(id)object {
    if (![DLMethod isMethod:object]) return NO;
    DLMethod *obj = (DLMethod *)object;
    return [obj.value isEqual:_ast];
}

- (NSUInteger)hash {
    return [_ast hash];
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

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLMethod *obj = [DLMethod new];
    obj.name = _name;
    obj.params = _params;
    obj.ast = _ast;
//    obj.fn = _fn;
    obj.env = _env;
    obj.attr = _attr;
    obj.cls = _cls;
    obj.selectorString = _selectorString;
    obj.selector = _selector;
    obj.signature = _signature;
    obj.imp = _imp;
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
    return [_selectorString value];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
