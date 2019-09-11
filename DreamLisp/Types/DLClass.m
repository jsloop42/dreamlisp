//
//  DLClass.m
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLClass.h"

@implementation DLClass {
    Class _cls;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _cls;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isClass:(id)any {
    return [[any className] isEqual:[self className]];
}

- (void)dealloc {
    [DLLog debug:[NSString stringWithFormat:@"%@ dealloc", [self className]]];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithClass:(Class)cls {
    self = [super init];
    if (self) {
        [self bootstrap];
        _cls = cls;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _cls = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLClass_value"];
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
    [coder encodeObject:_cls forKey:@"DLClass_value"];
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
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"class";
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
    return [cls.value isEqual:_cls];
}

- (NSUInteger)hash {
    return [_cls hash];
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
    DLClass *cls = [DLClass new];
    cls.value = _cls;
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
    return NSStringFromClass(_cls);
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
