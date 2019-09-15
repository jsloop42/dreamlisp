//
//  DLSlot.m
//  DreamLisp
//
//  Created by jsloop on 01/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLSlot.h"

@implementation DLSlot {
    DLSymbol *_name;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _name;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isSlot:(id)any {
    return [[any className] isEqual:[self className]];
}

- (void)dealloc {
    [DLLog debug:@"DLSlot dealloc"];
}

- (instancetype)init {
    return [super init];
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSlot_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_name forKey:@"DLSlot_value"];
    [coder encodeObject:_meta forKey:@"DLSlot_meta"];
    [coder encodeObject:_moduleName forKey:@"DLSlot_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLSlot_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLSlot_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLSlot_isMutable"];
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
    return @"slot";
}

- (BOOL)hasInitArg {
    return self.initializationArg != nil;
}

- (SEL)selectorForInitArg {
    return [DLTypeUtils convertInitKeywordToSelector:[self.initializationArg string]];
}

- (BOOL)isEqual:(id)object {
    if (![DLSlot isSlot:object]) return NO;
    DLSlot *slot = (DLSlot *)object;
    return slot.value == _name;
}

- (NSUInteger)hash {
    return [_name hash];
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
    DLSlot *slot = [DLSlot new];
    slot.value = _name;
    slot.initializationArg = _initializationArg;
    slot.attribute = _attribute;
    slot.isMutable = _isMutable;
    slot.isImported = _isImported;
    slot.meta = _meta;
    slot.moduleName = _moduleName;
    return slot;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [_name value];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
