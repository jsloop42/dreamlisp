//
//  DLPrinter.m
//  DreamLisp
//
//  Created by Jaseem V V on 05/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLPrinter.h"

@implementation DLPrinter

- (NSString *)readableStringFor:(NSString *)string {
    @autoreleasepool {
        NSString *readable = [[[string stringByReplacingOccurrencesOfString:@"\\" withString:@"\\\\"]
                               stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""]
                               stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
        return [NSString stringWithFormat:@"\"%@\"", readable];
    }
}

- (nullable NSString *)printStringFor:(id<DLDataProtocol>)data readably:(BOOL)readably {
    if (data == nil) { return nil; }
    NSString *dataType = [data dataType];
    if ([DLSymbol isSymbol:data]) {
        return [(DLSymbol *)data string];
    } else if ([DLNumber isNumber:data]) {
        return [(DLNumber *)data string];
    } else if ([dataType isEqual:@"NSNumber"] || [dataType isEqual:@"NSDecimalNumber"]) {
        return [[[DLNumber alloc] initWithNumber:(NSDecimalNumber *)data] string];
    } else if ([DLString isString:data]) {
        NSString *string = [(DLString *)data value];
        return readably ? [self readableStringFor:string] : string;
    } else if ([dataType isEqual:@"NSString"]) {
        NSString *string = (NSString *)data;
        if ([[string substringToIndex:1] isEqual:@"\u029e"]) return [string substringFromIndex:1];
        return readably ? [self readableStringFor:string] : string;
    } else if ([DLList isList:data]) {
        NSMutableArray *xs = [(DLList *)data map:^NSString *(id<DLDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"(%@)", [xs componentsJoinedByString:@" "]];
    } else if ([DLVector isVector:data]) {
        NSMutableArray *xs = [(DLVector *)data map:^NSString *(id<DLDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"NSArray"]) {
        NSMutableArray *xs = [(DLVector *)data map:^NSString *(id<DLDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([DLHashMap isHashMap:data]) {
        NSUInteger i = 0;
        DLHashMap *hm = (DLHashMap *)data;
        NSArray *keys = [hm allKeys];
        NSUInteger len = [keys count];
        NSMutableArray *xs = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            id key = keys[i];
            [xs addObject:[[NSString alloc] initWithFormat:@"%@ %@",
                                [self printStringFor:(id<DLDataProtocol>)key readably:readably],
                                [self printStringFor:(id<DLDataProtocol>)[hm objectForKey:key] readably:readably]]];
        }
        return [[NSString alloc] initWithFormat:@"{%@}", [xs componentsJoinedByString:@" "]];
    } else if ([DLKeyword isKeyword:data]) {
        return [(DLKeyword *)data value];
    } else if ([DLAtom isAtom:data]) {
        return [self printStringFor:[[DLString alloc] initWithFormat:@"(atom %@)", [self printStringFor:[(DLAtom *)data value] readably:false]] readably:false];
    } else if ([DLNil isNil:data]) {
        return @"nil";
    } else if ([DLBool isBool:data]) {
        return [(DLBool *)data value] ? @"true" : @"false";
    } else if ([DLFunction isFunction:data]) {
        DLFunction *fn = (DLFunction *)data;
        return [[fn name] isNotEmpty] ? [fn name] : @"#fn";
    } else if ([DLFault isFault:data]) {
        return [NSString stringWithFormat:@"#<fault %@>", [(DLFault *)data value]];
    } else if ([DLData isData:data]) {
        return [NSString stringWithFormat:@"#<data %@>", [(DLData *)data value]];
    } else if ([DLRegex isRegex:data]) {
        return [NSString stringWithFormat:@"<regex %@>", [(DLRegex *)data value]];
    }
    [[[DLError alloc] initWithFormat:DLInvalidDataType, [(id)data classForCoder], data] throw];
    return nil;
}
@end
