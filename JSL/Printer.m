//
//  Printer.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Printer.h"

@implementation Printer

- (NSString *)readableStringFor:(NSString *)string {
    NSString *readable = [[[string stringByReplacingOccurrencesOfString:@"\\" withString:@"\\\\"]
                           stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""]
                           stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
    return [NSString stringWithFormat:@"\"%@\"", readable];
}

- (nullable NSString *)printStringFor:(id<JSDataProtocol>)data readably:(BOOL)readably {
    if (data == nil) { return nil; }
    NSString *dataType = [data dataType];
    if ([JSSymbol isSymbol:data]) {
        return [(JSSymbol *)data string];
    } else if ([JSNumber isNumber:data]) {
        return [(JSNumber *)data string];
    } else if ([dataType isEqual:@"NSNumber"] || [dataType isEqual:@"NSDecimalNumber"]) {
        return [[[JSNumber alloc] initWithNumber:(NSDecimalNumber *)data] string];
    } else if ([JSString isString:data]) {
        NSString *string = [(JSString *)data value];
        return readably ? [self readableStringFor:string] : string;
    } else if ([dataType isEqual:@"NSString"]) {
        NSString *string = (NSString *)data;
        if ([[string substringToIndex:1] isEqual:@"\u029e"]) return [string substringFromIndex:1];
        return readably ? [self readableStringFor:string] : string;
    } else if ([JSList isList:data]) {
        NSMutableArray *xs = [(JSList *)data map:^NSString *(id<JSDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"(%@)", [xs componentsJoinedByString:@" "]];
    } else if ([JSVector isVector:data]) {
        NSMutableArray *xs = [(JSVector *)data map:^NSString *(id<JSDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"NSArray"]) {
        NSMutableArray *xs = [(JSVector *)data map:^NSString *(id<JSDataProtocol> obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([JSHashMap isHashMap:data]) {
        NSUInteger i = 0;
        JSHashMap *hm = (JSHashMap *)data;
        NSArray *keys = [hm allKeys];
        NSUInteger len = [keys count];
        NSMutableArray *xs = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            id key = keys[i];
            [xs addObject:[[NSString alloc] initWithFormat:@"%@ %@",
                                [self printStringFor:(id<JSDataProtocol>)key readably:readably],
                                [self printStringFor:(id<JSDataProtocol>)[hm objectForKey:key] readably:readably]]];
        }
        return [[NSString alloc] initWithFormat:@"{%@}", [xs componentsJoinedByString:@" "]];
    } else if ([JSKeyword isKeyword:data]) {
        return [(JSKeyword *)data value];
    } else if ([JSAtom isAtom:data]) {
        return [self printStringFor:[[JSString alloc] initWithFormat:@"(atom %@)", [self printStringFor:[(JSAtom *)data value] readably:false]] readably:false];
    } else if ([JSNil isNil:data]) {
        return @"nil";
    } else if ([JSBool isBool:data]) {
        return [(JSBool *)data value] ? @"true" : @"false";
    } else if ([JSFunction isFunction:data]) {
        JSFunction *fn = (JSFunction *)data;
        return [[fn name] isNotEmpty] ? [fn name] : @"#fn";
    } else if ([JSFault isFault:data]) {
        return [NSString stringWithFormat:@"#<fault %@>", (JSFault *)[data value]];
    } else if ([JSLazySequence isLazySequence:data]) {
        return [NSString stringWithFormat:@"#<lazy-seq %@>", [(JSLazySequence *)data description]];
    }
    [[[JSError alloc] initWithFormat:InvalidDataType, [(id)data classForCoder], data] throw];
    return nil;
}
@end
