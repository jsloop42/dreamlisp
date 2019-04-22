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

- (NSString *)printStringFor:(JSData *)data readably:(BOOL)readably {
    NSString *dataType = [data dataType];
    if ([dataType isEqual:@"JSSymbol"]) {
        return [(JSSymbol *)data name];
    } else if ([dataType isEqual:@"JSNumber"]) {
        return [(JSNumber *)data string];
    } else if ([dataType isEqual:@"NSNumber"] || [dataType isEqual:@"NSDecimalNumber"]) {
        return [[[JSNumber alloc] initWithNumber:(NSDecimalNumber *)data] string];
    } else if ([dataType isEqual:@"JSString"]) {
        NSString *string = [(JSString *)data value];
        if (readably) {
            return [self readableStringFor:string];
        }
        return string;
    } else if ([dataType isEqual:@"NSString"]) {
        NSString *string = (NSString *)data;
        if ([[string substringToIndex:1] isEqual:@"\u029e"]) {
            return [string substringFromIndex:1];
        }
        if (readably) {
            return [self readableStringFor:string];
        }
        return string;
    } else if ([dataType isEqual:@"JSList"]) {
        NSMutableArray *xs = [(JSList *)data map:^NSString *(JSData *obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"(%@)", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"JSVector"]) {
        NSMutableArray *xs = [(JSVector *)data map:^NSString *(JSData *obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"NSArray"]) {
        NSMutableArray *xs = [(JSVector *)data map:^NSString *(JSData *obj) {
            return [self printStringFor:obj readably:readably];
        }];
        return [[NSString alloc] initWithFormat:@"[%@]", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"JSHashMap"]) {
        NSUInteger i = 0;
        JSHashMap *hm = (JSHashMap *)data;
        NSArray *keys = [hm allKeys];
        NSUInteger len = [keys count];
        NSMutableArray *xs = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            NSString *key = keys[i];
            JSData *keyData = nil;
            if ([JSKeyword isKeyword:key]) {
                keyData = [[JSKeyword alloc] initWithEncodedKeyword:key];
            } else {
                keyData = [[JSString alloc] initWithString:key];
            }
            [xs addObject:[[NSString alloc] initWithFormat:@"%@ %@",
                                [self printStringFor:keyData readably:readably],
                                [self printStringFor:(JSData *)[hm objectForKey:key] readably:readably]]];
        }
        return [[NSString alloc] initWithFormat:@"{%@}", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"JSKeyword"]) {
        return [(JSKeyword *)data value];
    } else if ([dataType isEqual:@"JSAtom"]) {
        return [self printStringFor:[[JSString alloc] initWithFormat:@"(atom %@)", [self printStringFor:[(JSAtom *)data value] readably:false]] readably:false];
    } else if ([dataType isEqual:@"JSNil"]) {
        return @"nil";
    } else if ([dataType isEqual:@"JSBool"]) {
        if ([(JSBool *)data value]) {
            return @"true";
        }
        return @"false";
    } else if ([dataType isEqual:@"JSFunction"]) {
        return @"#<function>";
    }
    return JSL_ERROR_TYPE_MSG;
}
@end
